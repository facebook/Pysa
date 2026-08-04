(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open Ast
open Statement
open Pyre
module AstResult = PyreflyApi.AstResult

let class_method_decorators = ["classmethod"; "abstractclassmethod"; "abc.abstractclassmethod"]

let static_method_decorators = ["staticmethod"; "abstractstaticmethod"; "abc.abstractstaticmethod"]

module CallableSignature = PyreflyTypes.CallableSignature

let get_signature_and_definition ~pyrefly_api callable =
  let callable_id = Target.undecorated_callable_id_exn callable in
  PyreflyApi.ReadOnly.get_callable_signature_opt pyrefly_api callable_id
  >>| fun signature ->
  let define = PyreflyApi.ReadOnly.get_define_opt pyrefly_api callable_id in
  signature, define


let get_signature_and_definition_for_test = get_signature_and_definition

module DefineAndModule = struct
  type t = {
    module_id: PyreflyTypes.ModuleId.t;
    define: Define.t Node.t;
  }
end

module DefinesSharedMemory =
  Hack_parallel.Std.SharedMemory.FirstClassWithKeys.Make
    (Target.SharedMemoryKey)
    (struct
      type t = DefineAndModule.t AstResult.t

      let prefix = Hack_parallel.Std.Prefix.make ()

      let description = "callable defines"
    end)

module SignaturesSharedMemory =
  Hack_parallel.Std.SharedMemory.FirstClass.WithCache.Make
    (Target.SharedMemoryKey)
    (struct
      type t = CallableSignature.t

      let prefix = Hack_parallel.Std.Prefix.make ()

      let description = "callable signatures"
    end)

module ReadWrite = struct
  type t = {
    defines: DefinesSharedMemory.t;
    signatures: SignaturesSharedMemory.t;
    (* Read from the pyrefly API first, and fall back to shared memory if the callable does not
       exist (for artificial defines such as decorated targets). *)
    pyrefly_api: PyreflyApi.ReadOnly.t;
  }

  let cleanup { defines; signatures; _ } =
    let keys = SignaturesSharedMemory.KeySet.of_list (DefinesSharedMemory.keys defines) in
    let () = DefinesSharedMemory.cleanup ~clean_old:true defines in
    let () = SignaturesSharedMemory.remove_old_batch signatures keys in
    let () = SignaturesSharedMemory.remove_batch signatures keys in
    ()


  (* Create a [CallablesSharedMemory] that includes all available callables from the given API.
     Lookups are served directly by the pyrefly API, where signatures are pre-computed when parsing
     sources, so no callables need to be materialized into shared memory here. *)
  let from_pyrefly_api ~pyrefly_api =
    {
      defines = DefinesSharedMemory.create ();
      signatures = SignaturesSharedMemory.create ();
      pyrefly_api;
    }


  let add_alist_sequential ({ signatures; _ } as handle) entries =
    let () =
      List.iter entries ~f:(fun (target, signature, _) ->
          SignaturesSharedMemory.add signatures target signature)
    in
    let defines =
      entries
      |> List.map ~f:(fun (target, { CallableSignature.module_id; _ }, define) ->
             target, AstResult.Some { DefineAndModule.module_id; define })
      |> DefinesSharedMemory.add_alist_sequential handle.defines
    in
    { handle with defines }
end

module ReadOnly = struct
  type t = {
    defines: DefinesSharedMemory.ReadOnly.t;
    signatures: SignaturesSharedMemory.t;
    pyrefly_api: PyreflyApi.ReadOnly.t;
  }

  let read_only { ReadWrite.defines; signatures; pyrefly_api } =
    { defines = DefinesSharedMemory.read_only defines; signatures; pyrefly_api }


  let option_to_ast_result = function
    | Some ast_result -> ast_result
    | None -> AstResult.Pyre1NotFound


  let is_supported_by_pyrefly target =
    (not (Target.is_decorated target))
    && (not (Target.is_override target))
    && not (Target.is_object target)


  let get_define_from_pyrefly ~pyrefly_api target =
    if not (is_supported_by_pyrefly target) then
      None
    else
      Target.callable_id target
      >>| fun callable_id ->
      PyreflyApi.ReadOnly.get_define_opt pyrefly_api callable_id
      |> AstResult.map ~f:(fun define ->
             { DefineAndModule.define; module_id = PyreflyTypes.CallableId.module_id callable_id })


  let get_define_from_shared_memory ~defines target =
    DefinesSharedMemory.ReadOnly.get ~cache:true defines target |> option_to_ast_result


  let get_define { defines; pyrefly_api; _ } target =
    match get_define_from_pyrefly ~pyrefly_api target with
    | Some result -> result
    | None -> get_define_from_shared_memory ~defines target


  let get_signature_from_pyrefly ~pyrefly_api target =
    if not (is_supported_by_pyrefly target) then
      None
    else
      Target.callable_id target >>= PyreflyApi.ReadOnly.get_callable_signature_opt pyrefly_api


  let get_signature_from_shared_memory ~signatures target =
    SignaturesSharedMemory.get signatures target


  let get_signature { signatures; pyrefly_api; _ } target =
    match get_signature_from_pyrefly ~pyrefly_api target with
    | Some _ as result -> result
    | None -> get_signature_from_shared_memory ~signatures target


  let get_location ({ pyrefly_api; _ } as handle) target =
    target
    |> Target.strip_parameters
    |> get_signature handle
    >>| (fun { CallableSignature.module_id; location; _ } ->
          let qualifier = PyreflyApi.ReadOnly.module_qualifier_of_id pyrefly_api module_id in
          AstResult.map location ~f:(Location.with_module ~module_reference:qualifier))
    |> option_to_ast_result


  let get_location_opt handle target = get_location handle target |> AstResult.to_option

  let get_module handle target =
    get_signature handle target >>| fun { CallableSignature.module_id; _ } -> module_id


  let get_method_kind_from_pyrefly ~pyrefly_api target =
    if not (is_supported_by_pyrefly target) then
      None
    else
      Target.callable_id target
      >>| PyreflyApi.ReadOnly.Target.get_callable_metadata pyrefly_api
      >>| PyreflyApi.CallableMetadata.get_method_kind


  let get_method_kind_from_shared_memory ~signatures method_target =
    method_target
    |> Target.from_regular
    |> SignaturesSharedMemory.get signatures
    >>= fun { CallableSignature.method_kind; _ } -> method_kind


  (* Return `is_class_method` and `is_static_method`. *)
  let get_method_kind { signatures; pyrefly_api; _ } target =
    (* For `Override`, we just check its corresponding method. *)
    let method_target = target |> Target.get_regular |> Target.Regular.override_to_method in
    let method_kind =
      let method_target_as_target = method_target |> Target.from_regular in
      match get_method_kind_from_pyrefly ~pyrefly_api method_target_as_target with
      | Some result -> result
      | None -> get_method_kind_from_shared_memory ~signatures method_target
    in
    let method_target_name =
      PyreflyApi.ReadOnly.Target.method_name pyrefly_api (Target.from_regular method_target)
    in
    match method_kind, method_target_name with
    | _, Some "__new__" -> false, true
    | Some Class, _ -> true, false
    | Some Static, _ -> false, true
    | Some Instance, _ -> false, false
    | None, _ -> false, false


  let is_stub_like_from_pyrefly ~pyrefly_api target =
    if not (is_supported_by_pyrefly target) then
      None
    else
      Target.callable_id target >>= PyreflyApi.ReadOnly.is_stub_like_callable_opt pyrefly_api


  let is_stub_like_from_shared_memory ~signatures target =
    SignaturesSharedMemory.get signatures target
    >>| fun { CallableSignature.is_stub_like; _ } -> is_stub_like


  let is_stub_like { signatures; pyrefly_api; _ } target =
    match is_stub_like_from_pyrefly ~pyrefly_api target with
    | Some _ as result -> result
    | None -> is_stub_like_from_shared_memory ~signatures target


  let get_captures_from_pyrefly ~pyrefly_api target =
    if not (is_supported_by_pyrefly target) then
      None
    else
      Target.callable_id target >>= PyreflyApi.ReadOnly.get_callable_captures_opt pyrefly_api


  let get_captures_from_shared_memory ~signatures target =
    SignaturesSharedMemory.get signatures target
    >>| fun { CallableSignature.captures; _ } -> captures


  let get_captures { signatures; pyrefly_api; _ } target =
    match get_captures_from_pyrefly ~pyrefly_api target with
    | Some _ as result -> result
    | None -> get_captures_from_shared_memory ~signatures target


  let callable_from_reference { pyrefly_api; _ } name =
    (* Note: we can only create callable ids for names that are known to pyrefly. *)
    PyreflyApi.ReadOnly.Target.callable_id_from_name pyrefly_api name
    >>| fun callable_id ->
    PyreflyApi.ReadOnly.Target.target_from_callable_id pyrefly_api ~override:false callable_id


  let mem_from_pyrefly ~pyrefly_api target =
    is_supported_by_pyrefly target
    && Option.is_some
         (PyreflyApi.ReadOnly.Target.get_callable_metadata_opt
            pyrefly_api
            (Target.callable_id_exn target))


  let mem_from_shared_memory ~signatures target = SignaturesSharedMemory.mem signatures target

  let mem { signatures; pyrefly_api; _ } target =
    mem_from_pyrefly ~pyrefly_api target || mem_from_shared_memory ~signatures target
end
