(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open Ast
open Statement
open Expression
module PyrePysaLogic = Analysis.PyrePysaLogic
module AstResult = PyreflyApi.AstResult

module Heap = struct
  type t = StringLiteral.t Reference.Map.t [@@deriving show, equal]

  let of_alist_exn alist =
    alist
    |> List.map ~f:(fun (key, value) -> key, StringLiteral.create value)
    |> Reference.Map.of_alist_exn


  let empty = Reference.Map.empty

  let from_module ~pyrefly_api ~callables_to_definitions_map module_id =
    let top_level_callable_id =
      PyreflyTypes.CallableId.encode ~module_id PyreflyTypes.LocalFunctionId.ModuleTopLevel
    in
    let qualifier = PyreflyApi.ReadOnly.module_qualifier_of_id pyrefly_api module_id in
    let extract_string = function
      (* __module__ affects name resolution, due to __module__ specifying the module something was
         defined in, so a solution is just to skip __module__ assignments *)
      | {
          Node.value =
            {
              Assign.target =
                { Node.value = Expression.Name (Attribute { attribute = "__module__"; _ }); _ };
              _;
            };
          _;
        } ->
          None
      | {
          Node.value =
            {
              Assign.target = { Node.value = Expression.Name (_ as name); _ };
              Assign.value =
                Some { Node.value = Expression.Constant (Constant.String (_ as value)); _ };
              _;
            };
          _;
        }
        when Option.is_some (Ast.Expression.name_to_reference name) ->
          (* TODO(T225700656): Migrate for pyrefly *)
          let as_local = Ast.Expression.name_to_reference_exn name in
          let delocalized = Ast.Reference.delocalize as_local in
          if Ast.Reference.is_prefix ~prefix:qualifier delocalized then
            Some
              (delocalized, { value with Ast.Expression.StringLiteral.qualified_expression = None })
          else (* Should not collect global variables imported from a different module. *)
            None
      | _ -> None
    in
    let open Option.Monad_infix in
    top_level_callable_id
    |> PyreflyApi.ReadOnly.Target.target_from_callable_id pyrefly_api ~override:false
    |> CallablesSharedMemory.ReadOnly.get_define callables_to_definitions_map
    |> AstResult.to_option
    >>| (fun { CallablesSharedMemory.DefineAndModule.define; _ } -> define)
    >>| Ast.Node.value
    >>| (fun { Ast.Statement.Define.body; _ } -> body)
    |> Option.value ~default:[]
    |> Source.create_from_module_path
         ~typecheck_flags:(Source.TypecheckFlags.create_for_testing ())
         ~module_path:{ ModulePath.raw = ModulePath.Raw.empty; qualifier; should_type_check = true }
    |> PyreflyApi.ReadOnly.ensures_qualified pyrefly_api
    |> Preprocessing.toplevel_assigns
    |> List.concat_map ~f:Preprocessing.toplevel_expand_tuple_assign
    |> List.filter_map ~f:extract_string
    (* Overwrite with the newer expression for duplicate global assigns *)
    |> Ast.Reference.Map.of_alist_reduce ~f:(fun _old updated -> updated)


  (* Convenience wrapper used by tests, which naturally have a module qualifier in hand. *)
  let from_qualifier ~pyrefly_api ~callables_to_definitions_map qualifier =
    from_module
      ~pyrefly_api
      ~callables_to_definitions_map
      (PyreflyApi.ReadOnly.module_id_of_qualifier pyrefly_api qualifier)


  let from_modules ~pyrefly_api ~callables_to_definitions_map ~module_ids =
    let build_per_module module_id =
      from_module ~pyrefly_api ~callables_to_definitions_map module_id
    in
    let reduce =
      let merge ~key = function
        | `Both (left, right) when not (StringLiteral.equal left right) ->
            failwith
              (Format.asprintf
                 "Two different globals in different modules with the same unqualified name `%s` \
                  and different values `%s` and `%s`."
                 (StringLiteral.show left)
                 (StringLiteral.show right)
                 (Reference.show key))
        | `Both (value, _)
        | `Left value
        | `Right value ->
            Some value
      in
      Map.merge ~f:merge
    in
    module_ids |> List.map ~f:build_per_module |> Algorithms.fold_balanced ~init:empty ~f:reduce


  (* Convenience wrapper used by tests, which naturally have a module qualifier in hand. *)
  let from_qualifiers ~pyrefly_api ~callables_to_definitions_map ~qualifiers =
    let module_ids =
      List.map ~f:(PyreflyApi.ReadOnly.module_id_of_qualifier pyrefly_api) qualifiers
    in
    from_modules ~pyrefly_api ~callables_to_definitions_map ~module_ids
end

module SharedMemory = struct
  module T =
    SaveLoadSharedMemory.MakeKeyValue
      (PyrePysaLogic.SharedMemoryKeys.ReferenceKey)
      (struct
        type t = StringLiteral.t

        let prefix = Hack_parallel.Std.Prefix.make ()

        let handle_prefix = Hack_parallel.Std.Prefix.make ()

        let description = "Mapping from fully qualified global name to expression"
      end)

  include T

  let from_heap heap = heap |> Map.to_alist |> T.of_alist_sequential

  let from_modules
      ~scheduler
      ~scheduler_policies
      ~pyrefly_api
      ~callables_to_definitions_map
      ~module_ids
    =
    let scheduler_policy =
      Scheduler.Policy.from_configuration_or_default
        scheduler_policies
        Configuration.ScheduleIdentifier.GlobalConstants
        ~default:
          (Scheduler.Policy.fixed_chunk_count
             ~minimum_chunks_per_worker:1
             ~minimum_chunk_size:1
             ~preferred_chunks_per_worker:1
             ())
    in
    let handle = T.create () |> T.add_only in
    let empty_handle = T.AddOnly.create_empty handle in
    let add_heap handle heap =
      Map.fold heap ~init:handle ~f:(fun ~key ~data handle -> T.AddOnly.add handle key data)
    in
    Scheduler.map_reduce
      scheduler
      ~policy:scheduler_policy
      ~initial:handle
      ~map:(fun module_ids ->
        add_heap
          empty_handle
          (Heap.from_modules ~pyrefly_api ~callables_to_definitions_map ~module_ids))
      ~reduce:(fun smaller larger -> T.AddOnly.merge_same_handle_disjoint_keys ~smaller ~larger)
      ~inputs:module_ids
      ()
    |> T.from_add_only
end
