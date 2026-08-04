(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* Target: represents a global symbol that might have information attached to it.
 *
 * This is mostly used to represent callables in the interprocedural framework,
 * using `Function` or `Method`.
 * `Override f` represents the set of methods overriding the method `f`.
 * `GlobalVariable` and `ClassAttribute` represent modeled objects.
 * `Artificial` represents synthetic analysis targets.
 *)

open Core
open Ast
module CallableId = PyreflyTypes.CallableId
module ModuleId = PyreflyTypes.ModuleId
module ClassId = PyreflyTypes.ClassId
module DisplayApi = PyreflyTypes.DisplayApi

module ArtificialKind = struct
  type t =
    | FormatString
    | StrAdd
    | StrMod
    | StrFormat
    | StrLiteral
    | Condition
  [@@deriving sexp, compare, hash, equal, show]

  let name = function
    | FormatString -> "<format-string>"
    | StrAdd -> "<str.__add__>"
    | StrMod -> "<str.__mod__>"
    | StrFormat -> "<str.format>"
    | StrLiteral -> "<literal-string>"
    | Condition -> "<condition>"
end

module Regular = struct
  type t =
    | Function of CallableId.t
    | Method of CallableId.t
    | Override of CallableId.t
    | GlobalVariable of {
        module_id: ModuleId.t;
        name: string;
      }
    | ClassInstanceAttribute of {
        class_id: ClassId.t;
        name: string;
      }
    | ClassTypeAttribute of {
        class_id: ClassId.t;
        name: string;
      }
    | Artificial of ArtificialKind.t
    | UnknownCallee of string
  [@@deriving sexp, hash, equal]

  (* Lower priority appears earlier in comparison. *)
  let priority = function
    | Function _ -> 0
    | Method _ -> 1
    | Override _ -> 2
    | GlobalVariable _ -> 3
    | ClassInstanceAttribute _ -> 4
    | ClassTypeAttribute _ -> 5
    | Artificial _ -> 6
    | UnknownCallee _ -> 7


  let compare left right =
    let priority_comparison = Int.compare (priority left) (priority right) in
    if priority_comparison <> 0 then
      priority_comparison
    else
      match left, right with
      | Function first, Function second
      | Method first, Method second
      | Override first, Override second ->
          CallableId.compare first second
      | ( GlobalVariable { module_id = first_id; name = first_name },
          GlobalVariable { module_id = second_id; name = second_name } ) ->
          [%compare: ModuleId.t * string] (first_id, first_name) (second_id, second_name)
      | ( ClassInstanceAttribute { class_id = first_id; name = first_name },
          ClassInstanceAttribute { class_id = second_id; name = second_name } )
      | ( ClassTypeAttribute { class_id = first_id; name = first_name },
          ClassTypeAttribute { class_id = second_id; name = second_name } ) ->
          [%compare: ClassId.t * string] (first_id, first_name) (second_id, second_name)
      | Artificial first, Artificial second -> ArtificialKind.compare first second
      | UnknownCallee first, UnknownCallee second -> String.compare first second
      | _ -> failwith "The compared targets must belong to the same variant."


  (* Structural, api-free pretty-printer. Used for debug output, logging, and `[@@deriving show]`
     contexts that hold no display api. Renders the raw packed id, not a name. *)
  let pp formatter = function
    | Function callable_id -> Format.fprintf formatter "Function(%a)" CallableId.pp callable_id
    | Method callable_id -> Format.fprintf formatter "Method(%a)" CallableId.pp callable_id
    | Override callable_id -> Format.fprintf formatter "Override(%a)" CallableId.pp callable_id
    | GlobalVariable { module_id; name } ->
        Format.fprintf formatter "GlobalVariable(%a, %s)" ModuleId.pp module_id name
    | ClassInstanceAttribute { class_id; name } ->
        Format.fprintf formatter "ClassInstanceAttribute(%a, %s)" ClassId.pp class_id name
    | ClassTypeAttribute { class_id; name } ->
        Format.fprintf formatter "ClassTypeAttribute(%a, %s)" ClassId.pp class_id name
    | Artificial kind -> Format.fprintf formatter "Artificial(%s)" (ArtificialKind.name kind)
    | UnknownCallee name -> Format.fprintf formatter "UnknownCallee(%s)" name


  let show = Format.asprintf "%a" pp

  (* Structural (api-free) pretty-printers. Kept for debug/logging/test-printer sites; they render
     the raw id, not a name. Golden-generating sites use the `*_with_display_api` variants below. *)
  let pp_pretty formatter = function
    | Function callable_id
    | Method callable_id ->
        Format.fprintf formatter "%a" CallableId.pp callable_id
    | Override callable_id -> Format.fprintf formatter "Overrides{%a}" CallableId.pp callable_id
    | GlobalVariable { module_id; name } ->
        Format.fprintf formatter "Object{%a.%s}" ModuleId.pp module_id name
    | ClassInstanceAttribute { class_id; name } ->
        Format.fprintf formatter "Object{%a.%s}" ClassId.pp class_id name
    | ClassTypeAttribute { class_id; name } ->
        Format.fprintf formatter "Object{%a.__class__.%s}" ClassId.pp class_id name
    | Artificial kind -> Format.fprintf formatter "Object{%s}" (ArtificialKind.name kind)
    | UnknownCallee name -> Format.fprintf formatter "Object{unknown-callee:%s}" name


  let pp_pretty_with_kind formatter = function
    | Function callable_id -> Format.fprintf formatter "%a (fun)" CallableId.pp callable_id
    | Method callable_id -> Format.fprintf formatter "%a (method)" CallableId.pp callable_id
    | Override callable_id -> Format.fprintf formatter "%a (override)" CallableId.pp callable_id
    | GlobalVariable { module_id; name } ->
        Format.fprintf formatter "%a.%s (object)" ModuleId.pp module_id name
    | ClassInstanceAttribute { class_id; name } ->
        Format.fprintf formatter "%a.%s (object)" ClassId.pp class_id name
    | ClassTypeAttribute { class_id; name } ->
        Format.fprintf formatter "%a.__class__.%s (object)" ClassId.pp class_id name
    | Artificial kind -> Format.fprintf formatter "%s (object)" (ArtificialKind.name kind)
    | UnknownCallee name -> Format.fprintf formatter "unknown-callee:%s (object)" name


  (* Decode a callable id to its external (user-facing) name through the display api. The external
     name already includes the `@decorated` suffix for decorated callables. *)
  let callable_external_name ~display_api:{ DisplayApi.callable_external_name; _ } callable_id =
    Reference.show (callable_external_name callable_id)


  (* `transform` (default identity) rewrites the rendered qualified name; it is used to strip the
     pyrefly source-path prefix. Byte-identical to the old string-based rendering: the display api
     decodes the same qualified name the string target carried, and `strip_path_prefix` only affects
     the leading module prefix (the `@decorated` suffix has no colon, so it is left intact). *)
  let pp_external
      ~display_api:({ DisplayApi.module_name; class_name; _ } as display_api)
      ?(transform = Fn.id)
      formatter
    = function
    | Function callable_id
    | Method callable_id ->
        Format.fprintf formatter "%s" (transform (callable_external_name ~display_api callable_id))
    | Override callable_id ->
        Format.fprintf
          formatter
          "Overrides{%s}"
          (transform (callable_external_name ~display_api callable_id))
    | GlobalVariable { module_id; name } ->
        Format.fprintf
          formatter
          "Obj{%s.%s}"
          (transform (Reference.show (module_name module_id)))
          name
    | ClassInstanceAttribute { class_id; name } ->
        Format.fprintf
          formatter
          "Obj{%s.%s}"
          (transform (Reference.show (class_name class_id)))
          name
    | ClassTypeAttribute { class_id; name } ->
        Format.fprintf
          formatter
          "Obj{%s.__class__.%s}"
          (transform (Reference.show (class_name class_id)))
          name
    | Artificial kind -> Format.fprintf formatter "Obj{%s}" (transform (ArtificialKind.name kind))
    | UnknownCallee name -> Format.fprintf formatter "Obj{unknown-callee:%s}" name


  let pp_pretty_with_display_api
      ~display_api:({ DisplayApi.module_name; class_name; _ } as display_api)
      formatter
    = function
    | Function callable_id
    | Method callable_id ->
        Format.fprintf formatter "%s" (callable_external_name ~display_api callable_id)
    | Override callable_id ->
        Format.fprintf formatter "Overrides{%s}" (callable_external_name ~display_api callable_id)
    | GlobalVariable { module_id; name } ->
        Format.fprintf formatter "Object{%a.%s}" Reference.pp (module_name module_id) name
    | ClassInstanceAttribute { class_id; name } ->
        Format.fprintf formatter "Object{%a.%s}" Reference.pp (class_name class_id) name
    | ClassTypeAttribute { class_id; name } ->
        Format.fprintf formatter "Object{%a.__class__.%s}" Reference.pp (class_name class_id) name
    | Artificial kind -> Format.fprintf formatter "Object{%s}" (ArtificialKind.name kind)
    | UnknownCallee name -> Format.fprintf formatter "Object{unknown-callee:%s}" name


  let pp_pretty_with_kind_with_display_api
      ~display_api:({ DisplayApi.module_name; class_name; _ } as display_api)
      formatter
    = function
    | Function callable_id ->
        Format.fprintf formatter "%s (fun)" (callable_external_name ~display_api callable_id)
    | Method callable_id ->
        Format.fprintf formatter "%s (method)" (callable_external_name ~display_api callable_id)
    | Override callable_id ->
        Format.fprintf formatter "%s (override)" (callable_external_name ~display_api callable_id)
    | GlobalVariable { module_id; name } ->
        Format.fprintf formatter "%a.%s (object)" Reference.pp (module_name module_id) name
    | ClassInstanceAttribute { class_id; name } ->
        Format.fprintf formatter "%a.%s (object)" Reference.pp (class_name class_id) name
    | ClassTypeAttribute { class_id; name } ->
        Format.fprintf formatter "%a.__class__.%s (object)" Reference.pp (class_name class_id) name
    | Artificial kind -> Format.fprintf formatter "%s (object)" (ArtificialKind.name kind)
    | UnknownCallee name -> Format.fprintf formatter "unknown-callee:%s (object)" name


  let get_corresponding_method_exn = function
    | Override callable_id -> Method callable_id
    | _ -> failwith "not an override target"


  let get_corresponding_override_exn = function
    | Method callable_id -> Override callable_id
    | _ -> failwith "unexpected"


  let object_name ~display_api:{ DisplayApi.module_name; class_name; _ } = function
    | GlobalVariable { module_id; name } -> Reference.create ~prefix:(module_name module_id) name
    | ClassInstanceAttribute { class_id; name } ->
        Reference.create ~prefix:(class_name class_id) name
    | ClassTypeAttribute { class_id; name } ->
        Reference.create ~prefix:(class_name class_id) ("__class__." ^ name)
    | Artificial kind -> Reference.create (ArtificialKind.name kind)
    | UnknownCallee unknown_callee ->
        Reference.create (Format.sprintf "unknown-callee:%s" unknown_callee)
    | _ -> failwith "unexpected"


  let is_function_or_method = function
    | Function _
    | Method _ ->
        true
    | Override _
    | GlobalVariable _
    | ClassInstanceAttribute _
    | ClassTypeAttribute _
    | Artificial _
    | UnknownCallee _ ->
        false


  let is_method_or_override = function
    | Method _
    | Override _ ->
        true
    | Function _
    | GlobalVariable _
    | ClassInstanceAttribute _
    | ClassTypeAttribute _
    | Artificial _
    | UnknownCallee _ ->
        false


  let is_method = function
    | Method _ -> true
    | _ -> false


  let is_function = function
    | Function _ -> true
    | _ -> false


  let is_override = function
    | Override _ -> true
    | _ -> false


  let is_object = function
    | GlobalVariable _
    | ClassInstanceAttribute _
    | ClassTypeAttribute _
    | Artificial _
    | UnknownCallee _ ->
        true
    | _ -> false


  let is_decorated = function
    | Function callable_id
    | Method callable_id
    | Override callable_id ->
        CallableId.is_decorated callable_id
    | GlobalVariable _
    | ClassInstanceAttribute _
    | ClassTypeAttribute _
    | Artificial _
    | UnknownCallee _ ->
        false


  let override_to_method = function
    | Override callable_id -> Method callable_id
    | ( Function _ | Method _ | GlobalVariable _ | ClassInstanceAttribute _ | ClassTypeAttribute _
      | Artificial _ | UnknownCallee _ ) as regular ->
        regular


  (* Mark a callable as its decorated variant (the `FunctionDecoratedTarget` id tag). *)
  let to_decorated =
    let mark callable_id =
      if CallableId.is_decorated callable_id then
        failwith "to_decorated on already decorated target"
      else
        CallableId.to_decorated callable_id
    in
    function
    | Function callable_id -> Function (mark callable_id)
    | Method callable_id -> Method (mark callable_id)
    | Override callable_id -> Override (mark callable_id)
    | GlobalVariable _
    | ClassInstanceAttribute _
    | ClassTypeAttribute _
    | Artificial _
    | UnknownCallee _ ->
        failwith "to_decorated on non-callable target"


  (* Mark a decorated callable as its undecorated variant, or raise an error. *)
  let to_undecorated_exn =
    let unmark callable_id =
      if CallableId.is_decorated callable_id then
        CallableId.to_undecorated callable_id
      else
        failwith "to_undecorated_exn on non decorated target"
    in
    function
    | Function callable_id -> Function (unmark callable_id)
    | Method callable_id -> Method (unmark callable_id)
    | Override callable_id -> Override (unmark callable_id)
    | GlobalVariable _
    | ClassInstanceAttribute _
    | ClassTypeAttribute _
    | Artificial _
    | UnknownCallee _ ->
        failwith "to_undecorated_exn on non-callable target"
end

module ParameterMap = Data_structures.SerializableMap.Make (AccessPath.Root)

module T = struct
  type t =
    | Regular of Regular.t
    | Parameterized of {
        regular: Regular.t;
        parameters: parameter_value ParameterMap.t;
      }
  (* This represents a regular callable with its function-typed parameters being instantited with
     `parameters`. *)

  and parameter_value = {
    target: t;
    (* Whether an implicit receiver (self/cls) was provided at the capture site for `target`. This
       is the `CallTarget.implicit_receiver` of the captured callee, preserved so that the inlined
       call can be reconstructed without an off-by-one argument shift. *)
    implicit_receiver: bool;
  }
  [@@deriving show { with_path = false }, sexp, compare, hash, equal]
end

include T

module Map = struct
  include Data_structures.SerializableMap.Make (T)

  module Tree = Map.Make_tree (struct
    include T
    include Comparator.Make (T)
  end)
end

let rec pp_from_regular ~pp_regular formatter = function
  | Regular regular -> pp_regular formatter regular
  | Parameterized { regular; parameters } ->
      let recursive_pp = pp_from_regular ~pp_regular in
      let pp_parameter_value formatter { target; implicit_receiver } =
        Format.fprintf
          formatter
          "%a%s"
          recursive_pp
          target
          (if implicit_receiver then " (bound)" else "")
      in
      let rec pp_parameters formatter = function
        | [] -> Format.fprintf formatter ""
        | [(access_path, parameter_value)] ->
            Format.fprintf
              formatter
              "%a=%a"
              AccessPath.Root.pp
              access_path
              pp_parameter_value
              parameter_value
        | (access_path, parameter_value) :: tail ->
            let () =
              Format.fprintf
                formatter
                "%a=%a, "
                AccessPath.Root.pp
                access_path
                pp_parameter_value
                parameter_value
            in
            pp_parameters formatter tail
      in
      Format.fprintf
        formatter
        "%a[%a]"
        pp_regular
        regular
        pp_parameters
        (ParameterMap.to_alist parameters)


let pp_internal = pp

let show_internal = Format.asprintf "%a" pp_internal

(* Equivalent to pp_internal. Required by @@deriving. *)
let pp = pp_internal

let pp_pretty = pp_from_regular ~pp_regular:Regular.pp_pretty

let show_pretty = Format.asprintf "%a" pp_pretty

module ParameterValue = struct
  type t = parameter_value = {
    target: T.t;
    implicit_receiver: bool;
  }

  let create ?(implicit_receiver = false) target = { target; implicit_receiver }

  let target { target; _ } = target

  let pp_pretty formatter { target; implicit_receiver } =
    Format.fprintf formatter "%a%s" pp_pretty target (if implicit_receiver then " (bound)" else "")
end

let pp_pretty_with_kind = pp_from_regular ~pp_regular:Regular.pp_pretty_with_kind

let show_pretty_with_kind = Format.asprintf "%a" pp_pretty_with_kind

(* Api-aware pretty-printers used by golden-generating output sites (call/override graph dumps), so
   those keep rendering names (not raw ids) after the payload swap. *)
let pp_pretty_with_display_api ~display_api =
  pp_from_regular ~pp_regular:(Regular.pp_pretty_with_display_api ~display_api)


let show_pretty_with_display_api ~display_api =
  Format.asprintf "%a" (pp_pretty_with_display_api ~display_api)


let pp_pretty_with_kind_with_display_api ~display_api =
  pp_from_regular ~pp_regular:(Regular.pp_pretty_with_kind_with_display_api ~display_api)


let show_pretty_with_kind_with_display_api ~display_api =
  Format.asprintf "%a" (pp_pretty_with_kind_with_display_api ~display_api)


let pp_external ~display_api = pp_from_regular ~pp_regular:(Regular.pp_external ~display_api)

(* Render a target as an external (user-facing) name, decoding packed ids through the display api.
   The external name includes the `@decorated` suffix for decorated callables. `transform` (default
   identity) rewrites the rendered qualified name; it is used to strip the pyrefly source-path
   prefix without duplicating the rendering (see `Regular.pp_external`). *)
let external_name ~display_api ?transform target =
  Format.asprintf
    "%a"
    (pp_from_regular ~pp_regular:(Regular.pp_external ~display_api ?transform))
    target


let from_regular regular = Regular regular

let get_regular = function
  | Regular regular
  | Parameterized { regular; _ } ->
      regular


(* Return the callable id carried by a target. `Function`, `Method` and `Override` all carry one (an
   override wraps the callable id of the method it overrides); non-callable targets have none.
   Returns the id as-is, without stripping the `@decorated` tag - that is the caller's
   responsibility. *)
let callable_id target =
  match get_regular target with
  | Regular.Function callable_id
  | Regular.Method callable_id
  | Regular.Override callable_id ->
      Some callable_id
  | Regular.GlobalVariable _
  | Regular.ClassInstanceAttribute _
  | Regular.ClassTypeAttribute _
  | Regular.Artificial _
  | Regular.UnknownCallee _ ->
      None


let callable_id_exn target =
  match callable_id target with
  | Some callable_id -> callable_id
  | None -> Format.asprintf "expected a callable target, got %a" pp target |> failwith


let module_id target =
  match get_regular target with
  | Regular.Function callable_id
  | Regular.Method callable_id
  | Regular.Override callable_id ->
      Some (CallableId.module_id callable_id)
  | Regular.GlobalVariable { module_id; _ } -> Some module_id
  | Regular.ClassInstanceAttribute { class_id; _ }
  | Regular.ClassTypeAttribute { class_id; _ } ->
      Some (ClassId.module_id class_id)
  | Regular.Artificial _ -> None
  | Regular.UnknownCallee _ -> None


let module_id_exn target =
  match module_id target with
  | Some module_id -> module_id
  | None -> Format.asprintf "expected a callable target, got %a" pp target |> failwith


let undecorated_callable_id_exn target =
  let callable_id = callable_id_exn target in
  if CallableId.is_decorated callable_id then
    CallableId.to_undecorated callable_id
  else
    callable_id


(* Decode a `Method`/`Override`'s class name (the prefix of its define name) via the display api.
   `None` for functions and objects. *)
let class_name ~display_api:{ DisplayApi.callable_define_name; _ } target =
  match get_regular target with
  | Regular.Method callable_id
  | Regular.Override callable_id ->
      callable_define_name callable_id |> Reference.prefix |> Option.map ~f:Reference.show
  | Regular.Function _
  | Regular.GlobalVariable _
  | Regular.ClassInstanceAttribute _
  | Regular.ClassTypeAttribute _
  | Regular.Artificial _
  | Regular.UnknownCallee _ ->
      None


let class_name_exn ~display_api target =
  match class_name ~display_api target with
  | Some name -> name
  | None -> Format.asprintf "expected a method target, got %a" pp target |> failwith


(* Decode a `Method`/`Override`'s bare method name (the last component of its define name) via the
   display api. `None` for functions and objects. *)
let method_name ~display_api:{ DisplayApi.callable_define_name; _ } target =
  match get_regular target with
  | Regular.Method callable_id
  | Regular.Override callable_id ->
      Some (Reference.last (callable_define_name callable_id))
  | Regular.Function _
  | Regular.GlobalVariable _
  | Regular.ClassInstanceAttribute _
  | Regular.ClassTypeAttribute _
  | Regular.Artificial _
  | Regular.UnknownCallee _ ->
      None


let method_name_exn ~display_api target =
  match method_name ~display_api target with
  | Some name -> name
  | None -> Format.asprintf "expected a method target, got %a" pp target |> failwith


(* Decode a `Function`'s define name via the display api. `None` otherwise. *)
let function_name ~display_api:{ DisplayApi.callable_define_name; _ } target =
  match get_regular target with
  | Regular.Function callable_id -> Some (Reference.show (callable_define_name callable_id))
  | Regular.Method _
  | Regular.Override _
  | Regular.GlobalVariable _
  | Regular.ClassInstanceAttribute _
  | Regular.ClassTypeAttribute _
  | Regular.Artificial _
  | Regular.UnknownCallee _ ->
      None


let function_name_exn ~display_api target =
  match function_name ~display_api target with
  | Some name -> name
  | None -> Format.asprintf "expected a function target, got %a" pp target |> failwith


(* Decode a `Function`/`Method`'s fully-qualified define name via the display api. `None` for
   overrides and objects (which have no define name). *)
let define_name ~display_api:{ DisplayApi.callable_define_name; _ } target =
  match get_regular target with
  | Regular.Function callable_id
  | Regular.Method callable_id ->
      Some (callable_define_name callable_id)
  | Regular.Override _
  | Regular.GlobalVariable _
  | Regular.ClassInstanceAttribute _
  | Regular.ClassTypeAttribute _
  | Regular.Artificial _
  | Regular.UnknownCallee _ ->
      None


let define_name_exn ~display_api target =
  match define_name ~display_api target with
  | None -> Format.asprintf "expected a function or method target, got %a" pp target |> failwith
  | Some name -> name


let strip_parameters target = target |> get_regular |> from_regular

let as_regular_exn = function
  | Regular regular -> regular
  | Parameterized _ -> failwith "expect `Regular`"


let collect_nested_regular_targets =
  let rec fold sofar = function
    | Regular regular -> regular :: sofar
    | Parameterized { regular; parameters } ->
        ParameterMap.fold
          (fun _ { target; _ } sofar -> fold sofar target)
          parameters
          (regular :: sofar)
  in
  fold []


let create_function callable_id = Function callable_id |> from_regular

let create_method callable_id = Method callable_id |> from_regular

let create_override callable_id = Override callable_id |> from_regular

let create_global_variable module_id name = GlobalVariable { module_id; name } |> from_regular

let create_class_instance_attribute class_id name =
  ClassInstanceAttribute { class_id; name } |> from_regular


let create_class_type_attribute class_id name =
  ClassTypeAttribute { class_id; name } |> from_regular


let get_corresponding_method_exn ~must_be_regular target =
  (if must_be_regular then
     as_regular_exn target
  else
    get_regular target)
  |> Regular.get_corresponding_method_exn
  |> from_regular


let object_name ~display_api target = target |> get_regular |> Regular.object_name ~display_api

let is_function_or_method target = target |> get_regular |> Regular.is_function_or_method

let is_method_or_override target = target |> get_regular |> Regular.is_method_or_override

let is_method target = target |> get_regular |> Regular.is_method

let is_function target = target |> get_regular |> Regular.is_function

let is_override target = target |> get_regular |> Regular.is_override

let is_object target = target |> get_regular |> Regular.is_object

let is_decorated target = target |> get_regular |> Regular.is_decorated

let is_parameterized = function
  | Regular _ -> false
  | Parameterized _ -> true


let is_regular = function
  | Regular _ -> true
  | Parameterized _ -> false


(* A parameterized target contains recursive targets if one of its `regular` part also appears in
   one of its `parameters` part. Such recursion may lead to non-termination in high-order call graph
   building. *)
let contain_recursive_target target =
  let rec contain_recursive_target existing_regulars = function
    | Regular regular -> List.exists existing_regulars ~f:(Regular.equal regular)
    | Parameterized { regular; parameters } ->
        List.exists existing_regulars ~f:(Regular.equal regular)
        || ParameterMap.exists
             (fun _ { target; _ } -> contain_recursive_target (regular :: existing_regulars) target)
             parameters
  in
  contain_recursive_target [] target


(* Return the level of target nestedness within a given target. *)
let rec depth = function
  | Regular _ -> 1
  | Parameterized { parameters; _ } ->
      1
      + (parameters
        |> ParameterMap.data
        |> List.map ~f:(fun { target; _ } -> depth target)
        |> List.max_elt ~compare:Int.compare
        |> Option.value ~default:0)


let rec for_issue_handle = function
  | Regular regular -> regular |> Regular.override_to_method |> from_regular
  | Parameterized { regular; parameters } ->
      Parameterized
        {
          regular = Regular.override_to_method regular;
          parameters =
            ParameterMap.map
              (fun ({ target; _ } as value) -> { value with target = for_issue_handle target })
              parameters;
        }


let to_decorated = function
  | Regular regular -> Regular (Regular.to_decorated regular)
  | Parameterized { regular; parameters } ->
      Parameterized { regular = Regular.to_decorated regular; parameters }


let to_undecorated_exn = function
  | Regular regular -> Regular (Regular.to_undecorated_exn regular)
  | Parameterized { regular; parameters } ->
      Parameterized { regular = Regular.to_undecorated_exn regular; parameters }


module MakePrettyPrintContainer (Container : sig
  type container

  val pp : Format.formatter -> T.t -> unit

  val elements : container -> T.t list

  val separator : string

  val left_bracket : string

  val right_bracket : string
end) =
struct
  let pp formatter container =
    match Container.elements container with
    | [] -> Format.fprintf formatter "%s%s" Container.left_bracket Container.right_bracket
    | [element] ->
        Format.fprintf
          formatter
          "%s%a%s"
          Container.left_bracket
          Container.pp
          element
          Container.right_bracket
    | list ->
        let pp_element formatter element =
          Format.fprintf formatter "@%s%a" Container.separator Container.pp element
        in
        let pp_elements formatter = List.iter ~f:(pp_element formatter) in
        Format.fprintf
          formatter
          "%s@[<v 2>%a@]@,%s"
          Container.left_bracket
          pp_elements
          list
          Container.right_bracket


  let show = Format.asprintf "%a" pp
end

module Set = struct
  include Stdlib.Set.Make (T)

  module PrettyPrintWithKind = MakePrettyPrintContainer (struct
    type container = t

    let elements = elements

    let pp = pp_pretty_with_kind

    let separator = ","

    let left_bracket = "{"

    let right_bracket = "}"
  end)

  let pp_pretty_with_kind = PrettyPrintWithKind.pp

  let show_pretty_with_kind = PrettyPrintWithKind.show
end

module Hashable = Core.Hashable.Make (T)
module HashMap = Hashable.Table
module HashSet = Hashable.Hash_set

(* Define the meaning of `skip_analysis_targets`. We assume `skip_analysis_targets` only contains
   regular callables. *)
let should_skip_analysis ~skip_analysis_targets target =
  target |> strip_parameters |> Core.Hash_set.mem skip_analysis_targets


module ArtificialTargets = struct
  let create kind = Artificial kind |> from_regular

  let format_string = create ArtificialKind.FormatString

  let str_add = create ArtificialKind.StrAdd

  let str_mod = create ArtificialKind.StrMod

  let str_format = create ArtificialKind.StrFormat

  let str_literal = create ArtificialKind.StrLiteral

  let condition = create ArtificialKind.Condition
end

module SharedMemoryKey = struct
  include T

  let to_string key = sexp_of_t key |> Sexp.to_string

  let from_string sexp_string = Sexp.of_string sexp_string |> t_of_sexp
end

(* Represent a hashset of targets inside the shared memory *)
module HashsetSharedMemory = struct
  type target = T.t

  module T =
    Hack_parallel.Std.SharedMemory.FirstClassWithKeys.Make
      (SharedMemoryKey)
      (struct
        type t = unit

        let prefix = Hack_parallel.Std.Prefix.make ()

        let description = "A set of targets"
      end)

  type t = T.t

  let cleanup = T.cleanup

  let from_heap targets = targets |> List.map ~f:(fun target -> target, ()) |> T.of_alist_sequential

  module ReadOnly = T.ReadOnly

  let read_only = T.read_only
end

module List = struct
  type t = T.t list

  module PrettyPrintWithKind = MakePrettyPrintContainer (struct
    type container = t

    let elements = Fn.id

    let pp = pp_pretty_with_kind

    let separator = ";"

    let left_bracket = "["

    let right_bracket = "]"
  end)

  let pp_pretty_with_kind = PrettyPrintWithKind.pp

  let show_pretty_with_kind = PrettyPrintWithKind.show
end
