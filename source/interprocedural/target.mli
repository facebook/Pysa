(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Ast

module ArtificialKind : sig
  type t =
    | FormatString
    | StrAdd
    | StrMod
    | StrFormat
    | StrLiteral
    | Condition
  [@@deriving sexp, compare, hash, equal, show]

  val name : t -> string
end

module Regular : sig
  type t =
    | Function of PyreflyTypes.CallableId.t
    | Method of PyreflyTypes.CallableId.t
    | Override of PyreflyTypes.CallableId.t
    | GlobalVariable of {
        module_id: PyreflyTypes.ModuleId.t;
        name: string;
      }
    | ClassInstanceAttribute of {
        class_id: PyreflyTypes.ClassId.t;
        name: string;
      }
    | ClassTypeAttribute of {
        class_id: PyreflyTypes.ClassId.t;
        name: string;
      }
    | Artificial of ArtificialKind.t
    | UnknownCallee of string
  [@@deriving sexp, compare, hash, equal, show]

  val override_to_method : t -> t

  val get_corresponding_method_exn : t -> t

  val get_corresponding_override_exn : t -> t

  val is_decorated : t -> bool

  (* Mark a callable as its decorated variant (the `FunctionDecoratedTarget` id tag). *)
  val to_decorated : t -> t

  (* Mark a decorated callable as its undecorated variant, or raise an error. *)
  val to_undecorated_exn : t -> t
end

module ParameterMap : Data_structures.SerializableMap.S with type key = AccessPath.Root.t

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
  (* Whether an implicit receiver (self/cls) was provided at the capture site for `target`. *)
  implicit_receiver: bool;
}
[@@deriving sexp, compare, hash, equal]

module T : sig
  type nonrec t = t [@@deriving sexp, compare, hash, equal]
end

module ParameterValue : sig
  type t = parameter_value = {
    target: T.t;
    implicit_receiver: bool;
  }

  val create : ?implicit_receiver:bool -> T.t -> t

  val target : t -> T.t

  val pp_pretty : display_api:PyreflyTypes.DisplayApi.t -> Format.formatter -> t -> unit
end

module Map : sig
  include Data_structures.SerializableMap.S with type key = t

  module Tree : module type of struct
    include Core.Map.Make_tree (struct
      include T
      include Core.Comparator.Make (T)
    end)
  end
end

(* Pretty printers. *)

val pp_pretty : display_api:PyreflyTypes.DisplayApi.t -> Format.formatter -> t -> unit

val show_pretty : display_api:PyreflyTypes.DisplayApi.t -> t -> string

val pp_external : display_api:PyreflyTypes.DisplayApi.t -> Format.formatter -> t -> unit

(* Render a target as an external (user-facing) name, decoding packed ids through the display api.
   `transform` (default identity) rewrites the rendered qualified name; it is used to strip the
   pyrefly source-path prefix. *)
val external_name
  :  display_api:PyreflyTypes.DisplayApi.t ->
  ?transform:(string -> string) ->
  t ->
  string

(* Decode a `Method`/`Override`'s class name / bare method name via the display api. `None` for
   functions and objects. *)
val class_name : display_api:PyreflyTypes.DisplayApi.t -> t -> string option

(* Returns the class name; raises if the target isn't a method or override. *)
val class_name_exn : display_api:PyreflyTypes.DisplayApi.t -> t -> string

val method_name : display_api:PyreflyTypes.DisplayApi.t -> t -> string option

(* Returns the bare method name; raises if the target isn't a method or override. *)
val method_name_exn : display_api:PyreflyTypes.DisplayApi.t -> t -> string

(* Decode a `Function`'s fully-qualified name via the display api. `None` otherwise. *)
val function_name : display_api:PyreflyTypes.DisplayApi.t -> t -> string option

(* Returns the function name; raises if the target isn't a function. *)
val function_name_exn : display_api:PyreflyTypes.DisplayApi.t -> t -> string

(* Decode a `Function`/`Method`'s fully-qualified define name via the display api. `None` for
   overrides and objects. *)
val define_name : display_api:PyreflyTypes.DisplayApi.t -> t -> Reference.t option

val define_name_exn : display_api:PyreflyTypes.DisplayApi.t -> t -> Reference.t

(* Structural printer required by `[@@deriving]`. Prefer `pp_pretty` for explicit output. *)
val pp : Format.formatter -> t -> unit

(* Constructors. *)

val create_function : PyreflyTypes.CallableId.t -> t

val create_method : PyreflyTypes.CallableId.t -> t

val create_override : PyreflyTypes.CallableId.t -> t

val create_global_variable : PyreflyTypes.ModuleId.t -> string -> t

val create_class_instance_attribute : PyreflyTypes.ClassId.t -> string -> t

val create_class_type_attribute : PyreflyTypes.ClassId.t -> string -> t

val from_regular : Regular.t -> t

(* Return `Regular.t` when called on any `t`. *)
val get_regular : t -> Regular.t

(* Return `Regular.t`, but throw if called on `Parameterized`. *)
val as_regular_exn : t -> Regular.t

(* Return the callable id carried by a `Function`, `Method` or `Override` target (an override wraps
   the callable id of the method it overrides). `None` for non-callable targets. The id is returned
   as-is, without stripping the `@decorated` tag. *)
val callable_id : t -> PyreflyTypes.CallableId.t option

(* Like `callable_id`, but raises on non-callable targets. *)
val callable_id_exn : t -> PyreflyTypes.CallableId.t

val module_id : t -> PyreflyTypes.ModuleId.t option

val module_id_exn : t -> PyreflyTypes.ModuleId.t

(* Like `callable_id_exn`, but strips the `@decorated` tag, yielding an id suitable for the id-keyed
   shared memories (metadata, signatures, captures, type-of-expressions, ...), which are all keyed
   by undecorated ids. Raises on `Object` targets. *)
val undecorated_callable_id_exn : t -> PyreflyTypes.CallableId.t

val strip_parameters : t -> t

val collect_nested_regular_targets : t -> Regular.t list

val get_corresponding_method_exn : must_be_regular:bool -> t -> t

(* Accessors. *)

val object_name : display_api:PyreflyTypes.DisplayApi.t -> t -> Reference.t

val is_function_or_method : t -> bool

val is_method_or_override : t -> bool

val is_method : t -> bool

val is_function : t -> bool

val is_override : t -> bool

val is_object : t -> bool

val is_decorated : t -> bool

val is_parameterized : t -> bool

val is_regular : t -> bool

(* A parameterized target contains recursive targets if one of its `regular` part also appears in
   one of its `parameters` part. Such recursion may lead to non-termination in high-order call graph
   building. *)
val contain_recursive_target : t -> bool

(* Return the level of target nestedness within a given target. *)
val depth : t -> int

val for_issue_handle : t -> t

(* Mark a callable as its decorated / undecorated variant (the `CallableId` tag). Idempotent. *)
val to_decorated : t -> t

(* Mark a decorated callable as its undecorated variant, or raise an error. *)
val to_undecorated_exn : t -> t

module Set : sig
  include Stdlib.Set.S with type elt = t
end

module HashMap : Core.Hashtbl.S with type key := t

module HashSet : Core.Hash_set.S with type elt := t

(* Define the meaning of `skip_analysis_targets`. We assume `skip_analysis_targets` only contains
   regular callables. *)
val should_skip_analysis : skip_analysis_targets:HashSet.t -> t -> bool

module ArtificialTargets : sig
  val format_string : t

  val str_add : t

  val str_mod : t

  val str_format : t

  val str_literal : t

  val condition : t
end

module SharedMemoryKey : sig
  type nonrec t = t

  val compare : t -> t -> int

  val to_string : t -> string

  val from_string : string -> t
end

(* Represent a hashset of targets inside the shared memory *)
module HashsetSharedMemory : sig
  type target = t

  type t

  val cleanup : clean_old:bool -> t -> unit

  val from_heap : target list -> t

  module ReadOnly : sig
    type t

    val mem : t -> target -> bool
  end

  val read_only : t -> ReadOnly.t
end
