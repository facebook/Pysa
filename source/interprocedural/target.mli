(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Ast

module Regular : sig
  type t =
    | Function of PyreflyTypes.CallableId.t
    | Method of PyreflyTypes.CallableId.t
    | Override of PyreflyTypes.CallableId.t
    (* Represents a global variable or field of a class that we want to model, * e.g os.environ or
       HttpRequest.GET *)
    | Object of string
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

  val pp_pretty : Format.formatter -> t -> unit
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

val pp_internal : Format.formatter -> t -> unit

val show_internal : t -> string

val pp_pretty : Format.formatter -> t -> unit

val show_pretty : t -> string

(* Structural (api-free) pretty-printers. They render the raw packed id, not a name. Used for
   debug/logging/test-printer sites; golden-generating sites use the `*_with_display_api`
   variants. *)
val pp_pretty_with_kind : Format.formatter -> t -> unit

val show_pretty_with_kind : t -> string

(* Api-aware pretty-printers, used by golden-generating output sites so they keep rendering
   names. *)
val pp_pretty_with_display_api
  :  display_api:PyreflyTypes.DisplayApi.t ->
  Format.formatter ->
  t ->
  unit

val show_pretty_with_display_api : display_api:PyreflyTypes.DisplayApi.t -> t -> string

val pp_pretty_with_kind_with_display_api
  :  display_api:PyreflyTypes.DisplayApi.t ->
  Format.formatter ->
  t ->
  unit

val show_pretty_with_kind_with_display_api : display_api:PyreflyTypes.DisplayApi.t -> t -> string

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

(* Equivalent to pp_internal. Required by @@deriving. *)
val pp : Format.formatter -> t -> unit

(* Constructors. *)

val create_function : PyreflyTypes.CallableId.t -> t

val create_method : PyreflyTypes.CallableId.t -> t

val create_override : PyreflyTypes.CallableId.t -> t

val create_object : Reference.t -> t

val from_regular : Regular.t -> t

(* Return `Regular.t` when called on any `t`. *)
val get_regular : t -> Regular.t

(* Return `Regular.t`, but throw if called on `Parameterized`. *)
val as_regular_exn : t -> Regular.t

val strip_parameters : t -> t

val collect_nested_regular_targets : t -> Regular.t list

val get_corresponding_method_exn : must_be_regular:bool -> t -> t

(* Accessors. *)

val object_name : t -> Reference.t

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

  val pp_pretty_with_kind : Format.formatter -> t -> unit

  val show_pretty_with_kind : t -> string
end

module HashMap : Core.Hashtbl.S with type key := t

module HashSet : Core.Hash_set.S with type elt := t

module List : sig
  type t = T.t list

  val pp_pretty_with_kind : Format.formatter -> t -> unit

  val show_pretty_with_kind : t -> string
end

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
