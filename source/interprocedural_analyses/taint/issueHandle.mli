(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Interprocedural

(* After stripping the path prefix, the callable is not necessarily a valid callable, hence we
   represent it as an arbitrary string. *)
module CanonicalCallee : sig
  type t =
    | Target of Target.t
    | Name of string
  [@@deriving compare, equal, hash, sexp, show]

  val external_name : display_api:PyreflyTypes.DisplayApi.t -> t -> string
end

(* A unique identifier that represents the first sink of an issue. *)
module Sink : sig
  type t =
    | Call of {
        callee: CanonicalCallee.t;
        callee_suffix: string option;
        index: int;
        parameter: AccessPath.Root.t;
      }
    | Global of {
        callee: CanonicalCallee.t;
        index: int;
      }
    | Return
    | LiteralStringSink of Sinks.t
    | ConditionalTestSink of Sinks.t
    | StringFormat of {
        callee: CanonicalCallee.t;
        index: int;
        parameter_index: int;
      }
  [@@deriving compare, equal, hash, sexp, show]

  val make_call
    :  call_target:CallGraph.CallTarget.t ->
    root:AccessPath.Root.t ->
    callee_as_name:Ast.Expression.Name.t option ->
    t

  val make_global : call_target:CallGraph.CallTarget.t -> t

  val to_json : display_api:PyreflyTypes.DisplayApi.t -> t -> Yojson.Safe.t
end

type t = {
  code: int;
  callable: CanonicalCallee.t;
  sink: Sink.t;
}
[@@deriving compare, equal, show]

(* Strip the target parameters from callables. Must be used with `CanonicalCallee.Target`
   variants. *)
val strip_all_callable_parameters : t -> t

(* Remove the source path prefix - that we sometimes add to callable names to disambiguate
   conflicting module names - so that issue handles remain stable across runs. Must be used with the
   `CanonicalCallee.Target` variants and produces the `CanonicalCallee.Name` variants, since the
   produced callables names might not be valid callables. *)
val strip_callable_path_prefix : display_api:PyreflyTypes.DisplayApi.t -> t -> t

val deterministic_compare : display_api:PyreflyTypes.DisplayApi.t -> t -> t -> int

val master_handle : display_api:PyreflyTypes.DisplayApi.t -> t -> string

val name : string

module SerializableMap : Data_structures.SerializableMap.S with type key = t

module Set : Stdlib.Set.S with type elt = t
