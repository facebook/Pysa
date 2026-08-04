(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Ast

type class_id = PyreflyTypes.ClassId.t

module ClassIdSet : Stdlib.Set.S with type elt = class_id

module ClassIdMap : sig
  include Stdlib.Map.S with type key = class_id

  val show : pp_value:(Format.formatter -> 'a -> unit) -> 'a t -> string
end

(** Graph of root classes and their children, stored in the ocaml heap. *)
module Heap : sig
  type t [@@deriving equal]

  val empty : t

  (* Return the immediate children *)
  val children : t -> class_id -> ClassIdSet.t

  (* Add an edge in the graph *)
  val add : t -> parent:class_id -> child:class_id -> t

  val show : t -> string

  (* Traverse a module to collect the class hierarchy edges it contributes. *)
  val from_module : pyrefly_api:PyreflyApi.ReadOnly.t -> module_id:PyreflyTypes.ModuleId.t -> t

  (* Convenience wrapper around `from_module`, keyed on a module qualifier. *)
  val from_qualifier : pyrefly_api:PyreflyApi.ReadOnly.t -> qualifier:Reference.t -> t

  val create : roots:class_id list -> edges:(class_id * class_id list) list -> t

  val roots : t -> ClassIdSet.t

  val join : t -> t -> t

  val from_modules
    :  scheduler:Scheduler.t ->
    scheduler_policies:Configuration.SchedulerPolicies.t ->
    pyrefly_api:PyreflyApi.ReadOnly.t ->
    module_ids:PyreflyTypes.ModuleId.t list ->
    t

  (* Convenience wrapper around `from_modules`, keyed on module qualifiers. *)
  val from_qualifiers
    :  scheduler:Scheduler.t ->
    scheduler_policies:Configuration.SchedulerPolicies.t ->
    pyrefly_api:PyreflyApi.ReadOnly.t ->
    qualifiers:Reference.t list ->
    t
end

(** Mapping from a class id to the set of its direct children, stored in shared memory. *)
module SharedMemory : sig
  type t

  val from_heap : store_transitive_children_for:class_id list -> Heap.t -> t

  val get : t -> class_id:class_id -> ClassIdSet.t

  (* Returns the set of transitive children, or `None` if we did not pre-compute it. *)
  val get_transitive : t -> class_id:class_id -> ClassIdSet.t option
end
