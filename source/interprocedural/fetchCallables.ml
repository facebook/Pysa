(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* FetchCallables: implements the logic that discovers all functions and methods
 * to analyze, and categorize them.
 *)

open Core

type t = {
  (* All callables:
   * - With an explicit definition (i.e, existing `def <name>():`)
   * - That are not stubs (i.e, NOT `def <name>(): ...`)
   * - That are in files within the source paths (as opposed to being in the
   * search path).
   *)
  internals: Target.t list;
  (* All non-stub callables with a definition. *)
  definitions: Target.t list;
  (* All stub callables. *)
  stubs: Target.t list;
}

let empty = { internals = []; definitions = []; stubs = [] }

let join left right =
  {
    internals = List.rev_append right.internals left.internals;
    definitions = List.rev_append right.definitions left.definitions;
    stubs = List.rev_append right.stubs left.stubs;
  }


let from_module ~pyrefly_api ~module_id =
  let callable_ids =
    PyreflyApi.ReadOnly.get_callable_ids_for_module pyrefly_api ~exclude_test_modules:true module_id
  in
  let is_stub_module = PyreflyApi.ReadOnly.is_stub_module pyrefly_api module_id in
  let is_internal = PyreflyApi.ReadOnly.is_internal_module pyrefly_api module_id in
  let add_target result callable_id =
    let target =
      PyreflyApi.ReadOnly.Target.target_from_callable_id pyrefly_api ~override:false callable_id
    in
    let { PyreflyApi.CallableMetadata.is_toplevel; is_class_toplevel; _ } =
      PyreflyApi.ReadOnly.Target.get_callable_metadata pyrefly_api callable_id
    in
    let is_stub_like = PyreflyApi.ReadOnly.is_stub_like_callable pyrefly_api callable_id in
    (* Note: when changing this, also change `PyreflyApi.ReadOnly.parse_call_graphs` *)
    if is_stub_module && (is_toplevel || is_class_toplevel) then
      (* Ignore top level define for stub modules (i.e, `.pyi`) *)
      result
    else if is_stub_like then
      { result with stubs = target :: result.stubs }
    else if is_internal then
      {
        result with
        internals = target :: result.internals;
        definitions = target :: result.definitions;
      }
    else
      { result with definitions = target :: result.definitions }
  in
  List.fold callable_ids ~init:empty ~f:add_target


(* Convenience wrapper used by tests, which naturally have a module qualifier in hand. *)
let from_qualifier ~pyrefly_api ~qualifier =
  from_module
    ~pyrefly_api
    ~module_id:(PyreflyApi.ReadOnly.module_id_of_qualifier pyrefly_api qualifier)


let from_modules ~scheduler ~scheduler_policy ~pyrefly_api ~configuration:_ ~module_ids =
  let map module_ids =
    let callables_of_module callables module_id =
      from_module ~pyrefly_api ~module_id |> join callables
    in
    List.fold module_ids ~f:callables_of_module ~init:empty
  in
  Scheduler.map_reduce
    scheduler
    ~policy:scheduler_policy
    ~map
    ~reduce:join
    ~initial:empty
    ~inputs:module_ids
    ()


let get_internal_definitions { internals; _ } = internals

let get_definitions { definitions; _ } = definitions

let get_stubs { stubs; _ } = stubs

let get { definitions; stubs; _ } ~definitions:include_definitions ~stubs:include_stubs =
  let targets =
    if include_definitions then
      definitions
    else
      []
  in
  let targets =
    if include_stubs then
      List.rev_append stubs targets
    else
      targets
  in
  targets


let get_stats { internals; definitions; stubs } =
  [
    "definitions", List.length definitions;
    "internals", List.length internals;
    "stubs", List.length stubs;
  ]
