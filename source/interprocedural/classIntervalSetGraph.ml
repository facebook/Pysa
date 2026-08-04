(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* ClassIntervalSetGraph: represents a mapping from a class id to its class
 * interval set.
 *
 * This can be used as a traditional ocaml value using the `Heap` module, and
 * stored in shared memory using the `SharedMemory` module.
 *)

module HackSharedMemory = Hack_parallel.Std.SharedMemory
module ClassId = PyreflyTypes.ClassId
open ClassHierarchyGraph

type dfs_state =
  | Grey
  | Black

(** Mapping from a class id to its class interval set, stored in the ocaml heap. *)
module Heap = struct
  type t = ClassIntervalSet.t ClassHierarchyGraph.ClassIdMap.t

  let from_class_hierarchy class_hierarchy =
    let roots = ClassHierarchyGraph.Heap.roots class_hierarchy in
    let add_direct_cross_edge ~from_ ~to_ cross_edges =
      let update = function
        | Some nodes -> Some (ClassIdSet.add to_ nodes)
        | None -> Some (ClassIdSet.singleton to_)
      in
      ClassIdMap.update from_ update cross_edges
    in
    let add_indirect_cross_edge ~from_ ~to_ cross_edges =
      match ClassIdMap.find_opt to_ cross_edges with
      | None -> cross_edges
      | Some nodes ->
          let update = function
            | Some original_nodes -> Some (ClassIdSet.union nodes original_nodes)
            | None -> Some nodes
          in
          ClassIdMap.update from_ update cross_edges
    in

    (* To compute cross edges, the key observation is that, during the DFS, if a node is black, then
     *  its associated cross edges are final (i.e., we cannot discover any new indirect or direct edges
     *  that originate from that node) *)
    let rec depth_first_search node (intervals, states, cross_edges, time) =
      let time = time + 1 in
      let start = time in
      let states = ClassIdMap.add node Grey states in
      let intervals, states, cross_edges, time =
        let visit_child child ((intervals, states, cross_edges, time) as accumulator) =
          match ClassIdMap.find_opt child states with
          | None ->
              let intervals, states, cross_edges, time = depth_first_search child accumulator in
              (* Now the child is black *)
              let cross_edges = add_indirect_cross_edge ~from_:node ~to_:child cross_edges in
              intervals, states, cross_edges, time
          | Some Grey ->
              failwith
                (Format.asprintf
                   "Found a back edge from %a to %a in the class hierarchy"
                   ClassId.pp
                   node
                   ClassId.pp
                   child)
          | Some Black ->
              let cross_edges =
                cross_edges
                |> add_direct_cross_edge ~from_:node ~to_:child
                (* All cross edges of a black node are indirect cross edges *)
                |> add_indirect_cross_edge ~from_:node ~to_:child
              in
              intervals, states, cross_edges, time
        in
        ClassIdSet.fold
          visit_child
          (Heap.children class_hierarchy node)
          (intervals, states, cross_edges, time)
      in
      let time = time + 1 in
      let finish = time in
      let intervals = ClassIdMap.add node (ClassInterval.create start finish) intervals in
      intervals, ClassIdMap.add node Black states, cross_edges, time
    in
    let intervals, _, cross_edges, _ =
      ClassIdSet.fold
        depth_first_search
        roots
        (ClassIdMap.empty, ClassIdMap.empty, ClassIdMap.empty, 0)
    in
    let join_intervals_from nodes interval =
      ClassIdSet.fold
        (fun node accumulator ->
          match ClassIdMap.find_opt node intervals with
          | None -> failwith (Format.asprintf "Node %a should have an interval" ClassId.pp node)
          | Some child_interval -> child_interval :: accumulator)
        nodes
        [interval]
      |> ClassIntervalSet.of_list
    in
    let add_interval_with_cross_edges node interval accumulator =
      let interval =
        match ClassIdMap.find_opt node cross_edges with
        | None -> ClassIntervalSet.of_list [interval]
        | Some nodes -> join_intervals_from nodes interval
      in
      ClassIdMap.add node interval accumulator
    in
    ClassIdMap.fold add_interval_with_cross_edges intervals ClassIdMap.empty
end

(** Mapping from a class id to its class interval set, stored in shared memory. *)
module SharedMemory = struct
  module ClassIdKey = struct
    type t = ClassId.t

    let to_string class_id = ClassId.to_int class_id |> Int.to_string

    let compare = ClassId.compare
  end

  module Internal =
    HackSharedMemory.FirstClass.WithCache.Make
      (ClassIdKey)
      (struct
        type t = ClassIntervalSet.t

        let prefix = Hack_parallel.Std.Prefix.make ()

        let description = "class intervals of classes"
      end)

  type t = Internal.t

  let create = Internal.create

  let add handle ~class_id ~interval = Internal.add handle class_id interval

  let get handle ~class_id = Internal.get handle class_id

  let from_heap intervals =
    let handle = create () in
    let () = ClassIdMap.iter (fun class_id interval -> add handle ~class_id ~interval) intervals in
    handle


  (* Resolve a class id to its interval, or `top` if the class has no recorded interval. *)
  let of_class handle class_id = get handle ~class_id |> Option.value ~default:ClassIntervalSet.top

  let of_definition handle pyrefly_api callable_id =
    if not (PyreflyTypes.CallableId.is_decorated callable_id) then
      match PyreflyApi.ReadOnly.Target.get_method_class_id pyrefly_api callable_id with
      | Some class_id
        when not
               (PyreflyApi.ReadOnly.Target.get_callable_metadata pyrefly_api callable_id)
                 .PyreflyApi.CallableMetadata.is_staticmethod ->
          (* Note that we also return the interval of the class for class methods, since the same
             logic applies between instance and class methods. *)
          of_class handle class_id
      | _ -> ClassIntervalSet.top
    else
      ClassIntervalSet.top


  let cleanup handle intervals =
    intervals
    |> ClassIdMap.bindings
    |> List.map fst
    |> Internal.KeySet.of_list
    |> Internal.remove_batch handle
end
