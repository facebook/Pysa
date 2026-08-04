(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* ClassHierarchyGraph: defines a class hierarchy graph. This can be used to
 * find all classes deriving from a given class. *)

open Core
module ClassId = PyreflyTypes.ClassId

type class_id = ClassId.t

module ClassIdSet = Stdlib.Set.Make (struct
  type t = ClassId.t

  let compare = ClassId.compare
end)

module ClassIdMap = struct
  include Stdlib.Map.Make (struct
    type t = ClassId.t

    let compare = ClassId.compare
  end)

  let pp_map pp_value formatter map =
    let pp_pairs pp_value formatter pairs =
      let pp_pair formatter key value =
        Format.fprintf formatter "@,%a -> [%a]" ClassId.pp key pp_value value
      in
      iter (pp_pair formatter) pairs
    in
    Format.fprintf formatter "{@[<v 2>%a@]@,}" (pp_pairs pp_value) map


  let show ~pp_value map = Format.asprintf "%a" (pp_map pp_value) map
end

(** Graph of root classes and their children, stored in the ocaml heap. *)
module Heap = struct
  type t = {
    roots: ClassIdSet.t;
    edges: ClassIdSet.t ClassIdMap.t;
  }
  [@@deriving equal]

  let pp_set formatter set =
    ClassIdSet.iter (fun element -> Format.fprintf formatter "@[%a,@]" ClassId.pp element) set


  let pp formatter { roots; edges } =
    Format.fprintf
      formatter
      "roots: @[[%a]@]@\nedges: {@[<v 2>%a@]@,}"
      pp_set
      roots
      (ClassIdMap.pp_map pp_set)
      edges


  let show = Format.asprintf "%a" pp

  let empty = { roots = ClassIdSet.empty; edges = ClassIdMap.empty }

  let set_of_children = function
    | None -> ClassIdSet.empty
    | Some children -> children


  (* Return the immediate children *)
  let children { edges; _ } parent = set_of_children (ClassIdMap.find_opt parent edges)

  (* Add an edge in the graph *)
  let add { roots; edges } ~parent:parent_class ~child:child_class =
    let new_roots =
      let new_roots =
        if ClassIdMap.mem parent_class edges then
          roots
        else
          ClassIdSet.add parent_class roots
      in
      ClassIdSet.remove child_class new_roots
    in
    let new_edges =
      let add_child parent = Some (ClassIdSet.add child_class (set_of_children parent)) in
      let update_children ~parent ~update edges = ClassIdMap.update parent update edges in
      edges
      |> update_children ~parent:parent_class ~update:add_child
      |> update_children ~parent:child_class ~update:(fun key -> Some (set_of_children key))
    in
    { roots = new_roots; edges = new_edges }


  let roots { roots; _ } = roots

  let edges_from_module ~pyrefly_api ~module_id =
    PyreflyApi.ReadOnly.get_class_ids_for_module pyrefly_api ~exclude_test_modules:true module_id
    |> List.concat_map ~f:(fun class_id ->
           let parents = PyreflyApi.ReadOnly.class_immediate_parents pyrefly_api class_id in
           List.map parents ~f:(fun parent -> parent, class_id))


  let from_module ~pyrefly_api ~module_id =
    edges_from_module ~pyrefly_api ~module_id
    |> List.fold ~init:empty ~f:(fun accumulator (parent, child) -> add accumulator ~parent ~child)


  (* Convenience wrapper used by tests, which naturally have a module qualifier in hand. *)
  let from_qualifier ~pyrefly_api ~qualifier =
    from_module
      ~pyrefly_api
      ~module_id:(PyreflyApi.ReadOnly.module_id_of_qualifier pyrefly_api qualifier)


  let create ~roots ~edges =
    let roots = ClassIdSet.of_list roots in
    let edges =
      List.fold edges ~init:ClassIdMap.empty ~f:(fun accumulator (parent, children) ->
          ClassIdMap.add parent (ClassIdSet.of_list children) accumulator)
    in
    { roots; edges }


  let join ({ roots = _; edges = edges_left } as left) { roots = roots_right; edges = edges_right } =
    let add_edges parent_right children_right { roots; edges } =
      if ClassIdSet.is_empty children_right && not (ClassIdMap.mem parent_right edges) then
        let edges = ClassIdMap.add parent_right ClassIdSet.empty edges in
        { roots; edges }
      else
        let update = function
          | Some children_left -> Some (ClassIdSet.union children_left children_right)
          | None -> Some children_right
        in
        let edges = ClassIdMap.update parent_right update edges in
        (* Remove roots that now have incoming edges *)
        let roots = ClassIdSet.diff roots children_right in
        { roots; edges }
    in
    let { roots; edges } = ClassIdMap.fold add_edges edges_right left in
    let add_root root_right accumulator =
      if ClassIdMap.mem root_right edges_left then
        accumulator
      else
        ClassIdSet.add root_right accumulator
    in
    let roots = ClassIdSet.fold add_root roots_right roots in
    { roots; edges }


  let from_modules ~scheduler ~scheduler_policies ~pyrefly_api ~module_ids =
    (* We collect (parent, child) class-id pairs in parallel workers, then build the graph
       sequentially. This is much faster than building Heap.t graphs in workers and reducing with
       `join`, because serializing large ClassIdMap/ClassIdSet structures across process boundaries
       is very expensive. *)
    let collect_edges module_ids =
      List.concat_map module_ids ~f:(fun module_id -> edges_from_module ~pyrefly_api ~module_id)
    in
    let scheduler_policy =
      Scheduler.Policy.from_configuration_or_default
        scheduler_policies
        Configuration.ScheduleIdentifier.ClassHierarchyGraph
        ~default:
          (Scheduler.Policy.fixed_chunk_count
             ~minimum_chunks_per_worker:1
             ~minimum_chunk_size:1
             ~preferred_chunks_per_worker:4
             ())
    in
    let edges =
      Scheduler.map_reduce
        scheduler
        ~policy:scheduler_policy
        ~initial:[]
        ~map:collect_edges
        ~reduce:List.rev_append
        ~inputs:module_ids
        ()
    in
    List.fold edges ~init:empty ~f:(fun accumulator (parent, child) ->
        add accumulator ~parent ~child)


  (* Convenience wrapper around `from_modules` used by tests, which naturally have module qualifiers
     in hand. *)
  let from_qualifiers ~scheduler ~scheduler_policies ~pyrefly_api ~qualifiers =
    from_modules
      ~scheduler
      ~scheduler_policies
      ~pyrefly_api
      ~module_ids:(List.map qualifiers ~f:(PyreflyApi.ReadOnly.module_id_of_qualifier pyrefly_api))
end

(** Mapping from a class id to the set of its direct children, stored in shared memory. *)
module SharedMemory = struct
  module ClassIdKey = struct
    type t = ClassId.t

    let to_string class_id = ClassId.to_int class_id |> Int.to_string

    let compare = ClassId.compare
  end

  module DirectChildren =
    Memory.WithCache.Make
      (ClassIdKey)
      (struct
        type t = ClassIdSet.t

        let prefix = Hack_parallel.Std.Prefix.make ()

        let description = "direct children of classes"
      end)

  module TransitiveChildren =
    Memory.WithCache.Make
      (ClassIdKey)
      (struct
        type t = ClassIdSet.t

        let prefix = Hack_parallel.Std.Prefix.make ()

        let description = "transitive children of classes"
      end)

  type t = Handle

  let add Handle ~class_id ~direct_children = DirectChildren.add class_id direct_children

  let get Handle ~class_id = DirectChildren.get class_id |> Option.value ~default:ClassIdSet.empty

  let rec add_children ~class_hierarchy_graph =
    ClassIdSet.iter (fun class_id ->
        let direct_children = Heap.children class_hierarchy_graph class_id in
        add Handle ~class_id ~direct_children;
        add_children ~class_hierarchy_graph direct_children)


  let add_transitive Handle ~class_id ~transitive_children =
    TransitiveChildren.add class_id transitive_children


  let get_transitive Handle ~class_id = TransitiveChildren.get class_id

  let rec find_children_transitive ~class_hierarchy_graph to_process result =
    match to_process with
    | [] -> result
    | class_id :: rest ->
        let child_id_set = Heap.children class_hierarchy_graph class_id in
        let new_children = ClassIdSet.elements child_id_set in
        let result = List.fold ~f:(Fn.flip ClassIdSet.add) ~init:result new_children in
        find_children_transitive ~class_hierarchy_graph (List.rev_append new_children rest) result


  let add_children_transitive ~class_hierarchy_graph =
    List.iter ~f:(fun class_id ->
        let transitive_children =
          find_children_transitive ~class_hierarchy_graph [class_id] ClassIdSet.empty
        in
        add_transitive Handle ~class_id ~transitive_children)


  let from_heap ~store_transitive_children_for class_hierarchy_graph =
    let () = add_children ~class_hierarchy_graph (Heap.roots class_hierarchy_graph) in
    let () = add_children_transitive ~class_hierarchy_graph store_transitive_children_for in
    Handle
end
