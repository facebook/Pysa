(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* OverrideGraph: represents a mapping from a method to the methods overriding it.
 *
 * This can be used as a traditional ocaml value using the `Heap` module, and
 * stored in shared memory using the `SharedMemory` module.
 *)

open Core
open Pyre
open Ast

(** Override graph in the ocaml heap, mapping each member method to the list of the `CallableId`s of
    its overriding methods, resolved to real callables at build time. Consumers wrap each stored id
    into an `Override` target at the use site. *)
module Heap = struct
  type t = PyreflyTypes.CallableId.t list Target.Map.Tree.t

  let empty = Target.Map.Tree.empty

  let of_alist_exn = Target.Map.Tree.of_alist_exn

  let fold graph ~init ~f =
    Target.Map.Tree.fold graph ~init ~f:(fun ~key:member ~data:overrides -> f ~member ~overrides)


  let equal left right = Target.Map.Tree.equal (List.equal PyreflyTypes.CallableId.equal) left right

  let pp formatter overrides =
    let pp_pair formatter (member, overrides) =
      Format.fprintf
        formatter
        "@,%a -> %s"
        Target.pp_internal
        member
        (List.map ~f:PyreflyTypes.CallableId.show overrides |> String.concat ~sep:", ")
    in
    let pp_pairs formatter = List.iter ~f:(pp_pair formatter) in
    Format.fprintf formatter "{@[<v 2>%a@]@,}" pp_pairs (Target.Map.Tree.to_alist overrides)


  let show = Format.asprintf "%a" pp

  module OverridingRelation = struct
    (* Represent a relation where `base_callable` is overridden by `override_callable_id`, the
       `CallableId` of a real method with the same name defined in an overriding subclass. Consumers
       wrap it into an `Override` target. *)
    type t = {
      base_callable: Target.t;
      override_callable_id: PyreflyTypes.CallableId.t;
    }

    let from_method_id ~pyrefly_api method_id =
      PyreflyApi.ReadOnly.Target.get_overriden_base_method pyrefly_api method_id
      >>= fun base_callable ->
      Some { base_callable = Target.create_method base_callable; override_callable_id = method_id }
  end

  let from_overriding_relations relations =
    let accumulate map { OverridingRelation.base_callable; override_callable_id } =
      let update_overrides = function
        | Some overrides -> override_callable_id :: overrides
        | None -> [override_callable_id]
      in
      Target.Map.Tree.update map base_callable ~f:update_overrides
    in
    relations
    |> List.fold ~init:Target.Map.Tree.empty ~f:accumulate
    |> Target.Map.Tree.map ~f:(List.dedup_and_sort ~compare:PyreflyTypes.CallableId.compare)


  let skip_overrides ~pyrefly_api ~to_skip overrides =
    Target.Map.Tree.filter_keys
      ~f:(fun override ->
        to_skip
        |> Reference.SerializableSet.mem
             (override
             |> Target.get_regular
             |> Target.Regular.override_to_method
             |> Target.from_regular
             |> PyreflyApi.ReadOnly.Target.define_name_exn pyrefly_api)
        |> Core.not)
      overrides


  let from_module ~pyrefly_api ~skip_overrides_targets module_id =
    PyreflyApi.ReadOnly.get_methods_for_module ~exclude_test_modules:true pyrefly_api module_id
    |> List.filter_map ~f:(OverridingRelation.from_method_id ~pyrefly_api)
    |> from_overriding_relations
    |> skip_overrides ~pyrefly_api ~to_skip:skip_overrides_targets


  (* Convenience wrapper used by tests, which naturally have a module qualifier in hand. *)
  let from_qualifier ~pyrefly_api ~skip_overrides_targets qualifier =
    from_module
      ~pyrefly_api
      ~skip_overrides_targets
      (PyreflyApi.ReadOnly.module_id_of_qualifier pyrefly_api qualifier)


  type cap_overrides_result = {
    overrides: t;
    skipped_overrides: Target.t list;
  }

  (** If a method has too many overrides, ignore them. *)
  let cap_overrides ~pyrefly_api ~analyze_all_overrides_targets ~maximum_overrides overrides =
    (* Keep the information of whether we're skipping overrides in a ref that we accumulate while we
       filter the map. *)
    let skipped_overrides = ref [] in
    let keep_override_edge ~key:member ~data:overriding_methods =
      if Target.Set.mem member analyze_all_overrides_targets then
        let () =
          Log.info
            "Analyzing all overrides of `%s` as per @AnalyzeAllOverrides"
            (PyreflyApi.ReadOnly.Target.external_name ~pyrefly_api member)
        in
        true
      else
        let number_of_overrides = List.length overriding_methods in
        match maximum_overrides with
        | Some cap ->
            if number_of_overrides < cap then
              true
            else begin
              Log.log
                ~section:`SkippedOverride
                "Omitting overrides for `%s`. The number of overrides %d exceeds the limit %d."
                (PyreflyApi.ReadOnly.Target.external_name ~pyrefly_api member)
                number_of_overrides
                cap;
              skipped_overrides := member :: !skipped_overrides;
              false
            end
        | None ->
            if number_of_overrides > 50 then
              Log.warning
                "`%s` has %d overrides, this might slow down the analysis considerably."
                (PyreflyApi.ReadOnly.Target.external_name ~pyrefly_api member)
                number_of_overrides;
            true
    in
    let overrides = Target.Map.Tree.filteri overrides ~f:keep_override_edge in
    { overrides; skipped_overrides = !skipped_overrides }
end

(** Override graph in the shared memory, mapping each member method to the list of the `CallableId`s
    of its overriding methods. *)
module SharedMemory = struct
  module T =
    SaveLoadSharedMemory.MakeKeyValue
      (Target.SharedMemoryKey)
      (struct
        type t = PyreflyTypes.CallableId.t list

        let prefix = Hack_parallel.Std.Prefix.make ()

        let handle_prefix = Hack_parallel.Std.Prefix.make ()

        let description = "override methods"
      end)

  type t = T.t

  let create = T.create

  (** Records a heap override graph in shared memory. *)
  let from_heap overrides = overrides |> Target.Map.Tree.to_alist |> T.of_alist_sequential

  let to_heap handle = handle |> T.to_alist |> Target.Map.Tree.of_alist_exn

  (** Remove an override graph from shared memory. This must be called before storing another
      override graph. *)
  let cleanup = T.cleanup

  let read_only = T.read_only

  module ReadOnly = struct
    type t = T.ReadOnly.t

    let get_override_targets handle ~member = T.ReadOnly.get handle ~cache:true member

    let overrides_exist handle member = T.ReadOnly.mem handle member

    let expand_override_targets handle callees =
      let rec expand_and_gather expanded target =
        if not (Target.is_override target) then
          target :: expanded
        else
          let corresponding_method =
            (* In the override graph, keys can only be `Target.Regular.Method` and hence not
               `Target.Parameterized`. *)
            Target.get_corresponding_method_exn ~must_be_regular:false target
          in
          let overrides =
            handle
            |> get_override_targets ~member:corresponding_method
            |> Option.value ~default:[]
            |> List.map ~f:(fun callable_id ->
                   Target.from_regular (Target.Regular.Override callable_id))
          in
          corresponding_method :: List.fold overrides ~f:expand_and_gather ~init:expanded
      in
      List.fold callees ~init:[] ~f:expand_and_gather |> List.dedup_and_sort ~compare:Target.compare
  end

  let save_to_cache = T.save_to_cache

  let load_from_cache = T.load_from_cache
end

type skipped_overrides = Target.t list

type whole_program_overrides = {
  override_graph_heap: Heap.t;
  override_graph_shared_memory: SharedMemory.t;
  skipped_overrides: skipped_overrides;
}

(** Compute the override graph, which maps overide_targets (parent methods which are overridden) to
    all concrete methods overriding them, and save it to shared memory. *)
let build_whole_program_overrides
    ~scheduler
    ~static_analysis_configuration:
      ({ Configuration.StaticAnalysis.scheduler_policies; _ } as static_analysis_configuration)
    ~pyrefly_api
    ~skip_overrides_targets
    ~maximum_overrides
    ~analyze_all_overrides_targets
    ~module_ids
  =
  let overrides =
    let combine ~key:_ left right = List.rev_append left right in
    let build_overrides overrides module_id =
      module_id
      |> Heap.from_module ~pyrefly_api ~skip_overrides_targets
      |> Target.Map.Tree.merge_skewed ~combine overrides
    in
    let scheduler_policy =
      Scheduler.Policy.from_configuration_or_default
        scheduler_policies
        Configuration.ScheduleIdentifier.OverrideGraph
        ~default:
          (Scheduler.Policy.fixed_chunk_count
             ~minimum_chunks_per_worker:1
             ~minimum_chunk_size:1
             ~preferred_chunks_per_worker:1
             ())
    in
    Scheduler.map_reduce
      scheduler
      ~policy:scheduler_policy
      ~initial:Heap.empty
      ~map:(fun module_ids -> List.fold module_ids ~init:Heap.empty ~f:build_overrides)
      ~reduce:(Target.Map.Tree.merge_skewed ~combine)
      ~inputs:module_ids
      ()
  in
  let { Heap.overrides = override_graph_heap; skipped_overrides } =
    Heap.cap_overrides ~pyrefly_api ~analyze_all_overrides_targets ~maximum_overrides overrides
  in
  let override_graph_shared_memory = SharedMemory.from_heap override_graph_heap in
  let () =
    match static_analysis_configuration.Configuration.StaticAnalysis.save_results_to with
    | Some directory ->
        Log.info "Writing the override graph to `%s`" (PyrePath.absolute directory);
        let to_json_lines (member, overrides) =
          [
            {
              NewlineDelimitedJson.Line.kind = NewlineDelimitedJson.Kind.OverrideGraph;
              data =
                `Assoc
                  [
                    ( "callable",
                      `String (PyreflyApi.ReadOnly.Target.external_name ~pyrefly_api member) );
                    ( "overrides",
                      `List
                        (List.map overrides ~f:(fun callable_id ->
                             Target.create_method callable_id
                             |> PyreflyApi.ReadOnly.Target.external_name ~pyrefly_api
                             |> fun s -> `String s)) );
                  ];
            };
          ]
        in
        let local_root =
          static_analysis_configuration.Configuration.StaticAnalysis.configuration.local_root
        in
        let configuration = `Assoc ["repo", `String (PyrePath.absolute local_root)] in
        let elements = Target.Map.Tree.to_alist override_graph_heap in
        let () =
          match static_analysis_configuration.Configuration.StaticAnalysis.output_format with
          | Configuration.TaintOutputFormat.Json ->
              NewlineDelimitedJson.write_file
                ~path:(PyrePath.append directory ~element:"override-graph.json")
                ~configuration
                ~to_json_lines
                elements
          | Configuration.TaintOutputFormat.ShardedJson ->
              let filename_prefix = "override-graph" in
              NewlineDelimitedJson.remove_sharded_files ~directory ~filename_prefix;
              NewlineDelimitedJson.write_sharded_files
                ~scheduler
                ~directory
                ~filename_prefix
                ~configuration
                ~to_json_lines
                elements
        in
        ()
    | None -> ()
  in
  { override_graph_heap; override_graph_shared_memory; skipped_overrides }
