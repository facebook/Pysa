(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module CamlUnix = Unix
open Core
open Analysis

module ScratchPyreflyProject = struct
  type t = {
    api: Interprocedural.PyreflyApi.ReadWrite.t;
    configuration: Configuration.Analysis.t;
  }

  let find_pyrefly_binary () =
    match Stdlib.Sys.getenv_opt "PYREFLY_BINARY" with
    | Some path when not (String.equal path "") -> path
    | _ ->
        let pyre_root = Test.find_pyre_source_code_root () in
        let binary = PyrePath.create_relative ~root:pyre_root ~relative:"source/pyrefly.exe" in
        if Stdlib.Sys.file_exists (PyrePath.absolute binary) then
          PyrePath.absolute binary
        else
          failwith
            "Could not find source/pyrefly.exe; run ./facebook/scripts/setup.sh --local (Meta) or \
             ./scripts/setup.sh --local (OSS) before 'make'. Alternatively, set the PYREFLY_BINARY \
             environment variable."


  let setup
      ~context
      ~pyrefly_binary
      ~requires_type_of_expressions
      ~python_version
      ?(external_sources = [])
      ?search_paths
      sources
    =
    let local_root = Test.bracket_tmpdir context |> PyrePath.create_absolute in
    let external_root = Test.bracket_tmpdir context |> PyrePath.create_absolute in
    let add_source ~root (relative, content) =
      let content = Test.trim_extra_indentation content in
      let file = File.create ~content (PyrePath.create_relative ~root ~relative) in
      File.write file
    in
    let external_sources =
      Test.django_stubs ()
      @ Test.pysa_stubs ()
      @ Test.pytest_stubs ()
      @ Test.pyre_extensions_stubs ()
      @ external_sources
    in
    let () = List.iter sources ~f:(add_source ~root:local_root) in
    let () = List.iter external_sources ~f:(add_source ~root:external_root) in
    let () =
      File.write
        (File.create
           ~content:"\n"
           (PyrePath.create_relative ~root:local_root ~relative:"pyrefly.toml"))
    in
    let result_directory = Test.bracket_tmpdir context |> PyrePath.create_absolute in
    let extra_search_paths =
      match search_paths with
      | None -> []
      | Some search_paths ->
          List.map search_paths ~f:(fun search_path ->
              PyrePath.create_relative ~root:local_root ~relative:search_path
              |> Format.asprintf "--search-path=%a" PyrePath.pp)
    in
    let arguments =
      [
        "check";
        "--threads=1";
        "--verbose";
        Format.sprintf
          "--python-version=%d.%d.%d"
          python_version.Configuration.PythonVersion.major
          python_version.minor
          python_version.micro;
        Format.asprintf "--search-path=%a" PyrePath.pp external_root;
      ]
      @ extra_search_paths
      @ [
          Format.asprintf "--report-pysa=%a" PyrePath.pp result_directory;
          "--report-pysa-format=capnp";
          PyrePath.absolute local_root;
        ]
    in
    Log.info "Running command: %s" (Stdlib.Filename.quote_command pyrefly_binary arguments);
    let stdout_channel, stdin_channel, stderr_channel =
      CamlUnix.open_process_args_full
        pyrefly_binary
        (Array.of_list ("pyrefly" :: arguments))
        (CamlUnix.environment ())
    in
    let () = Out_channel.close stdin_channel in
    let stdout_content = In_channel.input_all stdout_channel in
    let stderr_content = In_channel.input_all stderr_channel in
    let () = In_channel.close stdout_channel in
    let () = In_channel.close stderr_channel in
    let configuration =
      Configuration.Analysis.create
        ~parallel:false
        ~local_root
        ~source_paths:[]
        ~search_paths:[]
        ~python_version
        ~enable_type_comments:false
        ()
    in
    let api =
      try
        Interprocedural.PyreflyApi.ReadWrite.create_from_directory
          ~scheduler:(Scheduler.create_sequential ())
          ~scheduler_policies:Configuration.SchedulerPolicies.empty
          ~configuration
          result_directory
      with
      | Interprocedural.PyreflyApi.PyreflyFileFormatError { path; error } ->
          Log.dump "Pyrefly stdout: %s" stdout_content;
          Log.dump "Pyrefly stderr: %s" stderr_content;
          failwith
            (Format.asprintf "%a: %a" PyrePath.pp path Interprocedural.PyreflyApi.Error.pp error)
    in
    let api =
      if requires_type_of_expressions then
        Interprocedural.PyreflyApi.ReadWrite.parse_type_of_expressions
          api
          ~scheduler:(Scheduler.create_sequential ())
          ~scheduler_policies:Configuration.SchedulerPolicies.empty
      else
        api
    in
    let () =
      (* Clean shared memory up after the test *)
      let set_up_shared_memory _ = () in
      let tear_down_shared_memory () _ = Memory.reset_shared_memory () in
      OUnit2.bracket set_up_shared_memory tear_down_shared_memory context
    in
    { api; configuration }


  let pyre_pysa_read_only_api { api; _ } = Interprocedural.PyreflyApi.ReadOnly.of_read_write_api api

  let configuration_of { configuration; _ } = configuration
end

module ScratchPyrePysaProject : sig
  type t

  val setup
    :  context:OUnitTest.ctxt ->
    requires_type_of_expressions:bool ->
    ?use_cache:bool ->
    ?external_sources:(string * string) list ->
    ?search_paths:string list ->
    ?decorator_preprocessing_configuration:PyrePysaLogic.DecoratorPreprocessing.Configuration.t ->
    (string * string) list ->
    t

  val errors : t -> AnalysisError.Instantiated.t list

  val read_only_api : t -> Interprocedural.PyreflyApi.ReadOnly.t

  val configuration_of : t -> Configuration.Analysis.t
end = struct
  type t = {
    project: ScratchPyreflyProject.t;
    pyrefly_api: Interprocedural.PyreflyApi.ReadOnly.t;
  }

  module ProjectInputs = struct
    module T = struct
      type t = {
        requires_type_of_expressions: bool;
        decorator_preprocessing_configuration:
          PyrePysaLogic.DecoratorPreprocessing.Configuration.t option;
        external_sources: string String.Map.t;
        sources: string String.Map.t;
        search_paths: string list;
      }
      [@@deriving compare, equal, sexp]
    end

    include T
    module Map = Map.Make (T)
  end

  type project = t

  module ProjectCache : sig
    type t

    val create : unit -> t

    val try_load : t -> ProjectInputs.t -> project option

    val save : t -> inputs:ProjectInputs.t -> project:project -> unit
  end = struct
    type cache_value = {
      file_path: PyrePath.t;
      project: project;
    }

    (* Maps a ProjectInputs to a file containing the shared memory state, and its associated
       project. *)
    type t = cache_value ProjectInputs.Map.t ref

    let create () = ref ProjectInputs.Map.empty

    let try_load cache inputs =
      match Map.find !cache inputs with
      | Some { file_path; project } ->
          let timer = Timer.start () in
          Memory.initialize_for_tests ();
          Hack_parallel.Std.SharedMemory.load_table (PyrePath.absolute file_path);
          (* Note: we don't save/load the dependency table, this could be a problem for some
             tests. *)
          Log.debug "Loaded project from shared memory cache in %.3fs" (Timer.stop_in_sec timer);
          Some project
      | None -> None


    let save cache ~inputs ~project =
      let timer = Timer.start () in
      let file_path, channel = Stdlib.Filename.open_temp_file "ounit-pysa-project-" ".shm" in
      Hack_parallel.Std.SharedMemory.collect `aggressive;
      Hack_parallel.Std.SharedMemory.save_table file_path;
      cache :=
        Map.add_exn
          !cache
          ~key:inputs
          ~data:{ file_path = PyrePath.create_absolute file_path; project };
      Log.debug "Saved project to shared memory cache in %.3fs" (Timer.stop_in_sec timer);
      (* Remove the file at the end of the tests. We can't use bracket_tmpfile because the cache
         needs to be preserved between brackets. *)
      let () =
        at_exit (fun () ->
            (try Stdlib.close_out channel with
            | _ -> ());
            try Stdlib.Sys.remove file_path with
            | _ -> ())
      in
      ()
  end

  let global_cache = ProjectCache.create ()

  let pyrefly_binary = lazy (ScratchPyreflyProject.find_pyrefly_binary ())

  let setup_without_cache
      ~context
      {
        ProjectInputs.requires_type_of_expressions;
        decorator_preprocessing_configuration;
        external_sources;
        sources;
        search_paths;
      }
    =
    let timer = Timer.start () in
    let external_sources = Map.to_alist external_sources in
    let sources = Map.to_alist sources in
    let default_python_version = { Configuration.PythonVersion.major = 3; minor = 12; micro = 2 } in
    let () =
      match decorator_preprocessing_configuration with
      | Some configuration -> PyrePysaLogic.DecoratorPreprocessing.setup_preprocessing configuration
      | None -> ()
    in
    let result =
      let pyrefly_binary = Lazy.force pyrefly_binary in
      let project =
        ScratchPyreflyProject.setup
          ~context
          ~pyrefly_binary
          ~requires_type_of_expressions
          ~python_version:default_python_version
          ~external_sources
          ~search_paths
          sources
      in
      let pyrefly_api = ScratchPyreflyProject.pyre_pysa_read_only_api project in
      { project; pyrefly_api }
    in
    Log.debug "Type checked project using pyrefly in %.3fs" (Timer.stop_in_sec timer);
    result


  let setup
      ~context
      ~requires_type_of_expressions
      ?(use_cache = true)
      ?(external_sources = [])
      ?(search_paths = [])
      ?decorator_preprocessing_configuration
      sources
    =
    let inputs =
      {
        ProjectInputs.requires_type_of_expressions;
        decorator_preprocessing_configuration;
        external_sources =
          external_sources
          |> String.Map.of_alist_exn
          |> String.Map.map ~f:Test.trim_extra_indentation;
        sources =
          sources |> String.Map.of_alist_exn |> String.Map.map ~f:Test.trim_extra_indentation;
        search_paths;
      }
    in
    if not use_cache then
      setup_without_cache ~context inputs
    else
      match ProjectCache.try_load global_cache inputs with
      | Some project -> project
      | None ->
          let project = setup_without_cache ~context inputs in
          let () = ProjectCache.save global_cache ~inputs ~project in
          project


  let read_only_api { pyrefly_api; _ } = pyrefly_api

  let errors { pyrefly_api; _ } = Interprocedural.PyreflyApi.ReadOnly.parse_type_errors pyrefly_api

  let configuration_of { project; _ } = ScratchPyreflyProject.configuration_of project
end
