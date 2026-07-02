(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* PyrePysaApi is a wrapper around a type checker API, which exposes source code, ASTs and type
   information about the code to analyze. This wraps the Pyrefly API provided by
   `Interprocedural.Pyrefly`. *)

open Core
module TypeModifier = Analysis.PysaTypes.TypeModifier
module ClassWithModifiers = Analysis.PysaTypes.ClassWithModifiers
module ScalarTypeProperties = Analysis.PysaTypes.ScalarTypeProperties
module ClassNamesFromType = Analysis.PysaTypes.ClassNamesFromType
module PysaType = Analysis.PysaTypes.PysaType
module PyreClassSummary = Analysis.PysaTypes.PyreClassSummary
module AstResult = Analysis.PysaTypes.AstResult
module TaintAccessPath = Analysis.TaintAccessPath

module PysaClassSummary = struct
  type t = Pyrefly of PyreflyApi.PysaClassSummary.t
end

module ReadWrite = struct
  type t = Pyrefly of PyreflyApi.ReadWrite.t

  let create_with_cold_start
      ~scheduler
      ~scheduler_policies
      ~configuration
      ~pyrefly_results
      ~decorator_configuration
    =
    (* This is required by CallableToDecoratorsMap *)
    let () = Analysis.DecoratorPreprocessing.setup_preprocessing decorator_configuration in
    Pyrefly
      (PyreflyApi.ReadWrite.create_from_directory
         ~scheduler
         ~scheduler_policies
         ~configuration
         pyrefly_results)


  let parse_type_of_expressions api ~scheduler ~scheduler_policies =
    match api with
    | Pyrefly pyrefly_api ->
        Pyrefly
          (PyreflyApi.ReadWrite.parse_type_of_expressions
             pyrefly_api
             ~scheduler
             ~scheduler_policies)
end

module ReadOnly = struct
  type t = Pyrefly of PyreflyApi.ReadOnly.t

  let of_read_write_api = function
    | ReadWrite.Pyrefly pyrefly_api -> Pyrefly (PyreflyApi.ReadOnly.of_read_write_api pyrefly_api)


  let from_pyrefly_api pyrefly_api = Pyrefly pyrefly_api

  let explicit_qualifiers = function
    | Pyrefly pyrefly_api -> PyreflyApi.ReadOnly.explicit_qualifiers pyrefly_api


  let all_sys_infos = function
    | Pyrefly pyrefly_api -> PyreflyApi.ReadOnly.all_sys_infos pyrefly_api


  let absolute_source_path_of_qualifier = function
    | Pyrefly pyrefly_api -> PyreflyApi.ReadOnly.absolute_source_path_of_qualifier pyrefly_api


  let repository_relative_path_of_qualifier ~repository_root api qualifier =
    let open Core.Option.Monad_infix in
    absolute_source_path_of_qualifier api qualifier
    >>| fun path ->
    match
      PyrePath.get_relative_to_root ~root:repository_root ~path:(PyrePath.create_absolute path)
    with
    | Some relative -> relative
    | None -> path


  let search_path_relative_path_of_qualifier api qualifier =
    match api with
    | Pyrefly pyrefly_api ->
        PyreflyApi.ReadOnly.search_path_relative_path_of_qualifier pyrefly_api qualifier


  let get_class_names_for_qualifier = function
    | Pyrefly pyrefly_api -> PyreflyApi.ReadOnly.get_class_names_for_qualifier pyrefly_api


  let get_class_summary api class_name =
    match api with
    | Pyrefly pyrefly_api ->
        PyreflyApi.ReadOnly.get_class_summary pyrefly_api class_name
        |> fun class_summary -> Some (PysaClassSummary.Pyrefly class_summary)


  let get_class_decorators_opt = function
    | Pyrefly pyrefly_api -> PyreflyApi.ReadOnly.get_class_decorators_opt pyrefly_api


  let get_class_attributes = function
    | Pyrefly pyrefly_api -> PyreflyApi.ReadOnly.get_class_attributes pyrefly_api


  let class_immediate_parents = function
    | Pyrefly pyrefly_api -> PyreflyApi.ReadOnly.class_immediate_parents pyrefly_api


  let class_mro = function
    | Pyrefly pyrefly_api -> PyreflyApi.ReadOnly.class_mro pyrefly_api


  let get_define_names_for_qualifier api ~exclude_test_modules qualifier =
    match api with
    | Pyrefly pyrefly_api ->
        PyreflyApi.ReadOnly.get_define_names_for_qualifier
          pyrefly_api
          ~exclude_test_modules
          qualifier


  let get_methods_for_qualifier api ~exclude_test_modules qualifier =
    match api with
    | Pyrefly pyrefly_api ->
        PyreflyApi.ReadOnly.get_methods_for_qualifier ~exclude_test_modules pyrefly_api qualifier


  let get_qualifier_top_level_define_name = function
    | Pyrefly pyrefly_api -> PyreflyApi.ReadOnly.get_qualifier_top_level_define_name pyrefly_api


  let get_callable_captures = function
    | Pyrefly pyrefly_api -> PyreflyApi.ReadOnly.get_callable_captures pyrefly_api


  let get_callable_return_annotations = function
    | Pyrefly pyrefly_api -> PyreflyApi.ReadOnly.get_callable_return_annotations pyrefly_api


  let get_callable_parameter_annotations = function
    | Pyrefly pyrefly_api -> PyreflyApi.ReadOnly.get_callable_parameter_annotations pyrefly_api


  let get_overriden_base_method api method_reference =
    match api with
    | Pyrefly pyrefly_api ->
        PyreflyApi.ReadOnly.get_overriden_base_method pyrefly_api method_reference


  let target_from_method_reference method_reference =
    let method_reference, is_property_setter =
      match method_reference with
      | Analysis.PysaTypes.MethodReference.Pyrefly { define_name; is_property_setter } ->
          define_name, is_property_setter
    in
    let kind = if is_property_setter then Target.PropertySetter else Target.Normal in
    Target.create_method_from_reference ~kind method_reference


  (* Turn a captured variable root into a root for the state. Used to assign user provided sources
     for captured variables at the beginning of the forward analysis. *)
  let state_root_of_captured_variable api captured_variable =
    match api with
    | Pyrefly _ ->
        (* In pyrefly, captured variable are represented with Root.CapturedVariable in the state. *)
        TaintAccessPath.Root.CapturedVariable captured_variable


  (* TODO(T225700656): Move this in the ClassSummary module *)
  let named_tuple_attributes api class_name =
    match api with
    | Pyrefly pyrefly_api -> PyreflyApi.ReadOnly.named_tuple_attributes pyrefly_api class_name


  let all_classes = function
    | Pyrefly pyrefly_api -> PyreflyApi.ReadOnly.all_classes pyrefly_api


  let ensures_qualified api source =
    match api with
    | Pyrefly _ -> Preprocessing.qualify source


  module Type = struct
    let scalar_properties = function
      | Pyrefly pyrefly_api -> PyreflyApi.ReadOnly.Type.scalar_properties pyrefly_api


    let get_class_names = function
      | Pyrefly pyrefly_api -> PyreflyApi.ReadOnly.Type.get_class_names pyrefly_api


    let is_dictionary_or_mapping = function
      | Pyrefly pyrefly_api -> PyreflyApi.ReadOnly.Type.is_dictionary_or_mapping pyrefly_api
  end

  module ClassSummary = struct
    let has_custom_new api class_summary =
      match api, class_summary with
      | Pyrefly pyrefly_api, PysaClassSummary.Pyrefly class_summary ->
          PyreflyApi.ReadOnly.ClassSummary.has_custom_new pyrefly_api class_summary


    let is_dataclass api class_summary =
      match api, class_summary with
      | Pyrefly pyrefly_api, PysaClassSummary.Pyrefly class_summary ->
          PyreflyApi.ReadOnly.ClassSummary.is_dataclass pyrefly_api class_summary


    let is_named_tuple api class_summary =
      match api, class_summary with
      | Pyrefly pyrefly_api, PysaClassSummary.Pyrefly class_summary ->
          PyreflyApi.ReadOnly.ClassSummary.is_named_tuple pyrefly_api class_summary


    let is_typed_dict api class_summary =
      match api, class_summary with
      | Pyrefly pyrefly_api, PysaClassSummary.Pyrefly class_summary ->
          PyreflyApi.ReadOnly.ClassSummary.is_typed_dict pyrefly_api class_summary


    let dataclass_ordered_attributes api class_summary =
      match api, class_summary with
      | Pyrefly pyrefly_api, PysaClassSummary.Pyrefly class_summary ->
          PyreflyApi.ReadOnly.ClassSummary.dataclass_ordered_attributes pyrefly_api class_summary


    let typed_dictionary_attributes api class_summary =
      match api, class_summary with
      | Pyrefly pyrefly_api, PysaClassSummary.Pyrefly class_summary ->
          PyreflyApi.ReadOnly.ClassSummary.typed_dictionary_attributes pyrefly_api class_summary
  end

  let add_builtins_prefix api reference =
    match api with
    | Pyrefly _ -> PyreflyApi.add_builtins_prefix reference


  (* Given a fully qualified name for a function, method, class, attribute or global variable,
     return its 'symbolic' name. This removes any path prefix and suffixes such as `@setter` and
     `$2`. *)
  let target_symbolic_name api reference =
    match api with
    | Pyrefly _ -> PyreflyApi.target_symbolic_name reference
end

module InContext = struct
  type t = Pyrefly of PyreflyApi.InContext.t

  let create_at_function_scope api ~module_qualifier ~define_name ~call_graph =
    match api with
    | ReadOnly.Pyrefly pyrefly_api ->
        Pyrefly
          (PyreflyApi.InContext.create_at_function_scope
             pyrefly_api
             ~module_qualifier
             ~define_name
             ~call_graph)


  let create_at_statement_scope api ~module_qualifier ~define_name ~call_graph ~statement_key =
    match api with
    | ReadOnly.Pyrefly pyrefly_api ->
        Pyrefly
          (PyreflyApi.InContext.create_at_statement_scope
             pyrefly_api
             ~module_qualifier
             ~define_name
             ~call_graph
             ~statement_key)


  let pyre_api = function
    | Pyrefly pyrefly_context -> ReadOnly.Pyrefly (PyreflyApi.InContext.pyre_api pyrefly_context)


  let is_global = function
    | Pyrefly pyrefly_context -> PyreflyApi.InContext.is_global pyrefly_context


  let resolve_reference = function
    | Pyrefly pyrefly_context -> PyreflyApi.InContext.resolve_reference pyrefly_context


  let resolve_assignment api assign =
    match api with
    | Pyrefly pyrefly_context ->
        Pyrefly (PyreflyApi.InContext.resolve_assignment pyrefly_context assign)


  let resolve_expression_to_type = function
    | Pyrefly pyrefly_context -> PyreflyApi.InContext.resolve_expression_to_type pyrefly_context


  let resolve_attribute_access = function
    | Pyrefly pyrefly_context -> PyreflyApi.InContext.resolve_attribute_access pyrefly_context


  let fallback_attribute = function
    | Pyrefly pyrefly_context -> PyreflyApi.InContext.fallback_attribute pyrefly_context


  let resolve_generators api generators =
    match api with
    | Pyrefly pyrefly_context ->
        Pyrefly (PyreflyApi.InContext.resolve_generators pyrefly_context generators)


  let module_qualifier = function
    | Pyrefly pyrefly_context -> PyreflyApi.InContext.module_qualifier pyrefly_context


  let define_name = function
    | Pyrefly pyrefly_context -> PyreflyApi.InContext.define_name pyrefly_context


  let root_of_identifier api ~location ~identifier =
    match api with
    | Pyrefly pyrefly_context ->
        PyreflyApi.InContext.root_of_identifier pyrefly_context ~location ~identifier


  let access_path_of_expression api ~self_variable expression =
    match api with
    | Pyrefly pyrefly_context ->
        TaintAccessPath.of_expression
          ~root_of_identifier:(PyreflyApi.InContext.root_of_identifier pyrefly_context)
          ~self_variable
          expression


  (* Propagate a captured variable from a callee to a caller. Return the new root representing that
     variable in the caller. *)
  let propagate_captured_variable api = function
    | TaintAccessPath.CapturedVariable.FromFunction { name; defining_function } -> (
        match api with
        | Pyrefly pyrefly_context ->
            PyreflyApi.InContext.propagate_captured_variable
              pyrefly_context
              ~defining_function
              ~name)


  (* Turn a captured variable root into a root for the state. Used to assign user provided sources
     for captured variables at the beginning of the forward analysis. *)
  let state_root_of_captured_variable api captured_variable =
    ReadOnly.state_root_of_captured_variable (pyre_api api) captured_variable
end

module ModelQueries = struct
  let property_decorators = Analysis.PysaTypes.ModelQueries.property_decorators

  let mangle_top_level_name = Analysis.PysaTypes.ModelQueries.mangle_top_level_name

  let demangle_class_attribute = Analysis.PysaTypes.ModelQueries.demangle_class_attribute

  let has_class_attribute_form = Analysis.PysaTypes.ModelQueries.has_class_attribute_form

  let mangle_class_attribute = Analysis.PysaTypes.ModelQueries.mangle_class_attribute

  module FunctionParameter = Analysis.PysaTypes.ModelQueries.FunctionParameter
  module FunctionParameters = Analysis.PysaTypes.ModelQueries.FunctionParameters
  module FunctionSignature = Analysis.PysaTypes.ModelQueries.FunctionSignature
  module Function = Analysis.PysaTypes.ModelQueries.Function
  module Global = Analysis.PysaTypes.ModelQueries.Global
  module ModuleResolutionResult = Analysis.PysaTypes.ModelQueries.ModuleResolutionResult
  module ResolutionResult = Analysis.PysaTypes.ModelQueries.ResolutionResult

  let resolve_user_qualified_name
      api
      ~is_property_getter
      ~is_property_setter
      ~verify_class_attributes
      name
    =
    match api with
    | ReadOnly.Pyrefly pyrefly_api ->
        PyreflyApi.ModelQueries.resolve_user_qualified_name
          pyrefly_api
          ~is_property_getter
          ~is_property_setter
          ~verify_class_attributes
          name


  let class_method_signatures = function
    | ReadOnly.Pyrefly pyrefly_api -> PyreflyApi.ModelQueries.class_method_signatures pyrefly_api


  let invalidate_cache = function
    | ReadOnly.Pyrefly _ -> ()
end
