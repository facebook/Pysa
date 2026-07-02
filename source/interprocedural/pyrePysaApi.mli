(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core

(* PyrePysaApi is a wrapper around a type checker API, which exposes source code, ASTs and type
   information about the code to analyze. This wraps the Pyrefly API provided by
   `Interprocedural.Pyrefly`. *)

module ScalarTypeProperties = Analysis.PysaTypes.ScalarTypeProperties
module TypeModifier = Analysis.PysaTypes.TypeModifier
module ClassWithModifiers = Analysis.PysaTypes.ClassWithModifiers
module ClassNamesFromType = Analysis.PysaTypes.ClassNamesFromType
module PysaType = Analysis.PysaTypes.PysaType
module PyreClassSummary = Analysis.ClassSummary
module AstResult = Analysis.PysaTypes.AstResult
module TaintAccessPath = Analysis.TaintAccessPath

(* Abstraction for information about a class, provided from Pyrefly and used by Pysa. See
   `ReadOnly.ClassSummary` for more functions. *)
module PysaClassSummary : sig
  type t
end

module ReadWrite : sig
  type t

  val create_with_cold_start
    :  scheduler:Scheduler.t ->
    scheduler_policies:Configuration.SchedulerPolicies.t ->
    configuration:Configuration.Analysis.t ->
    pyrefly_results:PyrePath.t ->
    decorator_configuration:Analysis.DecoratorPreprocessing.Configuration.t ->
    t

  val parse_type_of_expressions
    :  t ->
    scheduler:Scheduler.t ->
    scheduler_policies:Configuration.SchedulerPolicies.t ->
    t
end

module ReadOnly : sig
  type t = Pyrefly of PyreflyApi.ReadOnly.t

  val of_read_write_api : ReadWrite.t -> t

  val from_pyrefly_api : PyreflyApi.ReadOnly.t -> t

  val explicit_qualifiers : t -> Ast.Reference.t list

  val all_sys_infos : t -> Analysis.PysaTypes.SysInfo.t list

  val absolute_source_path_of_qualifier : t -> Ast.Reference.t -> string option

  val repository_relative_path_of_qualifier
    :  repository_root:PyrePath.t ->
    t ->
    Ast.Reference.t ->
    string option

  val search_path_relative_path_of_qualifier : t -> Ast.Reference.t -> string option

  val get_class_names_for_qualifier
    :  t ->
    exclude_test_modules:bool ->
    Ast.Reference.t ->
    Ast.Reference.t list

  val get_define_names_for_qualifier
    :  t ->
    exclude_test_modules:bool ->
    Ast.Reference.t ->
    Ast.Reference.t list

  val get_methods_for_qualifier
    :  t ->
    exclude_test_modules:bool ->
    Ast.Reference.t ->
    Analysis.PysaTypes.MethodReference.t list

  val get_qualifier_top_level_define_name : t -> Ast.Reference.t -> Ast.Reference.t

  val get_class_summary : t -> string -> PysaClassSummary.t option

  val get_class_decorators_opt : t -> string -> Ast.Expression.t list AstResult.t

  val get_class_attributes
    :  t ->
    include_generated_attributes:bool ->
    only_simple_assignments:bool ->
    string ->
    string list option

  val class_immediate_parents : t -> string -> string list

  val class_mro : t -> string -> string list

  val get_overriden_base_method
    :  t ->
    Analysis.PysaTypes.MethodReference.t ->
    Analysis.PysaTypes.MethodReference.t option

  val target_from_method_reference : Analysis.PysaTypes.MethodReference.t -> Target.t

  val get_callable_captures : t -> Ast.Reference.t -> TaintAccessPath.CapturedVariable.t list

  val get_callable_return_annotations
    :  t ->
    define_name:Ast.Reference.t ->
    define:Ast.Statement.Define.t ->
    PysaType.t list

  val get_callable_parameter_annotations
    :  t ->
    define_name:Ast.Reference.t ->
    TaintAccessPath.NormalizedParameter.t list ->
    (TaintAccessPath.NormalizedParameter.t * PysaType.t list) list

  (* Turn a captured variable root into a root for the state. Used to assign user provided sources
     for captured variables at the beginning of the forward analysis. *)
  val state_root_of_captured_variable
    :  t ->
    TaintAccessPath.CapturedVariable.t ->
    TaintAccessPath.Root.t

  (* TODO(T225700656): Move this in the ClassSummary module *)
  val named_tuple_attributes : t -> string -> string list option

  val all_classes : t -> scheduler:Scheduler.t -> string list

  val ensures_qualified : t -> Ast.Source.t -> Ast.Source.t

  module Type : sig
    val scalar_properties : t -> PysaType.t -> ScalarTypeProperties.t

    val get_class_names : t -> PysaType.t -> ClassNamesFromType.t

    val is_dictionary_or_mapping : t -> PysaType.t -> bool
  end

  module ClassSummary : sig
    val has_custom_new : t -> PysaClassSummary.t -> bool

    val is_dataclass : t -> PysaClassSummary.t -> bool

    val is_named_tuple : t -> PysaClassSummary.t -> bool

    val is_typed_dict : t -> PysaClassSummary.t -> bool

    val dataclass_ordered_attributes : t -> PysaClassSummary.t -> string list

    val typed_dictionary_attributes : t -> PysaClassSummary.t -> string list
  end

  val add_builtins_prefix : t -> Ast.Reference.t -> Ast.Reference.t

  val target_symbolic_name : t -> Ast.Reference.t -> Ast.Reference.t
end

module InContext : sig
  type t = Pyrefly of PyreflyApi.InContext.t

  val create_at_function_scope
    :  ReadOnly.t ->
    module_qualifier:Ast.Reference.t ->
    define_name:Ast.Reference.t ->
    call_graph:CallGraph.DefineCallGraph.t ->
    t

  val create_at_statement_scope
    :  ReadOnly.t ->
    module_qualifier:Ast.Reference.t ->
    define_name:Ast.Reference.t ->
    call_graph:CallGraph.DefineCallGraph.t ->
    statement_key:int ->
    t

  val pyre_api : t -> ReadOnly.t

  val is_global : t -> reference:Ast.Reference.t -> bool

  val resolve_reference : t -> Ast.Reference.t -> Type.t

  val resolve_assignment : t -> Ast.Statement.Assign.t -> t

  val resolve_expression_to_type : t -> Ast.Expression.t -> Type.t

  val resolve_attribute_access : t -> base_type:Type.t -> attribute:string -> Type.t

  val fallback_attribute
    :  t ->
    ?accessed_through_class:bool ->
    ?type_for_lookup:Type.t option ->
    name:string ->
    string ->
    Analysis.AnnotatedAttribute.instantiated option

  val resolve_generators : t -> Ast.Expression.Comprehension.Generator.t list -> t

  val module_qualifier : t -> Ast.Reference.t

  val define_name : t -> Ast.Reference.t

  val root_of_identifier
    :  t ->
    location:Ast.Location.t ->
    identifier:Ast.Identifier.t ->
    TaintAccessPath.Root.t

  val access_path_of_expression
    :  t ->
    self_variable:TaintAccessPath.Root.t option ->
    Ast.Expression.t ->
    TaintAccessPath.t option

  (* Propagate a captured variable from a callee to a caller. Return the new root representing that
     variable in the caller. *)
  val propagate_captured_variable
    :  t ->
    TaintAccessPath.CapturedVariable.t ->
    TaintAccessPath.Root.t

  (* Turn a captured variable root into a root for the state. Used to assign user provided sources
     for captured variables at the beginning of the forward analysis. *)
  val state_root_of_captured_variable
    :  t ->
    TaintAccessPath.CapturedVariable.t ->
    TaintAccessPath.Root.t

  (* Compute the type of the given expression. *)
  val type_of_expression : t -> Ast.Expression.t -> Analysis.PysaTypes.PysaType.t
end

module ModelQueries : sig
  val property_decorators : String.Set.t

  val mangle_top_level_name : Ast.Reference.t -> Ast.Reference.t

  val demangle_class_attribute : Ast.Reference.t -> Ast.Reference.t

  val has_class_attribute_form : Ast.Reference.t -> bool

  val mangle_class_attribute : Ast.Reference.t -> Ast.Reference.t

  module FunctionParameter = Analysis.PysaTypes.ModelQueries.FunctionParameter
  module FunctionParameters = Analysis.PysaTypes.ModelQueries.FunctionParameters
  module FunctionSignature = Analysis.PysaTypes.ModelQueries.FunctionSignature
  module Function = Analysis.PysaTypes.ModelQueries.Function
  module Global = Analysis.PysaTypes.ModelQueries.Global
  module ModuleResolutionResult = Analysis.PysaTypes.ModelQueries.ModuleResolutionResult
  module ResolutionResult = Analysis.PysaTypes.ModelQueries.ResolutionResult

  val resolve_user_qualified_name
    :  ReadOnly.t ->
    is_property_getter:bool ->
    is_property_setter:bool ->
    verify_class_attributes:bool ->
    Ast.Reference.t ->
    ResolutionResult.t

  val class_method_signatures
    :  ReadOnly.t ->
    Ast.Reference.t ->
    (Ast.Reference.t * Ast.Statement.Define.Signature.t option) list option

  val invalidate_cache : ReadOnly.t -> unit
end
