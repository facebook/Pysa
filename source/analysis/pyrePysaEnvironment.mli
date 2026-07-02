(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core

(* Fake module containing all implicit "decorated" targets, which are functions that inline
   decorators. *)
val artificial_decorator_define_module : Ast.Reference.t

module SysInfo = PysaTypes.SysInfo
module ScalarTypeProperties = PysaTypes.ScalarTypeProperties
module TypeModifier = PysaTypes.TypeModifier
module ClassWithModifiers = PysaTypes.ClassWithModifiers
module ClassNamesFromType = PysaTypes.ClassNamesFromType
module PyreflyType = PysaTypes.PyreflyType
module PysaType = PysaTypes.PysaType
module PyreClassSummary = PysaTypes.PyreClassSummary

(* Abstraction for information about a class, provided from Pyre1 or Pyrefly and used by Pysa. See
   `ReadOnly.ClassSummary` for more functions. *)
module PysaClassSummary : sig
  type t

  val find_attribute : t -> string -> PyreClassSummary.Attribute.t option

  val get_attributes : t -> PyreClassSummary.Attribute.t list
end

module AstResult = PysaTypes.AstResult
module MethodKind = PysaTypes.MethodKind
module CallableSignature = PysaTypes.CallableSignature

module ReadWrite : sig
  type t

  val load_from_cache : configuration:Configuration.Analysis.t -> t

  val create_with_cold_start
    :  scheduler:Scheduler.t ->
    scheduler_policies:Configuration.SchedulerPolicies.t ->
    configuration:Configuration.Analysis.t ->
    decorator_configuration:DecoratorPreprocessing.Configuration.t ->
    skip_type_checking_callables:Ast.Reference.SerializableSet.t ->
    callback_with_qualifiers_and_definitions:
      ((lookup_source:(ArtifactPath.t -> SourcePath.t option) -> Ast.Reference.t -> string option) ->
      Ast.Reference.t list ->
      Ast.Reference.t list ->
      unit) ->
    t

  val configuration : t -> Configuration.Analysis.t

  val module_paths : t -> Ast.ModulePath.t list

  val module_paths_from_disk : t -> Ast.ModulePath.t list

  val all_module_paths : t -> Ast.ModulePath.t list

  val artifact_path_of_module_path : t -> Ast.ModulePath.t -> ArtifactPath.t

  val save : t -> unit

  val purge_sources_from_shared_memory : t -> unit
end

module MethodReference = PysaTypes.MethodReference

module ReadOnly : sig
  type t

  val of_read_write_api : ReadWrite.t -> t

  val create
    :  type_environment:TypeEnvironment.TypeEnvironmentReadOnly.t ->
    global_module_paths_api:GlobalModulePathsApi.t ->
    t

  val absolute_source_path_of_qualifier
    :  lookup_source:(ArtifactPath.t -> SourcePath.t option) ->
    t ->
    Ast.Reference.t ->
    string option

  val explicit_qualifiers : t -> Ast.Reference.t list

  val all_sys_infos : t -> SysInfo.t list

  val parse_annotation
    :  t ->
    ?validation:AttributeResolution.type_validation_policy ->
    Ast.Expression.t ->
    Type.t

  val get_class_summary : t -> string -> PysaClassSummary.t option

  val get_class_decorators_opt : t -> string -> Ast.Expression.t list AstResult.t

  val get_class_attributes
    :  t ->
    include_generated_attributes:bool ->
    only_simple_assignments:bool ->
    string ->
    string list option

  val get_class_attribute_annotation
    :  t ->
    include_generated_attributes:bool ->
    class_name:string ->
    attribute:string ->
    Ast.Expression.t option

  val get_global_annotation : t -> Ast.Reference.t -> Ast.Expression.t option

  val source_is_unit_test : t -> source:Ast.Source.t -> bool

  val class_immediate_parents : t -> string -> string list

  val class_mro : t -> string -> string list

  val get_define_names_for_qualifier : t -> Ast.Reference.t -> Ast.Reference.t list

  val get_qualifier_top_level_define_name : t -> Ast.Reference.t -> Ast.Reference.t

  val parse_reference : t -> Ast.Reference.t -> Type.t

  val module_exists : t -> Ast.Reference.t -> bool

  val class_exists : t -> string -> bool

  val get_define_body : t -> Ast.Reference.t -> Ast.Statement.Define.t Ast.Node.t option

  val get_captured_variable_from_nonlocal_target
    :  t ->
    Ast.Identifier.t ->
    TaintAccessPath.CapturedVariable.t

  val get_captures_from_define
    :  t ->
    Ast.Statement.Define.t ->
    TaintAccessPath.CapturedVariable.t list

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

  val get_variable : t -> string -> Type.Variable.t option

  val resolve_define
    :  t ->
    callable_name:Ast.Reference.t option ->
    implementation:Ast.Statement.Define.Signature.t option ->
    overloads:Ast.Statement.Define.Signature.t list ->
    scoped_type_variables:Type.Variable.t Ast.Identifier.Map.t option ->
    AttributeResolution.resolved_define

  val resolve_define_undecorated
    :  t ->
    callable_name:Ast.Reference.t option ->
    implementation:Ast.Statement.Define.Signature.t option ->
    overloads:Ast.Statement.Define.Signature.t list ->
    scoped_type_variables:Type.Variable.t Ast.Identifier.Map.t option ->
    AnnotatedAttribute.decorated_method

  val global : t -> Ast.Reference.t -> AttributeResolution.Global.t option

  val get_overriden_base_method : t -> MethodReference.t -> MethodReference.t option

  val annotation_parser : t -> AnnotatedCallable.annotation_parser

  val less_or_equal : t -> left:Type.t -> right:Type.t -> bool

  val resolve_exports : t -> ?from:Ast.Reference.t -> Ast.Reference.t -> ResolvedReference.t option

  val location_of_global : t -> Ast.Reference.t -> Ast.Location.WithModule.t option

  val get_function_definition : t -> Ast.Reference.t -> FunctionDefinition.t option

  val attribute_from_class_name
    :  t ->
    ?transitive:bool ->
    ?accessed_through_class:bool ->
    ?accessed_through_readonly:bool ->
    ?special_method:bool ->
    string ->
    name:string ->
    type_for_lookup:Type.t ->
    AnnotatedAttribute.instantiated option

  val has_transitive_successor : t -> successor:string -> string -> bool

  val has_transitive_successor_ignoring_untracked
    :  t ->
    reflexive:bool ->
    predecessor:string ->
    successor:string ->
    bool

  val exists_matching_class_decorator
    :  t ->
    ?dependency:SharedMemoryKeys.DependencyKey.registered ->
    names:string list ->
    ClassSummary.t Ast.Node.t ->
    bool

  val generic_parameters_as_variables : t -> string -> Type.Variable.t list option

  val source_of_qualifier : t -> Ast.Reference.t -> Ast.Source.t option

  val get_class_names_for_qualifier
    :  t ->
    exclude_test_modules:bool ->
    Ast.Reference.t ->
    Ast.Reference.t list

  val relative_path_of_qualifier : t -> Ast.Reference.t -> string option

  val decorated_define : t -> Ast.Statement.Define.t Ast.Node.t -> Ast.Statement.Define.t Ast.Node.t

  val named_tuple_attributes : t -> string -> string list option

  val resolve_expression_to_type_info : t -> Ast.Expression.t -> TypeInfo.Unit.t

  val get_unannotated_global
    :  t ->
    ?dependency:SharedMemoryKeys.DependencyKey.registered ->
    Ast.Reference.t ->
    Module.UnannotatedGlobal.t option

  val all_classes : t -> scheduler:Scheduler.t -> string list

  val all_unannotated_globals : t -> scheduler:Scheduler.t -> Ast.Reference.t list

  module Type : sig
    (* Returns whether the type is an int, float, bool or enum, after stripping Optional and
       Awaitable. *)
    val scalar_properties : t -> PysaType.t -> ScalarTypeProperties.t

    (* Return a list of fully qualified class names that this type refers to, after
     * stripping Optional, ReadOnly and TypeVar.
     *
     * For instance:
     * Union[int, str] -> [int, str]
     * Optional[int] -> [int]
     * List[int] -> [List]
     * List[Dict[str, str]] -> [List]
     *)
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

  val get_methods_for_qualifier
    :  t ->
    exclude_test_modules:bool ->
    Ast.Reference.t ->
    MethodReference.t list
end

module InContext : sig
  type t

  val create_at_function_scope
    :  ReadOnly.t ->
    module_qualifier:Ast.Reference.t ->
    define_name:Ast.Reference.t ->
    t

  val create_at_statement_scope
    :  ReadOnly.t ->
    module_qualifier:Ast.Reference.t ->
    define_name:Ast.Reference.t ->
    define:Ast.Statement.Define.t Ast.Node.t ->
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
    AnnotatedAttribute.instantiated option

  val resolve_generators : t -> Ast.Expression.Comprehension.Generator.t list -> t

  val module_qualifier : t -> Ast.Reference.t

  val define_name : t -> Ast.Reference.t
end

module ModelQueries : sig
  val property_decorators : String.Set.t

  module FunctionParameter = PysaTypes.ModelQueries.FunctionParameter
  module FunctionParameters = PysaTypes.ModelQueries.FunctionParameters

  module FunctionSignature : sig
    type t = PysaTypes.ModelQueries.FunctionSignature.t = {
      parameters: FunctionParameters.t;
      return_annotation: PysaType.t;
    }
    [@@deriving equal, compare, show]

    val toplevel : t

    val from_callable_type : Type.Callable.t -> t list

    val from_pyre1_ast
      :  pyre_api:ReadOnly.t ->
      parameters:Ast.Expression.Parameter.t list ->
      return_annotation:Ast.Expression.t option ->
      t
  end

  module Function = PysaTypes.ModelQueries.Function
  module Global = PysaTypes.ModelQueries.Global
  module ModuleResolutionResult = PysaTypes.ModelQueries.ModuleResolutionResult
  module ResolutionResult = PysaTypes.ModelQueries.ResolutionResult

  val mangle_top_level_name : Ast.Reference.t -> Ast.Reference.t

  val demangle_class_attribute : Ast.Reference.t -> Ast.Reference.t

  val has_class_attribute_form : Ast.Reference.t -> bool

  val mangle_class_attribute : Ast.Reference.t -> Ast.Reference.t

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

  val invalidate_cache : unit -> unit
end
