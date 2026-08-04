(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* Type checker API used by Pysa, which exposes source code, ASTs and type information about the
   code to analyze, using the results from a pyrefly run with --report-pysa. *)

open Core
module PyreflyType = PyreflyTypes.PyreflyType
module AstResult = PyreflyTypes.AstResult
module ScalarTypeProperties = PyreflyTypes.ScalarTypeProperties
module TypeModifier = PyreflyTypes.TypeModifier
module ClassWithModifiers = PyreflyTypes.ClassWithModifiers
module ClassesFromType = PyreflyTypes.ClassesFromType

module SysInfo : sig
  type t = {
    python_version: Configuration.PythonVersion.t;
    platform: string option;
  }
  [@@deriving compare, equal, show, sexp, hash]

  module Set : Stdlib.Set.S with type elt = t
end

module FormatError : sig
  type t =
    | UnexpectedJsonType of {
        json: Yojson.Safe.t;
        message: string;
      }
    | UnsupportedVersion of { version: int }
    | UnparsableString of string
  [@@deriving show]
end

module Error : sig
  type t =
    | InvalidJsonError of string
    | InvalidCapnpError of string
    | IOError of string
    | FormatError of FormatError.t
  [@@deriving show]
end

exception
  PyreflyFileFormatError of {
    path: PyrePath.t;
    error: Error.t;
  }

exception NoSourceFilesToAnalyze

module NameLocation : sig
  type t =
    | DefineName of
        Ast.Location.t (* Location of the name AST node, i.e location of `foo` in `def foo():` *)
    | ModuleTopLevel
    | ClassName of
        Ast.Location.t (* Location of the class AST node, i.e location of `Foo` in `class Foo:` *)
    | UnknonwnForClassField
  [@@deriving show]
end

module CallableMetadata : sig
  type t = {
    module_id: PyreflyTypes.ModuleId.t;
    name_location: NameLocation.t;
    is_overload: bool;
    is_staticmethod: bool;
    is_classmethod: bool;
    is_property_getter: bool;
    is_property_setter: bool;
    is_toplevel: bool; (* Is this the body of a module? *)
    is_class_toplevel: bool; (* Is this the body of a class? *)
    is_stub_define: bool; (* Is this a stub definition, e.g `def foo(): ...` *)
    is_def_statement: bool; (* Is this associated with a `def ..` statement? *)
    parent_is_class: bool;
  }
  [@@deriving show]

  val get_method_kind : t -> PyreflyTypes.MethodKind.t option
end

module PysaClassSummary : sig
  type t
end

(* API handle stored in the main process. The type `t` should not be sent to workers, since it's
   expensive to copy. *)
module ReadWrite : sig
  type t

  val create_from_directory
    :  scheduler:Scheduler.t ->
    scheduler_policies:Configuration.SchedulerPolicies.t ->
    configuration:Configuration.Analysis.t ->
    PyrePath.t ->
    t

  val parse_type_of_expressions
    :  t ->
    scheduler:Scheduler.t ->
    scheduler_policies:Configuration.SchedulerPolicies.t ->
    t

  val create_with_cold_start
    :  scheduler:Scheduler.t ->
    scheduler_policies:Configuration.SchedulerPolicies.t ->
    configuration:Configuration.Analysis.t ->
    pyrefly_results:PyrePath.t ->
    decorator_configuration:Analysis.DecoratorPreprocessing.Configuration.t ->
    t

  val cleanup : t -> scheduler:Scheduler.t -> unit
end

(* Read-only API that can be sent to workers. Cheap to copy. *)
module ReadOnly : sig
  type t

  val of_read_write_api : ReadWrite.t -> t

  (* Return all module ids with source code *)
  val explicit_modules : t -> PyreflyTypes.ModuleId.t list

  (* Resolve a module qualifier to its module id. *)
  val module_id_of_qualifier_opt : t -> Ast.Reference.t -> PyreflyTypes.ModuleId.t option

  (* Resolve a module qualifier to its module id. *)
  val module_id_of_qualifier : t -> Ast.Reference.t -> PyreflyTypes.ModuleId.t

  (* Materialize a module id back into its module qualifier. *)
  val module_qualifier_of_id : t -> PyreflyTypes.ModuleId.t -> Ast.Reference.t

  val all_sys_infos : t -> SysInfo.t list

  val artifact_path_of_module : t -> PyreflyTypes.ModuleId.t -> ArtifactPath.t option

  val absolute_source_path_of_module : t -> PyreflyTypes.ModuleId.t -> string option

  val search_path_relative_path_of_module : t -> PyreflyTypes.ModuleId.t -> string option

  val get_class_ids_for_module
    :  t ->
    exclude_test_modules:bool ->
    PyreflyTypes.ModuleId.t ->
    PyreflyTypes.ClassId.t list

  (* Materialize a class id (as returned by `get_class_ids_for_module`) back into its fully
     qualified name. *)
  val class_name_from_id : t -> PyreflyTypes.ClassId.t -> Ast.Reference.t

  (* Resolve a fully qualified class name into its class id. Raises if the class is unknown. *)
  val class_id_from_name : t -> Ast.Reference.t -> PyreflyTypes.ClassId.t

  (* Resolve a fully qualified class name into its class id, or `None` if the class is unknown. *)
  val class_id_from_name_opt : t -> Ast.Reference.t -> PyreflyTypes.ClassId.t option

  val all_classes : t -> scheduler:Scheduler.t -> PyreflyTypes.ClassId.t list

  val all_global_variables : t -> scheduler:Scheduler.t -> (PyreflyTypes.ModuleId.t * string) list

  val get_callable_ids_for_module
    :  t ->
    exclude_test_modules:bool ->
    PyreflyTypes.ModuleId.t ->
    PyreflyTypes.CallableId.t list

  (* Return the immediate parents of a class, keyed on a class id (as returned by
     `get_class_ids_for_module`). Parents are class ids too (including the synthesized `object`
     parent). *)
  val class_immediate_parents : t -> PyreflyTypes.ClassId.t -> PyreflyTypes.ClassId.t list

  val class_mro : t -> PyreflyTypes.ClassId.t -> PyreflyTypes.ClassId.t list

  val is_subclass : t -> parent:PyreflyTypes.ClassId.t -> child:PyreflyTypes.ClassId.t -> bool

  val is_object_class_id : t -> PyreflyTypes.ClassId.t -> bool

  (* Whether the callable is stub-like, keyed on a callable id (as returned by
     `get_callable_ids_for_module`). *)
  val is_stub_like_callable : t -> PyreflyTypes.CallableId.t -> bool

  val is_stub_like_callable_opt : t -> PyreflyTypes.CallableId.t -> bool option

  val get_callable_return_annotations
    :  t ->
    callable_id:PyreflyTypes.CallableId.t ->
    define:Ast.Statement.Define.t ->
    PyreflyType.t list

  val get_callable_parameter_annotations
    :  t ->
    callable_id:PyreflyTypes.CallableId.t ->
    AccessPath.NormalizedParameter.t list ->
    (AccessPath.NormalizedParameter.t * PyreflyType.t list) list

  val get_callable_captures : t -> PyreflyTypes.CallableId.t -> AccessPath.CapturedVariable.t list

  val get_callable_captures_opt
    :  t ->
    PyreflyTypes.CallableId.t ->
    AccessPath.CapturedVariable.t list option

  val get_callable_decorator_callees
    :  t ->
    PyreflyTypes.CallableId.t ->
    Ast.Location.t ->
    PyreflyTypes.CallableId.t list option

  val get_class_decorator_callees
    :  t ->
    PyreflyTypes.ClassId.t ->
    Ast.Location.t ->
    PyreflyTypes.CallableId.t list option

  val get_methods_for_module
    :  t ->
    exclude_test_modules:bool ->
    PyreflyTypes.ModuleId.t ->
    PyreflyTypes.CallableId.t list

  (* Is this a test module (i.e, unit test code that we shouldn't analyze) *)
  val is_test_module : t -> PyreflyTypes.ModuleId.t -> bool

  (* Is this a stub module, i.e a `.pyi` file. *)
  val is_stub_module : t -> PyreflyTypes.ModuleId.t -> bool

  (* Is this an internal module (within the project's source directories). *)
  val is_internal_module : t -> PyreflyTypes.ModuleId.t -> bool

  (* Return the AST for the given function, keyed on a callable id (as returned by
     `get_callable_ids_for_module`). *)
  val get_define_opt
    :  t ->
    PyreflyTypes.CallableId.t ->
    Ast.Statement.Define.t Ast.Node.t AstResult.t

  val get_callable_signature_opt
    :  t ->
    PyreflyTypes.CallableId.t ->
    PyreflyTypes.CallableSignature.t option

  val get_undecorated_signatures
    :  t ->
    PyreflyTypes.CallableId.t ->
    PyreflyTypes.ModelQueries.FunctionSignature.t list

  val get_model_parser_function_info
    :  t ->
    PyreflyTypes.CallableId.t ->
    PyreflyTypes.ModelQueries.Function.t

  val get_class_summary : t -> PyreflyTypes.ClassId.t -> PysaClassSummary.t

  val get_class_decorators_opt : t -> PyreflyTypes.ClassId.t -> Ast.Expression.t list AstResult.t

  val get_class_attributes
    :  t ->
    include_generated_attributes:bool ->
    only_simple_assignments:bool ->
    PyreflyTypes.ClassId.t ->
    string list option

  val get_class_attribute_inferred_type
    :  t ->
    class_id:PyreflyTypes.ClassId.t ->
    attribute:string ->
    PyreflyType.t

  val get_class_attribute_explicit_annotation
    :  t ->
    class_id:PyreflyTypes.ClassId.t ->
    attribute:string ->
    string option

  val get_global_inferred_type
    :  t ->
    module_id:PyreflyTypes.ModuleId.t ->
    name:string ->
    PyreflyType.t option

  val parse_call_graphs
    :  t ->
    scheduler:Scheduler.t ->
    scheduler_policies:Configuration.SchedulerPolicies.t ->
    overrides_exist:(Target.t -> bool) ->
    get_overriding_targets:(Target.t -> PyreflyTypes.CallableId.t list option) ->
    global_is_string_literal:(Ast.Reference.t -> bool) ->
    store_shared_memory:bool ->
    attribute_targets:Target.Set.t ->
    skip_analysis_targets:Target.HashSet.t ->
    find_missing_flows:Configuration.MissingFlowKind.t option ->
    definitions:Target.t list ->
    create_dependency_for:CallGraph.AllTargetsUseCase.t ->
    redirect_to_decorated:(Target.t -> Target.t option) ->
    transform_call_graph:
      (t -> Target.t -> CallGraph.DefineCallGraph.t -> CallGraph.DefineCallGraph.t) ->
    CallGraph.SharedMemory.call_graphs

  val parse_type_errors : t -> Analysis.AnalysisError.Instantiated.t list

  val get_type_of_expression
    :  t ->
    callable_id:PyreflyTypes.CallableId.t ->
    location:Ast.Location.t ->
    PyreflyType.t option

  module Type : sig
    val scalar_properties : t -> PyreflyType.t -> PyreflyTypes.ScalarTypeProperties.t

    val get_classes : t -> PyreflyType.t -> PyreflyTypes.ClassesFromType.t

    val is_dictionary_or_mapping : t -> PyreflyType.t -> bool
  end

  module ClassSummary : sig
    val has_custom_new : t -> PysaClassSummary.t -> bool

    val is_dataclass : t -> PysaClassSummary.t -> bool

    val is_named_tuple : t -> PysaClassSummary.t -> bool

    val is_typed_dict : t -> PysaClassSummary.t -> bool

    val dataclass_ordered_attributes : t -> PysaClassSummary.t -> string list

    val typed_dictionary_attributes : t -> PysaClassSummary.t -> string list

    val named_tuple_attributes : t -> PysaClassSummary.t -> string list
  end

  val named_tuple_attributes : t -> PyreflyTypes.ClassId.t -> string list option

  val repository_relative_path_of_module
    :  repository_root:PyrePath.t ->
    t ->
    PyreflyTypes.ModuleId.t ->
    string option

  (* Turn a captured variable root into a root for the state. Used to assign user provided sources
     for captured variables at the beginning of the forward analysis. *)
  val state_root_of_captured_variable : t -> AccessPath.CapturedVariable.t -> AccessPath.Root.t

  val ensures_qualified : t -> Ast.Source.t -> Ast.Source.t

  (* Build a display api from the id->name shared-memory maps *)
  val display_api : t -> PyreflyTypes.DisplayApi.t

  (* Callable- and target-oriented queries. *)
  module Target : sig
    (* Render a target as an external (user-facing) name. Convenience wrapper that does not require
       a display API. *)
    val external_name : pyrefly_api:t -> Target.t -> string

    (* Return the define name of a Function or Method target. Note that multiple targets can match
       to the same define name (e.g, property getters and setters). Hence, use this at your own
       risk. *)
    val define_name : t -> Target.t -> Ast.Reference.t option

    val define_name_exn : t -> Target.t -> Ast.Reference.t

    val class_id : t -> Target.t -> PyreflyTypes.ClassId.t option

    val class_id_exn : t -> Target.t -> PyreflyTypes.ClassId.t

    val class_name : t -> Target.t -> string option

    val class_name_exn : t -> Target.t -> string

    val method_name : t -> Target.t -> string option

    val method_name_exn : t -> Target.t -> string

    val function_name : t -> Target.t -> string option

    val function_name_exn : t -> Target.t -> string

    (* Whether the target is a property setter (from metadata). *)
    val is_property_setter : t -> Target.t -> bool

    (* Whether the target is a "normal" callable: neither decorated nor a property setter, and not
       an `Object`. *)
    val is_normal : t -> Target.t -> bool

    val get_callable_metadata_opt : t -> PyreflyTypes.CallableId.t -> CallableMetadata.t option

    (* Return the metadata of a callable, keyed on a callable id (as returned by
       `get_callable_ids_for_module`). *)
    val get_callable_metadata : t -> PyreflyTypes.CallableId.t -> CallableMetadata.t

    val get_method_class_id : t -> PyreflyTypes.CallableId.t -> PyreflyTypes.ClassId.t option

    (* Return the fully qualified name of the class that defines this callable, or `None` if the
       callable is not defined within a class. *)
    val get_method_class_name : t -> PyreflyTypes.CallableId.t -> Ast.Reference.t option

    (* Resolve a `(class, bare method name)` pair to the real method target defined on that class,
       or `None` if the class has no method with that bare name. `method_name` must be the bare name
       (without suffixes like `@setter` or `$2`). *)
    val resolve_method_target
      :  t ->
      class_id:PyreflyTypes.ClassId.t ->
      method_name:string ->
      is_property_setter:bool ->
      Target.t option

    (* Resolve a function name to its real function target, or `None` if no such function exists. *)
    val resolve_function_target : t -> Ast.Reference.t -> Target.t option

    val get_overriden_base_method
      :  t ->
      PyreflyTypes.CallableId.t ->
      PyreflyTypes.CallableId.t option

    (* Build a target from a callable id (as returned by `get_callable_ids_for_module`). *)
    val target_from_callable_id : t -> override:bool -> PyreflyTypes.CallableId.t -> Target.t

    (* Resolve a define name into its callable id, or `None` if pyrefly does not know about it. *)
    val callable_id_from_name : t -> Ast.Reference.t -> PyreflyTypes.CallableId.t option

    (* Same as `callable_id_from_name`, but raises if pyrefly does not know about the callable. *)
    val callable_id_from_name_exn : t -> Ast.Reference.t -> PyreflyTypes.CallableId.t
  end
end

val add_builtins_prefix : Ast.Reference.t -> Ast.Reference.t

val target_symbolic_name : Ast.Reference.t -> Ast.Reference.t

(* Remove the source path prefix that pyrefly may add to a module name to disambiguate modules
   sharing the same name (e.g. `a/b/c.py:a.b.c.foo` -> `a.b.c.foo`). No-op when there is no
   prefix. *)
val strip_path_prefix : string -> string

module ModelQueries : sig
  module Function = PyreflyTypes.ModelQueries.Function
  module Global = PyreflyTypes.ModelQueries.Global
  module ModuleResolutionResult = PyreflyTypes.ModelQueries.ModuleResolutionResult
  module ResolutionResult = PyreflyTypes.ModelQueries.ResolutionResult
  module FunctionParameter = PyreflyTypes.ModelQueries.FunctionParameter
  module FunctionParameters = PyreflyTypes.ModelQueries.FunctionParameters
  module FunctionSignature = PyreflyTypes.ModelQueries.FunctionSignature

  val property_decorators : String.Set.t

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
    PyreflyTypes.ClassId.t ->
    (PyreflyTypes.CallableId.t * Ast.Statement.Define.Signature.t option) list option
end

module InContext : sig
  type t

  val create_at_function_scope
    :  ReadOnly.t ->
    callable_id:PyreflyTypes.CallableId.t ->
    define_name:Ast.Reference.t ->
    call_graph:CallGraph.DefineCallGraph.t ->
    t

  val create_at_statement_scope
    :  ReadOnly.t ->
    callable_id:PyreflyTypes.CallableId.t ->
    define_name:Ast.Reference.t ->
    call_graph:CallGraph.DefineCallGraph.t ->
    statement_key:int ->
    t

  val pyrefly_api : t -> ReadOnly.t

  val call_graph : t -> CallGraph.DefineCallGraph.t

  val root_of_identifier
    :  t ->
    location:Ast.Location.t ->
    identifier:Ast.Identifier.t ->
    AccessPath.Root.t

  (* Propagate a captured variable from a callee to a caller. Return the new root representing that
     variable in the caller. *)
  val propagate_captured_variable : t -> AccessPath.CapturedVariable.t -> AccessPath.Root.t

  val access_path_of_expression
    :  t ->
    self_variable:AccessPath.Root.t option ->
    Ast.Expression.t ->
    AccessPath.t option

  (* Turn a captured variable root into a root for the state. Used to assign user provided sources
     for captured variables at the beginning of the forward analysis. *)
  val state_root_of_captured_variable : t -> AccessPath.CapturedVariable.t -> AccessPath.Root.t

  (* Compute the type of the given expression. *)
  val type_of_expression : t -> Ast.Expression.t -> PyreflyType.t
end

(* Exposed for testing purposes *)
module ModulePath : sig
  type t =
    | Filesystem of ArtifactPath.t
    | Namespace of PyrePath.t
    | Memory of PyrePath.t
    | BundledTypeshed of PyrePath.t
    | BundledTypeshedThirdParty of PyrePath.t
    | BundledThirdParty of PyrePath.t
  [@@deriving compare, equal, show]
end

(* Exposed for testing purposes *)
module ModuleInfoFilename : sig
  type t [@@deriving compare, equal, show]

  val create : string -> t
end

(* Exposed for testing purposes *)
module ProjectFile : sig
  module Module : sig
    type t = {
      module_id: PyreflyTypes.ModuleId.t;
      module_name: Ast.Reference.t;
      absolute_source_path: ModulePath.t;
      relative_source_path: string option;
      info_filename: ModuleInfoFilename.t option;
      python_version: Configuration.PythonVersion.t;
      platform: string;
      is_test: bool;
      is_interface: bool;
      is_init: bool;
      is_internal: bool;
      failed_to_load: bool;
    }
    [@@deriving equal, show]
  end
end

(* Exposed for testing purposes *)
module ClassFieldDeclarationKind : sig
  type t =
    | DeclaredByAnnotation
    | DeclaredWithoutAnnotation
    | AssignedInBody
    | DefinedWithoutAssign
    | DefinedInMethod
  [@@deriving equal, show]
end

(* Exposed for testing purposes *)
module CapturedVariable : sig
  type t = {
    name: string;
    outer_function: PyreflyTypes.CallableId.t;
  }
  [@@deriving equal, show]
end

(* Exposed for testing purposes *)
module ModuleDefinitionsFile : sig
  module ParentScope : sig
    type t =
      | TopLevel
      | Class of PyreflyTypes.LocalClassId.t
      | Function of PyreflyTypes.FuncDefIndex.t
    [@@deriving equal, show]
  end

  module FunctionParameter : sig
    type t =
      | PosOnly of {
          name: string option;
          annotation: PyreflyType.t;
          required: bool;
        }
      | Pos of {
          name: string;
          annotation: PyreflyType.t;
          required: bool;
        }
      | VarArg of {
          name: string option;
          annotation: PyreflyType.t;
        }
      | KwOnly of {
          name: string;
          annotation: PyreflyType.t;
          required: bool;
        }
      | Kwargs of {
          name: string option;
          annotation: PyreflyType.t;
        }
    [@@deriving equal, show]
  end

  module FunctionParameters : sig
    type t =
      | List of FunctionParameter.t list
      | Ellipsis
      | ParamSpec
    [@@deriving equal, show]
  end

  module FunctionSignature : sig
    type t = {
      parameters: FunctionParameters.t;
      return_annotation: PyreflyType.t;
    }
    [@@deriving equal, show]
  end

  module FunctionDefinition : sig
    type t = {
      name: string;
      name_location: Ast.Location.t option;
      local_function_id: PyreflyTypes.LocalFunctionId.t;
      parent: ParentScope.t;
      undecorated_signatures: FunctionSignature.t list;
      captured_variables: CapturedVariable.t list;
      is_overload: bool;
      is_staticmethod: bool;
      is_classmethod: bool;
      is_property_getter: bool;
      is_property_setter: bool;
      is_stub: bool;
      is_def_statement: bool;
      is_toplevel: bool;
      is_class_toplevel: bool;
      overridden_base_method: PyreflyTypes.CallableId.t option;
      defining_class: PyreflyTypes.ClassId.t option;
      decorator_callees: PyreflyTypes.CallableId.t list Ast.Location.SerializableMap.t;
    }
    [@@deriving equal, show]
  end

  module ClassMro : sig
    type t =
      | Resolved of PyreflyTypes.ClassId.t list
      | Cyclic
    [@@deriving equal, show]
  end

  module PyreflyClassField : sig
    type t = {
      name: string;
      type_: PyreflyType.t;
      explicit_annotation: string option;
      location: Ast.Location.t option;
      declaration_kind: ClassFieldDeclarationKind.t option;
    }
    [@@deriving equal, show]
  end

  module ClassDefinition : sig
    type t = {
      name: string;
      local_class_id: PyreflyTypes.LocalClassId.t;
      name_location: Ast.Location.t;
      parent: ParentScope.t;
      bases: PyreflyTypes.ClassId.t list;
      mro: ClassMro.t;
      is_synthesized: bool;
      is_dataclass: bool;
      is_named_tuple: bool;
      is_typed_dict: bool;
      fields: PyreflyClassField.t list;
      decorator_callees: PyreflyTypes.CallableId.t list Ast.Location.SerializableMap.t;
    }
    [@@deriving equal, show]
  end
end

(* Exposed for testing purposes *)
module ModuleQualifier : sig
  type t [@@deriving compare, equal, show]

  val create : path:string option -> Ast.Reference.t -> t

  val from_reference_unchecked : Ast.Reference.t -> t

  val to_reference : t -> Ast.Reference.t

  module Map : Map.S with type Key.t = t
end

(* Exposed for testing purposes *)
module FullyQualifiedName : sig
  type t [@@deriving compare, equal, show]

  val to_reference : t -> Ast.Reference.t
end

(* Exposed for testing purposes *)
module Testing : sig
  module Module : sig
    type t = {
      module_id: PyreflyTypes.ModuleId.t;
      module_name: Ast.Reference.t;
      absolute_source_path: ArtifactPath.t option;
      relative_source_path: string option;
      pyrefly_info_filename: ModuleInfoFilename.t option;
      sys_info: SysInfo.t;
      is_test: bool;
      is_stub: bool;
      is_internal: bool;
      failed_to_load: bool;
    }
    [@@deriving compare, equal, show]
  end

  (* Build a mapping from unique module qualifiers (module name + path prefix) to module. *)
  val create_module_qualifiers
    :  pyrefly_directory:PyrePath.t ->
    add_toplevel_modules:bool ->
    ProjectFile.Module.t list ->
    Module.t ModuleQualifier.Map.t

  module Definition : sig
    type t =
      | Function of ModuleDefinitionsFile.FunctionDefinition.t
      | Class of ModuleDefinitionsFile.ClassDefinition.t
    [@@deriving equal, show]
  end

  module QualifiedDefinition : sig
    type t = {
      qualified_name: FullyQualifiedName.t;
      local_name: Ast.Reference.t; (* a non-unique name, more user-friendly. *)
      definition: Definition.t; (* class or def *)
      name_location: NameLocation.t;
    }
  end

  val create_fully_qualified_names
    :  module_qualifier:ModuleQualifier.t ->
    module_exists:(ModuleQualifier.t -> bool) ->
    class_definitions:ModuleDefinitionsFile.ClassDefinition.t PyreflyTypes.LocalClassId.Map.t ->
    function_definitions:
      ModuleDefinitionsFile.FunctionDefinition.t PyreflyTypes.LocalFunctionId.Map.t ->
    QualifiedDefinition.t list
end
