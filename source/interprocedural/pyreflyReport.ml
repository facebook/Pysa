(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* Shared types and format-agnostic utilities for pyrefly report files. *)

open Core
open Ast
module PyreflyType = PyreflyTypes.PyreflyType
module ScalarTypeProperties = PyreflyTypes.ScalarTypeProperties
module FormatError = PyreflyTypes.FormatError
module ModuleId = PyreflyTypes.ModuleId
module LocalClassId = PyreflyTypes.LocalClassId
module FuncDefIndex = PyreflyTypes.FuncDefIndex
module LocalFunctionId = PyreflyTypes.LocalFunctionId

module Error = struct
  type t =
    | InvalidJsonError of string
    | InvalidCapnpError of string
    | IOError of string
    | FormatError of FormatError.t
  [@@deriving show]
end

let fixup_location { Location.start; stop } =
  (* WARNING: Pysa uses 0-indexed column numbers while Pyrefly uses 1-indexed column numbers. *)
  let decrement_column { Location.line; column } = { Location.line; column = column - 1 } in
  { Location.start = decrement_column start; stop = decrement_column stop }


let parse_location location =
  match Location.from_string location with
  | Ok location -> Ok (fixup_location location)
  | Error error ->
      Error (FormatError.UnexpectedJsonType { json = `String location; message = error })


exception
  PyreflyFileFormatError of {
    path: PyrePath.t;
    error: Error.t;
  }

(* Path of a module, equivalent of the `pyrefly_python::module_path::ModulePathDetails` rust
   type. *)
module ModulePath = struct
  type t =
    | Filesystem of ArtifactPath.t
    | Namespace of PyrePath.t
    | Memory of PyrePath.t
    | BundledTypeshed of PyrePath.t
    | BundledTypeshedThirdParty of PyrePath.t
    | BundledThirdParty of PyrePath.t
  [@@deriving compare, equal, show]

  let artifact_file_path ~pyrefly_directory = function
    | Filesystem path -> Some path
    | Namespace _ -> None (* directory *)
    | Memory _ -> None (* not handled *)
    | BundledTypeshed path ->
        Some
          (pyrefly_directory
          |> PyrePath.append ~element:"typeshed"
          |> PyrePath.append ~element:(PyrePath.absolute path)
          |> ArtifactPath.create)
    | BundledTypeshedThirdParty path ->
        Some
          (pyrefly_directory
          |> PyrePath.append ~element:"typeshed_third_party"
          |> PyrePath.append ~element:(PyrePath.absolute path)
          |> ArtifactPath.create)
    | BundledThirdParty path ->
        Some
          (pyrefly_directory
          |> PyrePath.append ~element:"third_party"
          |> PyrePath.append ~element:(PyrePath.absolute path)
          |> ArtifactPath.create)
end

(* Unique identifier for a class, assigned by pyrefly. *)
module GlobalClassId = struct
  type t = {
    module_id: ModuleId.t;
    local_class_id: LocalClassId.t;
  }
  [@@deriving compare, equal, show]

  let to_class_id { module_id; local_class_id } =
    PyreflyTypes.ClassId.encode ~module_id local_class_id


  let of_class_id class_id =
    let module_id, local_class_id = PyreflyTypes.ClassId.decode class_id in
    { module_id; local_class_id }
end

(* Unique identifier for a callable (function or method) *)
module GlobalCallableId = struct
  type t = {
    module_id: ModuleId.t;
    local_function_id: LocalFunctionId.t;
  }
  [@@deriving compare, equal, show]

  let to_callable_id { module_id; local_function_id } =
    PyreflyTypes.CallableId.encode ~module_id local_function_id


  let of_callable_id callable_id =
    let module_id, local_function_id = PyreflyTypes.CallableId.decode callable_id in
    { module_id; local_function_id }


  let _ = pp, LocalFunctionId.show
end

module PyreflyTarget = struct
  type t =
    | Function of GlobalCallableId.t
    | Overrides of GlobalCallableId.t
    | FormatString
  [@@deriving compare, equal, show]
end

module ModuleIdSharedMemoryKey = struct
  type t = ModuleId.t [@@deriving compare]

  let to_string module_id = string_of_int (ModuleId.to_int module_id)
end

module GlobalCallableIdSharedMemoryKey = struct
  type t = GlobalCallableId.t [@@deriving compare]

  let to_string { GlobalCallableId.module_id; local_function_id } =
    Format.asprintf "%d|%a" (ModuleId.to_int module_id) LocalFunctionId.pp local_function_id
end

module CallableIdSharedMemoryKey = struct
  type t = PyreflyTypes.CallableId.t [@@deriving compare]

  let to_string id = string_of_int (PyreflyTypes.CallableId.to_int id)
end

module ClassIdSharedMemoryKey = struct
  type t = PyreflyTypes.ClassId.t [@@deriving compare]

  let to_string id = string_of_int (PyreflyTypes.ClassId.to_int id)
end

(* The name of a module (e.g., `django.http.request`). This is a plain dotted name without any path
   prefix. Compare with `ModuleQualifier` which may include a path prefix for disambiguation. *)
module ModuleName : sig
  type t [@@deriving compare, equal, sexp, hash, show]

  val create : Reference.t -> t

  val to_reference : t -> Reference.t

  val from_reference_unchecked : Reference.t -> t

  module Map : Map.S with type Key.t = t
end = struct
  module T = struct
    type t = Reference.t [@@deriving compare, equal, sexp, hash, show]
  end

  include T

  let create = Fn.id

  let to_reference = Fn.id

  let from_reference_unchecked = Fn.id

  module Map = Map.Make (T)
end

(* A unique qualifier for a module and source path.
 *
 * When multiple modules share the same name (e.g., a `.pyi` stub and a `.py` source
 * for `django.http.request`), the qualifier includes a path prefix to disambiguate
 * (e.g., `request.py:django.http.request`).
 *
 * For modules with a unique name, the qualifier is the same as the module name (no prefix).
 * Each module qualifier maps to a unique source file.
 *
 * Note that this is converted into `Reference.t` during the taint analysis for
 * backward compatibility with the old Pyre1 API, which assumes a module name can only map to one
 * source file. *)
module ModuleQualifier : sig
  type t [@@deriving compare, equal, sexp, hash, show]

  val create : path:string option -> Reference.t -> t

  (* The taint analysis uses Reference.t to uniquely represent modules, so we use
     to_reference/from_reference to convert to that type *)
  val to_reference : t -> Reference.t

  (* This is marked `unchecked` because it doesn't actually validate that the reference is a valid
     module qualifier. *)
  val from_reference_unchecked : Reference.t -> t

  (* Return the module name without the path prefix. *)
  val bare_module_name : t -> ModuleName.t

  module Map : Map.S with type Key.t = t
end = struct
  module T = struct
    (* For now, this is stored as a reference internally, because Pyre1 uses references everywhere.
       It doesn't really make a lot of sense though, because the path might have dots. For instance:
       'a/b.py:a.b' will be stored as ['a/b', 'py:a', 'b']. *)
    type t = Reference.t [@@deriving compare, equal, sexp, hash, show]
  end

  include T

  let create ~path module_name =
    let module_name =
      if
        List.exists (Reference.as_list module_name) ~f:(fun s ->
            String.contains s ':'
            || String.contains s '#'
            || String.contains s '$'
            || String.contains s '@')
      then
        let escape_special_characters s =
          s
          |> String.substr_replace_all ~pattern:":" ~with_:"\\x3a"
          |> String.substr_replace_all ~pattern:"#" ~with_:"\\x23"
          |> String.substr_replace_all ~pattern:"$" ~with_:"\\x24"
          |> String.substr_replace_all ~pattern:"@" ~with_:"\\x40"
        in
        module_name
        |> Reference.as_list
        |> List.map ~f:escape_special_characters
        |> Reference.create_from_list
      else
        module_name
    in
    match path with
    | None -> module_name
    | Some path -> Format.sprintf "%s:%s" path (Reference.show module_name) |> Reference.create


  let to_reference = Fn.id

  let from_reference_unchecked = Fn.id

  let bare_module_name qualifier =
    let parts = Reference.as_list qualifier in
    let reference =
      if List.exists parts ~f:(fun s -> String.contains s ':') then
        parts
        |> String.concat ~sep:"."
        |> String.rsplit2_exn ~on:':'
        |> snd
        |> String.split ~on:'.'
        |> Reference.create_from_list
      else
        qualifier
    in
    let unescape_special_characters s =
      s
      |> String.substr_replace_all ~pattern:"\\x3a" ~with_:":"
      |> String.substr_replace_all ~pattern:"\\x23" ~with_:"#"
      |> String.substr_replace_all ~pattern:"\\x24" ~with_:"$"
      |> String.substr_replace_all ~pattern:"\\x40" ~with_:"@"
    in
    reference
    |> Reference.as_list
    |> List.map ~f:unescape_special_characters
    |> Reference.create_from_list
    |> ModuleName.from_reference_unchecked


  module Map = Map.Make (T)
end

module ModuleQualifierSharedMemoryKey = struct
  type t = ModuleQualifier.t [@@deriving compare]

  let to_string key =
    key |> ModuleQualifier.to_reference |> Analysis.SharedMemoryKeys.ReferenceKey.to_string
end

module ModuleNameSharedMemoryKey = struct
  type t = ModuleName.t [@@deriving compare]

  let to_string key =
    key |> ModuleName.to_reference |> Analysis.SharedMemoryKeys.ReferenceKey.to_string
end

(* Filename for a pyrefly module information file. *)
module ModuleInfoFilename : sig
  type t [@@deriving compare, equal, sexp, hash, show]

  val create : string -> t

  val raw : t -> string
end = struct
  type t = string [@@deriving compare, equal, sexp, hash, show]

  let create = Fn.id

  let raw = Fn.id
end

(* Content of the `pyrefly.pysa.json` file exported by pyrefly. This matches the
   `pyrefly::report::pysa::PysaProjectFile` rust type. *)
module ProjectFile = struct
  module Module = struct
    type t = {
      module_id: ModuleId.t;
      module_name: Reference.t;
      absolute_source_path: ModulePath.t;
          (* Filesystem path to the source file for the module, as seen by the analyzer *)
      relative_source_path: string option; (* Relative path from a root or search path *)
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

  type t = {
    modules: Module.t list;
    builtin_module_ids: ModuleId.t list;
    object_class_refs: GlobalClassId.t list;
    dict_class_refs: GlobalClassId.t list;
    typing_module_ids: ModuleId.t list;
    typing_mapping_class_refs: GlobalClassId.t list;
  }
end

module ClassFieldDeclarationKind = struct
  type t =
    | DeclaredByAnnotation
    | DeclaredWithoutAnnotation
    | AssignedInBody
    | DefinedWithoutAssign
    | DefinedInMethod
  [@@deriving equal, compare, show]

  let from_string = function
    | "DeclaredByAnnotation" -> Ok DeclaredByAnnotation
    | "DeclaredWithoutAnnotation" -> Ok DeclaredWithoutAnnotation
    | "AssignedInBody" -> Ok AssignedInBody
    | "DefinedWithoutAssign" -> Ok DefinedWithoutAssign
    | "DefinedInMethod" -> Ok DefinedInMethod
    | s ->
        Error
          (FormatError.UnexpectedJsonType
             { json = `String s; message = "expected declaration kind" })
end

module CapturedVariable = struct
  type t = {
    name: string;
    outer_function: GlobalCallableId.t;
  }
  [@@deriving equal, show]
end

(* Information from pyrefly about all definitions in a given module, stored as a
   `<root>/definitions/<module>:<id>.json` file. This matches the
   `pyrefly::report::pysa::PysaModuleDefinitions` rust type. *)
module ModuleDefinitionsFile = struct
  module ParentScope = struct
    type t =
      | TopLevel
      | Class of LocalClassId.t
      | Function of FuncDefIndex.t
    [@@deriving equal, show]
  end

  module FunctionParameter = struct
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

  module FunctionParameters = struct
    type t =
      | List of FunctionParameter.t list
      | Ellipsis
      | ParamSpec
    [@@deriving equal, show]
  end

  module FunctionSignature = struct
    type t = {
      parameters: FunctionParameters.t;
      return_annotation: PyreflyType.t;
    }
    [@@deriving equal, show]
  end

  module FunctionDefinition = struct
    type t = {
      name: string;
      name_location: Location.t option;
      local_function_id: LocalFunctionId.t;
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
      overridden_base_method: GlobalCallableId.t option;
      defining_class: GlobalClassId.t option;
      decorator_callees: GlobalCallableId.t list Location.SerializableMap.t;
    }
    [@@deriving equal, show]

    let create_module_toplevel () =
      {
        name = Ast.Statement.toplevel_define_name;
        name_location = None;
        local_function_id = LocalFunctionId.ModuleTopLevel;
        parent = ParentScope.TopLevel;
        undecorated_signatures =
          [
            {
              FunctionSignature.parameters = FunctionParameters.List [];
              return_annotation =
                {
                  PyreflyType.string = "None";
                  scalar_properties = ScalarTypeProperties.none;
                  classes = None;
                };
            };
          ];
        captured_variables = [];
        is_overload = false;
        is_staticmethod = false;
        is_classmethod = false;
        is_property_getter = false;
        is_property_setter = false;
        is_stub = false;
        is_def_statement = false;
        is_toplevel = true;
        is_class_toplevel = false;
        overridden_base_method = None;
        defining_class = None;
        decorator_callees = Location.SerializableMap.empty;
      }


    let create_class_toplevel ~local_class_id =
      {
        name = Ast.Statement.class_toplevel_define_name;
        name_location = None;
        local_function_id = LocalFunctionId.ClassTopLevel local_class_id;
        parent = ParentScope.Class local_class_id;
        undecorated_signatures =
          [
            {
              FunctionSignature.parameters = FunctionParameters.List [];
              return_annotation =
                {
                  PyreflyType.string = "None";
                  scalar_properties = ScalarTypeProperties.none;
                  classes = None;
                };
            };
          ];
        captured_variables = [];
        is_overload = false;
        is_staticmethod = false;
        is_classmethod = false;
        is_property_getter = false;
        is_property_setter = false;
        is_stub = false;
        is_def_statement = false;
        is_toplevel = false;
        is_class_toplevel = true;
        overridden_base_method = None;
        defining_class = None;
        decorator_callees = Location.SerializableMap.empty;
      }
  end

  module ClassMro = struct
    type t =
      | Resolved of GlobalClassId.t list
      | Cyclic
    [@@deriving equal, show]
  end

  module PyreflyClassField = struct
    type t = {
      name: string;
      type_: PyreflyType.t;
      explicit_annotation: string option;
      location: Location.t option;
      declaration_kind: ClassFieldDeclarationKind.t option;
    }
    [@@deriving equal, show]
  end

  module ClassDefinition = struct
    type t = {
      name: string;
      local_class_id: LocalClassId.t;
      name_location: Location.t;
      parent: ParentScope.t;
      bases: GlobalClassId.t list;
      mro: ClassMro.t;
      is_synthesized: bool;
      is_dataclass: bool;
      is_named_tuple: bool;
      is_typed_dict: bool;
      fields: PyreflyClassField.t list;
      decorator_callees: GlobalCallableId.t list Location.SerializableMap.t;
    }
    [@@deriving equal, show]
  end

  module PyreflyGlobalVariable = struct
    type t = {
      name: string;
      type_: PyreflyType.t option;
      location: Location.t;
    }
  end

  type t = {
    (* TODO(T225700656): module_name and source_path are already specified in the Project module. We
       should probably remove those from the file format. *)
    module_id: ModuleId.t;
    function_definitions: FunctionDefinition.t LocalFunctionId.Map.t;
    class_definitions: ClassDefinition.t LocalClassId.Map.t;
    global_variables: PyreflyGlobalVariable.t list;
  }
end

(* Information from pyrefly about type of expressions in a given module, stored as a
   `<root>/type_of_expressions/<module>:<id>.json` file. This matches the
   `pyrefly::report::pysa::PysaModuleTypeOfExpressions` rust type. *)
module ModuleTypeOfExpressions = struct
  module LocalTypeId = struct
    type t = int [@@deriving equal]

    let of_int x = x

    let to_index x = x
  end

  module TypeAtLocation = struct
    type t = {
      location: Location.t;
      type_: LocalTypeId.t;
    }
  end

  module FunctionTypeOfExpressions = struct
    type t = {
      function_id: LocalFunctionId.t;
      types: PyreflyType.t array;
      locations: TypeAtLocation.t list;
    }
  end

  type t = {
    (* TODO(T225700656): module_name and source_path are already specified in the Project module. We
       should probably remove those from the file format. *)
    module_id: ModuleId.t;
    functions: FunctionTypeOfExpressions.t list;
  }
end

(* Mapping from function id to to call graph information. This represents the
   `pyrefly::report::pysa::PysaModuleCallGraphs` rust type. *)
module ModuleCallGraphs = struct
  module PyreflyImplicitReceiver = struct
    type t =
      | TrueWithClassReceiver
      | TrueWithObjectReceiver
      | False

    let is_true = function
      | TrueWithClassReceiver -> true
      | TrueWithObjectReceiver -> true
      | False -> false
  end

  module PyreflyCallTarget = struct
    type t = {
      target: PyreflyTarget.t;
      implicit_receiver: PyreflyImplicitReceiver.t;
      implicit_dunder_call: bool;
      receiver_class: GlobalClassId.t option;
      is_class_method: bool;
      is_static_method: bool;
      return_type: ScalarTypeProperties.t;
    }
  end

  module PyreflyHigherOrderParameter = struct
    type t = {
      index: int;
      call_targets: PyreflyCallTarget.t list;
      unresolved: CallGraph.Unresolved.t;
    }
  end

  module PyreflyHigherOrderParameterMap = struct
    type t = PyreflyHigherOrderParameter.t list

    let empty = []

    let data = Fun.id
  end

  module PyreflyGlobalVariable = struct
    type t = {
      module_id: ModuleId.t;
      name: string;
    }
  end

  module PyreflyCallCallees = struct
    type t = {
      call_targets: PyreflyCallTarget.t list;
      init_targets: PyreflyCallTarget.t list;
      new_targets: PyreflyCallTarget.t list;
      higher_order_parameters: PyreflyHigherOrderParameterMap.t;
      unresolved: CallGraph.Unresolved.t;
    }
  end

  module PyreflyAttributeAccessCallees = struct
    type t = {
      if_called: PyreflyCallCallees.t;
      property_setters: PyreflyCallTarget.t list;
      property_getters: PyreflyCallTarget.t list;
      global_targets: PyreflyGlobalVariable.t list;
      is_attribute: bool;
    }
  end

  module PyreflyIdentifierCallees = struct
    type t = {
      if_called: PyreflyCallCallees.t;
      global_targets: PyreflyGlobalVariable.t list;
      captured_variables: CapturedVariable.t list;
    }
  end

  module PyreflyDefineCallees = struct
    type t = { define_targets: PyreflyCallTarget.t list }
  end

  module PyreflyFormatStringArtificialCallees = struct
    type t = { targets: PyreflyCallTarget.t list }
  end

  module PyreflyFormatStringStringifyCallees = struct
    type t = {
      targets: PyreflyCallTarget.t list;
      unresolved: CallGraph.Unresolved.t;
    }
  end

  module PyreflyReturnShimCallees = struct
    type t = {
      targets: PyreflyCallTarget.t list;
      arguments: CallGraph.ReturnShimCallees.argument_mapping list;
    }
  end

  module PyreflyExpressionCallees = struct
    type t =
      | Call of PyreflyCallCallees.t
      | Identifier of PyreflyIdentifierCallees.t
      | AttributeAccess of PyreflyAttributeAccessCallees.t
      | Define of PyreflyDefineCallees.t
      | FormatStringArtificial of PyreflyFormatStringArtificialCallees.t
      | FormatStringStringify of PyreflyFormatStringStringifyCallees.t
      | Return of PyreflyReturnShimCallees.t
  end

  module CallGraphEdge = struct
    type t = {
      expression_identifier: ExpressionIdentifier.t;
      callees: PyreflyExpressionCallees.t;
    }
  end

  module PyreflyCallGraph = struct
    type t = CallGraphEdge.t list
  end

  type t = {
    module_id: ModuleId.t;
    call_graphs: PyreflyCallGraph.t LocalFunctionId.Map.t;
  }
end

(* Set of type errors parsed from pyrefly. This represents the
   `pyrefly::report::pysa::PysaTypeErrorsFile` rust type. *)
module TypeErrors = struct
  module PyreflyError = struct
    type t = {
      module_name: string;
      module_path: ModulePath.t;
      location: Location.t;
      kind: string;
      message: string;
    }
  end

  type t = { errors: PyreflyError.t list }
end
