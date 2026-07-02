(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* PysaTypes contains type definitions used by the Pyrefly Pysa backend. *)

open Core

(* Fake module containing all implicit "decorated" targets, which are functions that inline
   decorators. *)
let artificial_decorator_define_module = Ast.Reference.create "artificial_decorator_defines"

module SysInfo = struct
  type t = {
    python_version: Configuration.PythonVersion.t;
    platform: string option;
  }
  [@@deriving compare, equal, sexp, hash]

  let pp
      formatter
      { python_version = { Configuration.PythonVersion.major; minor; micro }; platform }
    =
    Format.fprintf
      formatter
      "{ python_version = %d.%d.%d; platform = %s }"
      major
      minor
      micro
      (Option.value platform ~default:"<none>")


  let show = Format.asprintf "%a" pp

  module Set = Stdlib.Set.Make (struct
    type nonrec t = t

    let compare = compare
  end)
end

(* Scalar properties of a type (it is a bool/int/float/etc.) *)
module ScalarTypeProperties = struct
  type t = int [@@deriving compare, equal, sexp, hash]

  let boolean_bit = 0b1

  let integer_bit = 0b10

  let float_bit = 0b100

  let enumeration_bit = 0b1000

  let get_bit pattern value = Int.equal (Int.bit_and pattern value) pattern

  let set_bit pattern value = Int.bit_or pattern value

  let is_boolean = get_bit boolean_bit

  let is_integer = get_bit integer_bit

  let is_float = get_bit float_bit

  let is_enumeration = get_bit enumeration_bit

  let set_boolean flag = set_bit (if flag then boolean_bit else 0)

  let set_integer flag = set_bit (if flag then integer_bit else 0)

  let set_float flag = set_bit (if flag then float_bit else 0)

  let set_enumeration flag = set_bit (if flag then enumeration_bit else 0)

  let pp formatter value =
    let add_if condition tag tags =
      if condition then
        tag :: tags
      else
        tags
    in
    []
    |> add_if (is_enumeration value) "enum"
    |> add_if (is_float value) "float"
    |> add_if (is_integer value) "int"
    |> add_if (is_boolean value) "bool"
    |> String.concat ~sep:"|"
    |> Format.fprintf formatter "{%s}"


  let show = Format.asprintf "%a" pp

  let none = 0

  let unknown = none

  let bool = 0 |> set_bit boolean_bit |> set_bit integer_bit |> set_bit float_bit

  let integer = 0 |> set_bit integer_bit |> set_bit float_bit

  let enumeration = set_bit enumeration_bit 0

  let create ~is_boolean ~is_integer ~is_float ~is_enumeration =
    0
    |> set_boolean is_boolean
    |> set_integer is_integer
    |> set_float is_float
    |> set_enumeration is_enumeration
end

module TypeModifier = struct
  type t =
    | Optional (* Optional[T] *)
    | Coroutine (* Coroutine[.., T] *)
    | Awaitable (* Awaitable[T] *)
    | TypeVariableBound (* TypeVar(.., bound=T) *)
    | TypeVariableConstraint (* TypeVar("T", ..., ...) *)
    | ReadOnly (* ReadOnly[T] *)
    | Type (* type[T] *)
  [@@deriving equal, compare, show]

  let from_string = function
    | "Optional" -> Some Optional
    | "Coroutine" -> Some Coroutine
    | "Awaitable" -> Some Awaitable
    | "TypeVariableBound" -> Some TypeVariableBound
    | "TypeVariableConstraint" -> Some TypeVariableConstraint
    | "ReadOnly" -> Some ReadOnly
    | "Type" -> Some Type
    | _ -> None
end

module ClassWithModifiers = struct
  type t = {
    class_name: string;
    modifiers: TypeModifier.t list;
  }

  let from_class_name class_name = { class_name; modifiers = [] }

  let prepend_modifier ~modifier { class_name; modifiers } =
    { class_name; modifiers = modifier :: modifiers }
end

(* Result of extracting class names from a type. *)
module ClassNamesFromType = struct
  type t = {
    classes: ClassWithModifiers.t list;
    is_exhaustive: bool;
        (* Is there an element (after stripping) that isn't a class name? For instance:
           get_class_name(Union[A, Callable[...])) = { class_names = [A], is_exhaustive = false } *)
  }

  let from_class_name class_name =
    { classes = [ClassWithModifiers.from_class_name class_name]; is_exhaustive = true }


  let not_a_class = { classes = []; is_exhaustive = false }

  let prepend_modifier ~modifier { classes; is_exhaustive } =
    { classes = List.map ~f:(ClassWithModifiers.prepend_modifier ~modifier) classes; is_exhaustive }


  let join left right =
    {
      classes = List.rev_append left.classes right.classes;
      is_exhaustive = left.is_exhaustive && right.is_exhaustive;
    }
end

module PyreflyType = struct
  module ClassWithModifiers = struct
    type t = {
      module_id: int;
      class_id: int;
      modifiers: TypeModifier.t list;
    }
    [@@deriving equal, compare, show]

    let from_class (module_id, class_id) = { module_id; class_id; modifiers = [] }
  end

  module ClassNamesFromType = struct
    type t = {
      classes: ClassWithModifiers.t list;
      is_exhaustive: bool;
    }
    [@@deriving equal, compare, show]

    let from_class (module_id, class_id) =
      { classes = [ClassWithModifiers.from_class (module_id, class_id)]; is_exhaustive = true }
  end

  type t = {
    string: string;
    scalar_properties: ScalarTypeProperties.t;
    class_names: ClassNamesFromType.t option;
  }
  [@@deriving equal, compare, show]

  let top =
    { string = "unknown"; scalar_properties = ScalarTypeProperties.none; class_names = None }
end

(* Minimal abstraction for a type, provided from Pyrefly and used by Pysa. See `ReadOnly.Type` for
   more functions. *)
module PysaType = struct
  (* TODO(T225700656): We currently expose the representation for Pyrefly here instead of exposing
     it in Interprocedural.Pyrefly, because the current module defines other types that depend on
     `PysaType`, such as `FunctionDefinition.t`. The alternative would require to copy/paste all
     these type definitions, which is not ideal. *)
  type t = Pyrefly of PyreflyType.t [@@deriving equal, compare, show]

  let from_pyrefly_type type_ = Pyrefly type_

  let as_pyrefly_type = function
    | Pyrefly type_ -> Some type_


  (* Pretty print the type, usually meant for the user *)
  let pp_concise formatter = function
    | Pyrefly { PyreflyType.string; _ } ->
        (* Technically, this is the fully qualified representation, but we use it as the concise
           representation for now. *)
        Format.fprintf formatter "%s" string


  let show_fully_qualified = function
    | Pyrefly { PyreflyType.string; _ } -> string


  let weaken_literals = function
    | Pyrefly type_ -> Pyrefly type_ (* pyrefly already weakens literals before exporting types *)
end

module PyreClassSummary = ClassSummary

module AstResult = struct
  type 'a t =
    | Some of 'a
    | FailedToLoad (* callable in a module that pyrefly failed to load *)
    | ParseError (* callable in a module that failed to parse *)
    | TestFile (* callable in a module marked with is_test = true *)
    | Synthesized (* callable in a synthesized class or function *)
    | Pyre1NotFound (* callable not found - only raised when using pyre1 *)

  let to_option = function
    | FailedToLoad -> None
    | ParseError -> None
    | TestFile -> None
    | Synthesized -> None
    | Pyre1NotFound -> None
    | Some ast -> Some ast


  let value_exn ~message = function
    | Some value -> value
    | FailedToLoad -> Format.sprintf "%s (reason: failed to load source)" message |> failwith
    | ParseError -> Format.sprintf "%s (reason: parser error)" message |> failwith
    | TestFile -> Format.sprintf "%s (reason: within a test file)" message |> failwith
    | Synthesized -> Format.sprintf "%s (reason: synthesized function)" message |> failwith
    | Pyre1NotFound -> Format.sprintf "%s (reason: not found)" message |> failwith


  let map ~f = function
    | Some ast -> Some (f ast)
    | FailedToLoad -> FailedToLoad
    | ParseError -> ParseError
    | TestFile -> TestFile
    | Synthesized -> Synthesized
    | Pyre1NotFound -> Pyre1NotFound


  let map_node ~f = function
    | Some { Ast.Node.value = ast; location } -> Some { Ast.Node.value = f ast; location }
    | FailedToLoad -> FailedToLoad
    | ParseError -> ParseError
    | TestFile -> TestFile
    | Synthesized -> Synthesized
    | Pyre1NotFound -> Pyre1NotFound
end

(** Whether a method is an instance method, or a class method, or a static method. *)
module MethodKind = struct
  type t =
    | Static
    | Class
    | Instance
end

module CallableSignature = struct
  type t = {
    qualifier: Ast.Reference.t;
    define_name: Ast.Reference.t;
    location: Ast.Location.t AstResult.t;
    parameters: Ast.Expression.Parameter.t list AstResult.t;
    return_annotation: Ast.Expression.t option AstResult.t;
    decorators: Ast.Expression.t list AstResult.t;
    captures: TaintAccessPath.CapturedVariable.t list;
    method_kind: MethodKind.t option;
    is_stub_like: bool;
  }
end

module MethodReference = struct
  type t =
    | Pyrefly of {
        define_name: Ast.Reference.t;
        is_property_setter: bool;
      }
  [@@deriving show]

  let class_name = function
    | Pyrefly { define_name; _ } ->
        define_name |> Ast.Reference.prefix |> Option.value_exn ~message:"Expect a method name"
end

module ModelQueries = struct
  module FunctionParameter = struct
    type t =
      | PositionalOnly of {
          name: string option;
          position: int;
          annotation: PysaType.t;
          has_default: bool;
        }
      | Named of {
          name: string;
          position: int;
          annotation: PysaType.t;
          has_default: bool;
        }
      | KeywordOnly of {
          name: string;
          annotation: PysaType.t;
          has_default: bool;
        }
      | Variable of {
          name: string option;
          position: int;
        }
      | Keywords of {
          name: string option;
          annotation: PysaType.t;
          excluded: string list;
        }
    [@@deriving equal, compare, show]

    let root = function
      | PositionalOnly { name; position; _ } ->
          let name =
            match name with
            | Some name -> name
            | None -> Format.sprintf "__arg%d" position
          in
          TaintAccessPath.Root.PositionalParameter { position; name; positional_only = true }
      | Named { name; position; _ } ->
          TaintAccessPath.Root.PositionalParameter { position; name; positional_only = false }
      | KeywordOnly { name; _ } -> TaintAccessPath.Root.NamedParameter { name }
      | Variable { position; _ } -> TaintAccessPath.Root.StarParameter { position }
      | Keywords { excluded; _ } ->
          TaintAccessPath.Root.StarStarParameter
            { excluded = Ast.Identifier.SerializableSet.of_list excluded }


    let annotation = function
      | PositionalOnly { annotation; _ } -> Some annotation
      | Named { annotation; _ } -> Some annotation
      | KeywordOnly { annotation; _ } -> Some annotation
      | Variable _ -> None
      | Keywords { annotation; _ } -> Some annotation


    let name = function
      | PositionalOnly { name; _ } -> name
      | Named { name; _ } -> Some name
      | KeywordOnly { name; _ } -> Some name
      | Variable { name; _ } -> name
      | Keywords { name; _ } -> name


    let has_default = function
      | PositionalOnly { has_default; _ }
      | Named { has_default; _ }
      | KeywordOnly { has_default; _ } ->
          has_default
      | _ -> false
  end

  module FunctionParameters = struct
    type t =
      | List of FunctionParameter.t list
      | Ellipsis
      | ParamSpec
    [@@deriving equal, compare, show]
  end

  module FunctionSignature = struct
    type t = {
      parameters: FunctionParameters.t;
      return_annotation: PysaType.t;
    }
    [@@deriving equal, compare, show]
  end

  module Function = struct
    type t = {
      define_name: Ast.Reference.t;
      (* If the user-provided name is a re-export, this is the original name. *)
      imported_name: Ast.Reference.t option;
      (* Signature of the function, ignoring all decorators. None when unknown. *)
      (* Note that functions with `@overload` have multiple signatures. *)
      undecorated_signatures: FunctionSignature.t list option;
      is_property_getter: bool;
      is_property_setter: bool;
      is_method: bool;
      module_qualifier: Ast.Reference.t option;
      location: Ast.Location.t option;
    }
    [@@deriving show]
  end

  module Global = struct
    type t =
      | Class of {
          class_name: string;
          module_qualifier: Ast.Reference.t option;
          location: Ast.Location.t option;
        }
      | Module of { qualifier: Ast.Reference.t }
      (* function or method *)
      | Function of Function.t
      (* non-callable class attribute. *)
      | ClassAttribute of {
          name: Ast.Reference.t;
          module_qualifier: Ast.Reference.t option;
          location: Ast.Location.t option;
        }
      (* non-callable module global variable. *)
      | ModuleGlobal of {
          name: Ast.Reference.t;
          module_qualifier: Ast.Reference.t option;
          location: Ast.Location.t option;
        }
      (* class attribute exists, but type is unknown. *)
      | UnknownClassAttribute of {
          name: Ast.Reference.t;
          module_qualifier: Ast.Reference.t option;
          location: Ast.Location.t option;
        }
      (* module global exists, but type is unknown. *)
      | UnknownModuleGlobal of {
          name: Ast.Reference.t;
          module_qualifier: Ast.Reference.t option;
          location: Ast.Location.t option;
        }
    [@@deriving show]

    let is_module = function
      | Module _ -> true
      | _ -> false


    let is_class = function
      | Class _ -> true
      | _ -> false


    let strip_location_and_module = function
      | Class { class_name; _ } -> Class { class_name; module_qualifier = None; location = None }
      | Module { qualifier } -> Module { qualifier }
      | Function f -> Function { f with module_qualifier = None; location = None }
      | ClassAttribute { name; _ } ->
          ClassAttribute { name; module_qualifier = None; location = None }
      | ModuleGlobal { name; _ } -> ModuleGlobal { name; module_qualifier = None; location = None }
      | UnknownClassAttribute { name; _ } ->
          UnknownClassAttribute { name; module_qualifier = None; location = None }
      | UnknownModuleGlobal { name; _ } ->
          UnknownModuleGlobal { name; module_qualifier = None; location = None }


    let module_qualifier = function
      | Class { module_qualifier; _ } -> module_qualifier
      | Module { qualifier } -> Some qualifier
      | Function { module_qualifier; _ } -> module_qualifier
      | ClassAttribute { module_qualifier; _ } -> module_qualifier
      | ModuleGlobal { module_qualifier; _ } -> module_qualifier
      | UnknownClassAttribute { module_qualifier; _ } -> module_qualifier
      | UnknownModuleGlobal { module_qualifier; _ } -> module_qualifier


    let location = function
      | Class { location; _ } -> location
      | Module _ -> None
      | Function { location; _ } -> location
      | ClassAttribute { location; _ } -> location
      | ModuleGlobal { location; _ } -> location
      | UnknownClassAttribute { location; _ } -> location
      | UnknownModuleGlobal { location; _ } -> location
  end

  module ModuleResolutionResult = struct
    type t =
      (* Symbol found in a module *)
      | Resolved of Global.t
      (* Module exists but symbol not found within it *)
      | Unresolved of {
          module_qualifier: Ast.Reference.t;
          module_name: Ast.Reference.t; (* Bare module name *)
          suffix: Ast.Reference.t; (* Unresolved part of the name *)
        }
  end

  module ResolutionResult = struct
    type t =
      (* At least one module prefix matched. `results` contains one entry per matching definition; a
         single module can contribute multiple entries when the same name is defined more than once
         in a file, and an entry may also indicate the module exists but the symbol was not found
         (Unresolved). *)
      | ModuleFound of {
          (* Bare module name for all results. This is not necessarily a valid module qualifier. *)
          module_name: Ast.Reference.t option;
          results: ModuleResolutionResult.t list;
        }
      (* No module prefix matched at all *)
      | BaseModuleNotFound
  end

  let property_decorators =
    Set.union Recognized.property_decorators Recognized.classproperty_decorators


  let mangle_top_level_name name =
    Ast.Reference.map_last
      ~f:(function
        | "__top_level__" -> Ast.Statement.toplevel_define_name
        | "__class_top_level__" -> Ast.Statement.class_toplevel_define_name
        | identifier -> identifier)
      name


  let demangle_class_attribute name =
    let parts = Ast.Reference.as_list name in
    if List.exists parts ~f:(String.equal "__class__") then
      match List.rev parts with
      | attribute :: "__class__" :: rest ->
          attribute :: rest |> List.rev |> Ast.Reference.create_from_list
      | _ -> name
    else
      name


  let has_class_attribute_form name =
    let name = Ast.Reference.as_list name in
    List.exists ~f:(String.equal "__class__") name
    &&
    match List.rev name with
    | _ :: "__class__" :: _ -> true
    | _ -> false


  let mangle_class_attribute name =
    let parts = Ast.Reference.as_list name in
    match List.rev parts with
    | attribute :: rest ->
        attribute :: "__class__" :: rest |> List.rev |> Ast.Reference.create_from_list
    | [] -> name
end
