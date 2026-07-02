(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* PysaTypes contains type definitions used by the Pyrefly Pysa backend. *)

(* Fake module containing all implicit "decorated" targets, which are functions that inline
   decorators. *)
val artificial_decorator_define_module : Ast.Reference.t

module SysInfo : sig
  type t = {
    python_version: Configuration.PythonVersion.t;
    platform: string option;
  }
  [@@deriving compare, equal, show, sexp, hash]

  module Set : Stdlib.Set.S with type elt = t
end

(* Scalar properties of a type (it is a bool/int/float/etc.) *)
module ScalarTypeProperties : sig
  type t [@@deriving compare, equal, sexp, hash, show]

  val none : t

  val unknown : t

  val bool : t

  val integer : t

  val enumeration : t

  val is_boolean : t -> bool

  val is_integer : t -> bool

  val is_float : t -> bool

  val is_enumeration : t -> bool

  val create : is_boolean:bool -> is_integer:bool -> is_float:bool -> is_enumeration:bool -> t
end

module TypeModifier : sig
  type t =
    | Optional (* Optional[T] *)
    | Coroutine (* Coroutine[.., T] *)
    | Awaitable (* Awaitable[T] *)
    | TypeVariableBound (* TypeVar(.., bound=T) *)
    | TypeVariableConstraint (* TypeVar("T", ..., ...) *)
    | ReadOnly (* ReadOnly[T] *)
    | Type (* type[T] *)
  [@@deriving equal, compare, show]

  val from_string : string -> t option
end

module ClassWithModifiers : sig
  type t = {
    class_name: string;
    modifiers: TypeModifier.t list;
  }

  val from_class_name : string -> t

  val prepend_modifier : modifier:TypeModifier.t -> t -> t
end

(* Result of extracting class names from a type. *)
module ClassNamesFromType : sig
  type t = {
    classes: ClassWithModifiers.t list;
    is_exhaustive: bool;
        (* Is there an element (after stripping) that isn't a class name? For instance:
           get_class_name(Union[A, Callable[...])) = { class_names = [A], is_exhaustive = false } *)
  }

  val from_class_name : string -> t

  val not_a_class : t

  val prepend_modifier : modifier:TypeModifier.t -> t -> t

  val join : t -> t -> t
end

module PyreflyType : sig
  module ClassWithModifiers : sig
    type t = {
      module_id: int;
      class_id: int;
      modifiers: TypeModifier.t list;
    }
    [@@deriving equal, compare, show]

    val from_class : int * int -> t
  end

  module ClassNamesFromType : sig
    type t = {
      classes: ClassWithModifiers.t list;
      is_exhaustive: bool;
    }
    [@@deriving equal, compare, show]

    val from_class : int * int -> t
  end

  type t = {
    string: string;
    scalar_properties: ScalarTypeProperties.t;
    class_names: ClassNamesFromType.t option;
  }
  [@@deriving equal, compare, show]

  val top : t
end

(* Minimal abstraction for a type, provided from Pyrefly and used by Pysa. See `ReadOnly.Type` for
   more functions. *)
module PysaType : sig
  type t = Pyrefly of PyreflyType.t [@@deriving equal, compare, show]

  val from_pyrefly_type : PyreflyType.t -> t

  val as_pyrefly_type : t -> PyreflyType.t option

  (* Pretty print the type, usually meant for the user *)
  val pp_concise : Format.formatter -> t -> unit

  val show_fully_qualified : t -> string

  val weaken_literals : t -> t
end

module PyreClassSummary = ClassSummary

module AstResult : sig
  type 'a t =
    | Some of 'a
    | FailedToLoad (* callable in a module that pyrefly failed to load *)
    | ParseError (* callable in a module that failed to parse *)
    | TestFile (* callable in a module marked with is_test = true *)
    | Synthesized (* callable in a synthesized class or function *)
    | Pyre1NotFound (* callable not found - only raised when using pyre1 *)

  val to_option : 'a t -> 'a option

  val value_exn : message:string -> 'a t -> 'a

  val map : f:('a -> 'b) -> 'a t -> 'b t

  val map_node : f:('a -> 'b) -> 'a Ast.Node.t t -> 'b Ast.Node.t t
end

(** Whether a method is an instance method, or a class method, or a static method. *)
module MethodKind : sig
  type t =
    | Static
    | Class
    | Instance
end

module CallableSignature : sig
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

module MethodReference : sig
  type t =
    | Pyrefly of {
        define_name: Ast.Reference.t;
        is_property_setter: bool;
      }
  [@@deriving show]

  val class_name : t -> Ast.Reference.t
end

module ModelQueries : sig
  module FunctionParameter : sig
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

    val annotation : t -> PysaType.t option

    val root : t -> TaintAccessPath.Root.t

    val name : t -> string option

    val has_default : t -> bool
  end

  module FunctionParameters : sig
    type t =
      | List of FunctionParameter.t list
      | Ellipsis
      | ParamSpec
    [@@deriving equal, compare, show]
  end

  module FunctionSignature : sig
    type t = {
      parameters: FunctionParameters.t;
      return_annotation: PysaType.t;
    }
    [@@deriving equal, compare, show]
  end

  module Function : sig
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

  module Global : sig
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

    val is_module : t -> bool

    val is_class : t -> bool

    val strip_location_and_module : t -> t

    val module_qualifier : t -> Ast.Reference.t option

    val location : t -> Ast.Location.t option
  end

  module ModuleResolutionResult : sig
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

  module ResolutionResult : sig
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

  val property_decorators : Core.String.Set.t

  val mangle_top_level_name : Ast.Reference.t -> Ast.Reference.t

  val demangle_class_attribute : Ast.Reference.t -> Ast.Reference.t

  val has_class_attribute_form : Ast.Reference.t -> bool

  val mangle_class_attribute : Ast.Reference.t -> Ast.Reference.t
end
