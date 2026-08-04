(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* PyreflyTypes contains type definitions used by the Pyrefly Pysa backend. *)

open Core

module FormatError = struct
  type t =
    | UnexpectedJsonType of {
        json: Yojson.Safe.t;
        message: string;
      }
    | UnsupportedVersion of { version: int }
    | UnparsableString of string
  [@@deriving show]
end

(* Unique identifier for a module, assigned by pyrefly. This maps to a source file. *)
module ModuleId : sig
  type t [@@deriving compare, equal, sexp, hash, show]

  (* Number of bits used to store a module id in the packed `ClassId`/`CallableId` encodings. *)
  val bit_width : int

  val from_int : int -> t

  val to_int : t -> int

  val increment : t -> t
end = struct
  type t = int [@@deriving compare, equal, sexp, hash, show]

  let bit_width = 24

  let from_int = Fn.id

  let to_int = Fn.id

  let increment id = id + 1
end

(* Unique identifier for a class within a module, assigned by pyrefly. *)
module LocalClassId : sig
  type t [@@deriving compare, equal, sexp, hash, show]

  (* Number of bits used to store a local class id in the packed `ClassId`/`CallableId`
     encodings. *)
  val bit_width : int

  val from_int : int -> t

  val to_int : t -> int

  val of_string : string -> t

  module Map : Map.S with type Key.t = t
end = struct
  module T = struct
    type t = int [@@deriving compare, equal, sexp, hash, show]
  end

  include T

  let bit_width = 17

  let from_int = Fn.id

  let to_int = Fn.id

  let of_string = Int.of_string

  module Map = Map.Make (T)
end

(* Index of a function definition within a module, assigned by pyrefly. *)
module FuncDefIndex : sig
  type t [@@deriving compare, equal, sexp, hash, show]

  val from_int : int -> t

  val to_int : t -> int

  val of_string : string -> t
end = struct
  type t = int [@@deriving compare, equal, sexp, hash, show]

  let from_int = Fn.id

  let to_int = Fn.id

  let of_string = Int.of_string
end

(* Unique identifier for a class field within a class, assigned by pyrefly. *)
module LocalClassFieldId : sig
  type t [@@deriving compare, equal, sexp, hash, show]

  val from_int : int -> t

  val to_int : t -> int

  val of_string : string -> t
end = struct
  type t = int [@@deriving compare, equal, sexp, hash, show]

  let from_int = Fn.id

  let to_int = Fn.id

  let of_string = Int.of_string
end

(* Unique identifier for a function within a module, which needs to be consistent between here and
   the outputs of Pyrefly because the outputs often use this as the key to associate information
   with (e.g., call graphs). *)
module LocalFunctionId : sig
  type t =
    (* Function declared with a `def` statement. *)
    | Function of FuncDefIndex.t
    (* Implicit function containing all top level statement. *)
    | ModuleTopLevel
    (* Implicit function containing the class body. *)
    | ClassTopLevel of LocalClassId.t
    (* Function-like class field that is not a `def` statement. *)
    | ClassField of {
        class_id: LocalClassId.t;
        field_id: LocalClassFieldId.t;
      }
    (* Decorated target, which represents an artificial function containing all decorators of a
       function, inlined as an expression. For e.g, `@foo` on `def bar()` -> `return foo(bar)` *)
    | FunctionDecoratedTarget of FuncDefIndex.t
  [@@deriving compare, equal, show, sexp]

  val from_string : string -> (t, FormatError.t) result

  val create_function : FuncDefIndex.t -> t

  val is_class_field : t -> bool

  module Map : Map.S with type Key.t = t
end = struct
  module T = struct
    type t =
      | Function of FuncDefIndex.t
      | ModuleTopLevel
      | ClassTopLevel of LocalClassId.t
      | ClassField of {
          class_id: LocalClassId.t;
          field_id: LocalClassFieldId.t;
        }
      | FunctionDecoratedTarget of FuncDefIndex.t
    [@@deriving compare, equal, show, sexp]
  end

  include T

  let from_string string =
    match String.lsplit2 string ~on:':' with
    | None when String.equal string "MTL" -> Ok ModuleTopLevel
    | Some ("F", func_def_index) -> Ok (Function (FuncDefIndex.of_string func_def_index))
    | Some ("CTL", class_id) -> Ok (ClassTopLevel (LocalClassId.of_string class_id))
    | Some ("CF", class_field) -> (
        match String.lsplit2 class_field ~on:':' with
        | Some (class_id, field_id) ->
            Ok
              (ClassField
                 {
                   class_id = LocalClassId.of_string class_id;
                   field_id = LocalClassFieldId.of_string field_id;
                 })
        | None -> Error (FormatError.UnparsableString string))
    | Some ("FDT", func_def_index) ->
        Ok (FunctionDecoratedTarget (FuncDefIndex.of_string func_def_index))
    | _ -> Error (FormatError.UnparsableString string)


  let create_function func_def_index = Function func_def_index

  let is_class_field = function
    | ClassField _ -> true
    | _ -> false


  module Map = Map.Make (T)
end

(* The packed encodings below assume OCaml's native `int` has at least 63 bits (true on all 64-bit
   platforms), so that a 62-bit payload with a zero sign bit fits without overflow. *)
let () = assert (Int.num_bits >= 63)

(* Identifier that uniquely identifies a class across the whole project, packing the module id and
   the local class id into a single integer. `encode` and `decode` are total mutual inverses. *)
module ClassId : sig
  type t [@@deriving compare, equal, sexp, hash, show]

  val to_int : t -> int

  val encode : module_id:ModuleId.t -> LocalClassId.t -> t

  val decode : t -> ModuleId.t * LocalClassId.t

  val module_id : t -> ModuleId.t

  val local_class_id : t -> LocalClassId.t

  module Map : Map.S with type Key.t = t
end = struct
  type t = int [@@deriving sexp, hash]

  let to_int = Fn.id

  let module_id_shift = 35

  let mask bits = (1 lsl bits) - 1

  let encode ~module_id local_class_id =
    let module_id = ModuleId.to_int module_id in
    let local_class_id = LocalClassId.to_int local_class_id in
    assert (module_id >= 0 && module_id <= mask ModuleId.bit_width);
    assert (local_class_id >= 0 && local_class_id <= mask LocalClassId.bit_width);
    (module_id lsl module_id_shift) lor local_class_id


  let module_id_as_int value = (value lsr module_id_shift) land mask ModuleId.bit_width

  let module_id value = ModuleId.from_int (module_id_as_int value)

  let local_class_id value = LocalClassId.from_int (value land mask LocalClassId.bit_width)

  let decode value = module_id value, local_class_id value

  let compare left right =
    match Int.compare (module_id_as_int left) (module_id_as_int right) with
    | 0 -> Int.compare left right
    | result -> result


  let equal = Int.equal

  let pp formatter value =
    let module_id, local_class_id = decode value in
    Format.fprintf
      formatter
      "ClassId(module_id=%d, local_class_id=%d)"
      (ModuleId.to_int module_id)
      (LocalClassId.to_int local_class_id)


  let show = Format.asprintf "%a" pp

  module Map = Map.Make (struct
    type nonrec t = t

    let compare = compare

    let t_of_sexp = t_of_sexp

    let sexp_of_t = sexp_of_t
  end)
end

(* Identifier that uniquely identifies a callable across the whole project, packing the module id
   and the local function id into a single integer. `encode` and `decode` are total mutual
   inverses. *)
module CallableId : sig
  type t [@@deriving compare, equal, sexp, hash, show]

  val to_int : t -> int

  val encode : module_id:ModuleId.t -> LocalFunctionId.t -> t

  val decode : t -> ModuleId.t * LocalFunctionId.t

  val module_id : t -> ModuleId.t

  val local_function_id : t -> LocalFunctionId.t

  val is_class_top_level : t -> bool

  val is_decorated : t -> bool

  val to_decorated : t -> t

  val to_undecorated : t -> t

  val strip_decorated : t -> t

  module Map : Map.S with type Key.t = t
end = struct
  type t = int [@@deriving sexp, hash]

  let to_int = Fn.id

  let tag_shift = 59

  let tag_bits = 3

  let module_id_shift = 35

  let payload_bit_width = 35

  (* A `ClassField` payload packs the class id in the high bits and the field id in the low bits,
     sharing the `payload_bit_width`-wide payload. The field-id width is whatever remains after the
     class id. *)
  let class_field_id_bit_width = payload_bit_width - LocalClassId.bit_width

  let function_tag = 0

  let function_decorated_target_tag = 1

  let module_top_level_tag = 2

  let class_top_level_tag = 3

  let class_field_tag = 4

  let mask bits = (1 lsl bits) - 1

  let encode ~module_id local_function_id =
    let module_id = ModuleId.to_int module_id in
    assert (module_id >= 0 && module_id <= mask ModuleId.bit_width);
    let tag, payload =
      match local_function_id with
      | LocalFunctionId.Function func_def_index ->
          let func_def_index = FuncDefIndex.to_int func_def_index in
          assert (func_def_index >= 0 && func_def_index <= mask payload_bit_width);
          function_tag, func_def_index
      | LocalFunctionId.FunctionDecoratedTarget func_def_index ->
          let func_def_index = FuncDefIndex.to_int func_def_index in
          assert (func_def_index >= 0 && func_def_index <= mask payload_bit_width);
          function_decorated_target_tag, func_def_index
      | LocalFunctionId.ModuleTopLevel -> module_top_level_tag, 0
      | LocalFunctionId.ClassTopLevel local_class_id ->
          let local_class_id = LocalClassId.to_int local_class_id in
          assert (local_class_id >= 0 && local_class_id <= mask LocalClassId.bit_width);
          class_top_level_tag, local_class_id
      | LocalFunctionId.ClassField { class_id; field_id } ->
          let class_id = LocalClassId.to_int class_id in
          let field_id = LocalClassFieldId.to_int field_id in
          assert (class_id >= 0 && class_id <= mask LocalClassId.bit_width);
          assert (field_id >= 0 && field_id <= mask class_field_id_bit_width);
          class_field_tag, (class_id lsl class_field_id_bit_width) lor field_id
    in
    (tag lsl tag_shift) lor (module_id lsl module_id_shift) lor payload


  let module_id_as_int value = (value lsr module_id_shift) land mask ModuleId.bit_width

  let module_id value = ModuleId.from_int (module_id_as_int value)

  let local_function_id value =
    let tag = (value lsr tag_shift) land mask tag_bits in
    let payload = value land mask payload_bit_width in
    if Int.equal tag function_tag then
      LocalFunctionId.Function (FuncDefIndex.from_int payload)
    else if Int.equal tag function_decorated_target_tag then
      LocalFunctionId.FunctionDecoratedTarget (FuncDefIndex.from_int payload)
    else if Int.equal tag module_top_level_tag then
      LocalFunctionId.ModuleTopLevel
    else if Int.equal tag class_top_level_tag then
      LocalFunctionId.ClassTopLevel (LocalClassId.from_int payload)
    else if Int.equal tag class_field_tag then
      LocalFunctionId.ClassField
        {
          class_id =
            LocalClassId.from_int
              ((payload lsr class_field_id_bit_width) land mask LocalClassId.bit_width);
          field_id = LocalClassFieldId.from_int (payload land mask class_field_id_bit_width);
        }
    else
      Format.asprintf "unexpected CallableId tag: %d" tag |> failwith


  let decode value = module_id value, local_function_id value

  let is_class_top_level value =
    Int.equal ((value lsr tag_shift) land mask tag_bits) class_top_level_tag


  let is_decorated value =
    Int.equal ((value lsr tag_shift) land mask tag_bits) function_decorated_target_tag


  let to_decorated value =
    match local_function_id value with
    | LocalFunctionId.Function func_def_index ->
        encode ~module_id:(module_id value) (LocalFunctionId.FunctionDecoratedTarget func_def_index)
    | local_function_id ->
        Format.asprintf
          "CallableId.to_decorated: expected a Function, got %a"
          LocalFunctionId.pp
          local_function_id
        |> failwith


  let to_undecorated value =
    match local_function_id value with
    | LocalFunctionId.FunctionDecoratedTarget func_def_index ->
        encode ~module_id:(module_id value) (LocalFunctionId.Function func_def_index)
    | local_function_id ->
        Format.asprintf
          "CallableId.to_undecorated: expected a FunctionDecoratedTarget, got %a"
          LocalFunctionId.pp
          local_function_id
        |> failwith


  let strip_decorated value =
    match local_function_id value with
    | LocalFunctionId.FunctionDecoratedTarget func_def_index ->
        encode ~module_id:(module_id value) (LocalFunctionId.Function func_def_index)
    | _ -> value


  let compare left right =
    match Int.compare (module_id_as_int left) (module_id_as_int right) with
    | 0 -> Int.compare left right
    | result -> result


  let equal = Int.equal

  let pp formatter value =
    let module_id, local_function_id = decode value in
    Format.fprintf
      formatter
      "CallableId(module_id=%d, %a)"
      (ModuleId.to_int module_id)
      LocalFunctionId.pp
      local_function_id


  let show = Format.asprintf "%a" pp

  module Map = Map.Make (struct
    type nonrec t = t

    let compare = compare

    let t_of_sexp = t_of_sexp

    let sexp_of_t = sexp_of_t
  end)
end

module DisplayApi = struct
  (* Pure id->reference closures used to render target names.

     There are two flavors of callable name: - The *define name* is the actual definition's
     fully-qualified name (`module.Class.method` / `module.function`). It never carries an
     `@decorated` marker. - The *external name* is the user-facing rendering: identical to the
     define name, except that a decorated callable (a `FunctionDecoratedTarget`) gets an
     `@decorated` suffix appended to its last component. *)
  type t = {
    callable_external_name: CallableId.t -> Ast.Reference.t;
    callable_define_name: CallableId.t -> Ast.Reference.t;
    module_name: ModuleId.t -> Ast.Reference.t;
    class_name: ClassId.t -> Ast.Reference.t;
  }

  (* Debug/no-api renderer for pretty-printers that hold no `PyreflyApi` handle; renders ids
     structurally. The define name renders the raw id; the external name appends `@decorated` for
     decorated ids, mirroring how the real builder distinguishes them. *)
  let for_debug =
    let callable_define_name callable_id = Ast.Reference.create (CallableId.show callable_id) in
    {
      callable_define_name;
      callable_external_name =
        (fun callable_id ->
          let define_name = callable_define_name callable_id in
          if CallableId.is_decorated callable_id then
            Ast.Reference.create
              ?prefix:(Ast.Reference.prefix define_name)
              (Ast.Reference.last define_name ^ "@decorated")
          else
            define_name);
      module_name = (fun module_id -> Ast.Reference.create (ModuleId.show module_id));
      class_name = (fun class_id -> Ast.Reference.create (ClassId.show class_id));
    }
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
    class_id: ClassId.t;
    modifiers: TypeModifier.t list;
  }
  [@@deriving equal, compare, show]

  let from_class class_id = { class_id; modifiers = [] }

  let prepend_modifier ~modifier { class_id; modifiers } =
    { class_id; modifiers = modifier :: modifiers }
end

(* Result of extracting class names from a type. *)
module ClassesFromType = struct
  type t = {
    classes: ClassWithModifiers.t list;
    is_exhaustive: bool;
        (* Is there an element (after stripping) that isn't a class name? For instance:
           get_class_name(Union[A, Callable[...])) = { classes = [A], is_exhaustive = false } *)
  }
  [@@deriving equal, compare, show]

  let from_class class_id =
    { classes = [ClassWithModifiers.from_class class_id]; is_exhaustive = true }


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
  type t = {
    string: string;
    scalar_properties: ScalarTypeProperties.t;
    classes: ClassesFromType.t option;
  }
  [@@deriving equal, compare, show]

  let top = { string = "unknown"; scalar_properties = ScalarTypeProperties.none; classes = None }

  (* Pretty print the type, usually meant for the user *)
  let pp_concise formatter { string; _ } =
    (* Technically, this is the fully qualified representation, but we use it as the concise
       representation for now. *)
    Format.fprintf formatter "%s" string


  let show_fully_qualified { string; _ } = string

  let weaken_literals type_ = type_ (* pyrefly already weakens literals before exporting types *)
end

(** Whether a method is an instance method, or a class method, or a static method. *)
module MethodKind = struct
  type t =
    | Static
    | Class
    | Instance
end

module PyreClassSummary = Analysis.ClassSummary

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

module CapturedVariable = struct
  type t = {
    name: string;
    defining_function: CallableId.t;
  }
  [@@deriving compare, equal, hash, sexp, show]
end

module CallableSignature = struct
  type t = {
    module_id: ModuleId.t;
    define_name: Ast.Reference.t;
    location: Ast.Location.t AstResult.t;
    parameters: Ast.Expression.Parameter.t list AstResult.t;
    return_annotation: Ast.Expression.t option AstResult.t;
    decorators: Ast.Expression.t list AstResult.t;
    captures: CapturedVariable.t list;
    method_kind: MethodKind.t option;
    is_stub_like: bool;
  }
end

module ModelQueries = struct
  module FunctionParameter = struct
    type t =
      | PositionalOnly of {
          name: string option;
          position: int;
          annotation: PyreflyType.t;
          has_default: bool;
        }
      | Named of {
          name: string;
          position: int;
          annotation: PyreflyType.t;
          has_default: bool;
        }
      | KeywordOnly of {
          name: string;
          annotation: PyreflyType.t;
          has_default: bool;
        }
      | Variable of {
          name: string option;
          position: int;
        }
      | Keywords of {
          name: string option;
          annotation: PyreflyType.t;
          excluded: string list;
        }
    [@@deriving equal, compare, show]

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
      return_annotation: PyreflyType.t;
    }
    [@@deriving equal, compare, show]
  end

  module Function = struct
    type t = {
      callable_id: CallableId.t;
      define_name: Ast.Reference.t;
      (* If the user-provided name is a re-export, this is the original name. *)
      imported_name: Ast.Reference.t option;
      (* Signature of the function, ignoring all decorators. None when unknown. *)
      (* Note that functions with `@overload` have multiple signatures. *)
      undecorated_signatures: FunctionSignature.t list option;
      is_property_getter: bool;
      is_property_setter: bool;
      is_method: bool;
      location: Ast.Location.t option;
    }
    [@@deriving show]
  end

  module Global = struct
    type t =
      | Class of {
          class_id: ClassId.t;
          class_name: string;
          location: Ast.Location.t option;
        }
      | Module of { module_id: ModuleId.t }
      (* function or method *)
      | Function of Function.t
      (* non-callable class attribute. *)
      | ClassAttribute of {
          class_id: ClassId.t;
          name: Ast.Reference.t;
          location: Ast.Location.t option;
        }
      (* non-callable module global variable. *)
      | ModuleGlobal of {
          name: Ast.Reference.t;
          module_id: ModuleId.t;
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
      | Class c -> Class { c with location = None }
      | Module m -> Module m
      | Function f -> Function { f with location = None }
      | ClassAttribute ca -> ClassAttribute { ca with location = None }
      | ModuleGlobal mg -> ModuleGlobal { mg with location = None }


    let module_id = function
      | Class { class_id; _ } -> ClassId.module_id class_id
      | Module { module_id; _ } -> module_id
      | Function { callable_id; _ } -> CallableId.module_id callable_id
      | ClassAttribute { class_id; _ } -> ClassId.module_id class_id
      | ModuleGlobal { module_id; _ } -> module_id


    let location = function
      | Class { location; _ } -> location
      | Module _ -> None
      | Function { location; _ } -> location
      | ClassAttribute { location; _ } -> location
      | ModuleGlobal { location; _ } -> location
  end

  module ModuleResolutionResult = struct
    type t =
      (* Symbol found in a module *)
      | Resolved of Global.t
      (* Module exists but symbol not found within it *)
      | Unresolved of {
          module_id: ModuleId.t;
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
