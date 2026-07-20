(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open OUnit2
open Interprocedural
open Ast
module ModulePath = PyreflyApi.ModulePath
module ModuleId = PyreflyApi.ModuleId
module ModuleQualifier = PyreflyApi.ModuleQualifier
module ModuleDefinitionsFile = PyreflyApi.ModuleDefinitionsFile
module FuncDefIndex = PyreflyApi.FuncDefIndex
module FullyQualifiedName = PyreflyApi.FullyQualifiedName

module ModuleQualifierInput = struct
  type t = {
    module_name: string;
    path: ModulePath.t;
    id: int;
  }
end

module ModuleQualifierExpected = struct
  type t = {
    qualifier: string;
    module_name: string;
    source_path: string option;
    id: int;
  }
  [@@deriving show]

  let _ = pp
end

let filesystem_module_path path =
  ModulePath.Filesystem (ArtifactPath.create (PyrePath.create_absolute path))


let test_module_qualifiers _ =
  let open Option.Monad_infix in
  let assert_module_qualifiers ?(add_toplevel_modules = false) ~inputs ~expected () =
    let pyrefly_directory = PyrePath.create_absolute "/pyrefly" in
    let make_module_definitions { ModuleQualifierInput.module_name; path; id } =
      {
        PyreflyApi.ProjectFile.Module.module_id = ModuleId.from_int id;
        module_name = Reference.create module_name;
        absolute_source_path = path;
        relative_source_path = None;
        info_filename = None;
        python_version = { Configuration.PythonVersion.major = 3; minor = 12; micro = 0 };
        platform = "linux";
        is_test = false;
        is_interface = false;
        is_init = false;
        is_internal = false;
        failed_to_load = false;
      }
    in
    let make_testing_module { ModuleQualifierExpected.module_name; source_path; id; qualifier = _ } =
      {
        PyreflyApi.Testing.Module.module_id = ModuleId.from_int id;
        module_name = Reference.create module_name;
        absolute_source_path = source_path >>| PyrePath.create_absolute >>| ArtifactPath.create;
        relative_source_path = None;
        pyrefly_info_filename = None;
        sys_info =
          {
            PyreflyApi.SysInfo.python_version =
              { Configuration.PythonVersion.major = 3; minor = 12; micro = 0 };
            platform = Some "linux";
          };
        is_test = false;
        is_stub = false;
        is_internal = false;
        failed_to_load = false;
      }
    in
    let to_string map =
      map |> Map.to_alist |> [%show: (ModuleQualifier.t * PyreflyApi.Testing.Module.t) list]
    in
    let inputs = List.map ~f:make_module_definitions inputs in
    let expected =
      expected
      |> List.map ~f:(fun ({ ModuleQualifierExpected.qualifier; _ } as test_module) ->
             ( ModuleQualifier.from_reference_unchecked (Reference.create qualifier),
               make_testing_module test_module ))
      |> PyreflyApi.ModuleQualifier.Map.of_alist_exn
    in
    let actual =
      PyreflyApi.Testing.create_module_qualifiers ~pyrefly_directory ~add_toplevel_modules inputs
    in
    assert_equal
      ~cmp:(ModuleQualifier.Map.equal PyreflyApi.Testing.Module.equal)
      ~printer:to_string
      expected
      actual
  in
  assert_module_qualifiers
    ~inputs:
      [
        {
          ModuleQualifierInput.module_name = "a";
          path = filesystem_module_path "/root/a.py";
          id = 0;
        };
        {
          ModuleQualifierInput.module_name = "b";
          path = filesystem_module_path "/root/b.py";
          id = 1;
        };
        {
          ModuleQualifierInput.module_name = "c";
          path = filesystem_module_path "/root/c.py";
          id = 2;
        };
      ]
    ~expected:
      [
        {
          ModuleQualifierExpected.qualifier = "a";
          module_name = "a";
          source_path = Some "/root/a.py";
          id = 0;
        };
        {
          ModuleQualifierExpected.qualifier = "b";
          module_name = "b";
          source_path = Some "/root/b.py";
          id = 1;
        };
        {
          ModuleQualifierExpected.qualifier = "c";
          module_name = "c";
          source_path = Some "/root/c.py";
          id = 2;
        };
      ]
    ();
  assert_module_qualifiers
    ~inputs:
      [
        {
          ModuleQualifierInput.module_name = "a";
          path = filesystem_module_path "/root/a/__init__.py";
          id = 0;
        };
        {
          ModuleQualifierInput.module_name = "a.b";
          path = filesystem_module_path "/root/a/b/__init__.py";
          id = 1;
        };
        {
          ModuleQualifierInput.module_name = "a.b.c";
          path = filesystem_module_path "/root/a/b/c.py";
          id = 2;
        };
      ]
    ~expected:
      [
        {
          ModuleQualifierExpected.qualifier = "a";
          module_name = "a";
          source_path = Some "/root/a/__init__.py";
          id = 0;
        };
        {
          ModuleQualifierExpected.qualifier = "a.b";
          module_name = "a.b";
          source_path = Some "/root/a/b/__init__.py";
          id = 1;
        };
        {
          ModuleQualifierExpected.qualifier = "a.b.c";
          module_name = "a.b.c";
          source_path = Some "/root/a/b/c.py";
          id = 2;
        };
      ]
    ();
  (* Conflicting module name due to multiple roots *)
  assert_module_qualifiers
    ~inputs:
      [
        {
          ModuleQualifierInput.module_name = "a.b";
          path = filesystem_module_path "/first_root/a/b.py";
          id = 0;
        };
        {
          ModuleQualifierInput.module_name = "a.b";
          path = filesystem_module_path "/second_root/a/b.py";
          id = 1;
        };
      ]
    ~expected:
      [
        {
          ModuleQualifierExpected.qualifier = "first_root/a/b.py:a.b";
          module_name = "a.b";
          source_path = Some "/first_root/a/b.py";
          id = 0;
        };
        {
          ModuleQualifierExpected.qualifier = "second_root/a/b.py:a.b";
          module_name = "a.b";
          source_path = Some "/second_root/a/b.py";
          id = 1;
        };
      ]
    ();
  (* Conflicting module name due to stub files *)
  assert_module_qualifiers
    ~inputs:
      [
        {
          ModuleQualifierInput.module_name = "a.b";
          path = filesystem_module_path "/root/a/b.py";
          id = 0;
        };
        {
          ModuleQualifierInput.module_name = "a.b";
          path = filesystem_module_path "/root/a/b.pyi";
          id = 1;
        };
      ]
    ~expected:
      [
        {
          ModuleQualifierExpected.qualifier = "b.py:a.b";
          module_name = "a.b";
          source_path = Some "/root/a/b.py";
          id = 0;
        };
        {
          ModuleQualifierExpected.qualifier = "b.pyi:a.b";
          module_name = "a.b";
          source_path = Some "/root/a/b.pyi";
          id = 1;
        };
      ]
    ();
  (* Multiple modules with conflicts *)
  assert_module_qualifiers
    ~inputs:
      [
        {
          ModuleQualifierInput.module_name = "a.b";
          path = filesystem_module_path "/root/a/b.py";
          id = 0;
        };
        {
          ModuleQualifierInput.module_name = "a.b";
          path = filesystem_module_path "/root/a/b.pyi";
          id = 1;
        };
        {
          ModuleQualifierInput.module_name = "a.c";
          path = filesystem_module_path "/root/a/c.py";
          id = 2;
        };
        {
          ModuleQualifierInput.module_name = "a.c";
          path = filesystem_module_path "/root/a/c.pyi";
          id = 3;
        };
      ]
    ~expected:
      [
        {
          ModuleQualifierExpected.qualifier = "b.py:a.b";
          module_name = "a.b";
          source_path = Some "/root/a/b.py";
          id = 0;
        };
        {
          ModuleQualifierExpected.qualifier = "b.pyi:a.b";
          module_name = "a.b";
          source_path = Some "/root/a/b.pyi";
          id = 1;
        };
        {
          ModuleQualifierExpected.qualifier = "c.py:a.c";
          module_name = "a.c";
          source_path = Some "/root/a/c.py";
          id = 2;
        };
        {
          ModuleQualifierExpected.qualifier = "c.pyi:a.c";
          module_name = "a.c";
          source_path = Some "/root/a/c.pyi";
          id = 3;
        };
      ]
    ();
  (* __init__.py vs module.py *)
  assert_module_qualifiers
    ~inputs:
      [
        {
          ModuleQualifierInput.module_name = "a.b";
          path = filesystem_module_path "/first_root/a/b.py";
          id = 0;
        };
        {
          ModuleQualifierInput.module_name = "a.b";
          path = filesystem_module_path "/second_root/a/b/__init__.py";
          id = 1;
        };
      ]
    ~expected:
      [
        {
          ModuleQualifierExpected.qualifier = "b.py:a.b";
          module_name = "a.b";
          source_path = Some "/first_root/a/b.py";
          id = 0;
        };
        {
          ModuleQualifierExpected.qualifier = "__init__.py:a.b";
          module_name = "a.b";
          source_path = Some "/second_root/a/b/__init__.py";
          id = 1;
        };
      ]
    ();
  (* 3 conflicts *)
  assert_module_qualifiers
    ~inputs:
      [
        {
          ModuleQualifierInput.module_name = "a.b";
          path = filesystem_module_path "/first_root/a/b.py";
          id = 0;
        };
        {
          ModuleQualifierInput.module_name = "a.b";
          path = filesystem_module_path "/second_root/a/b/__init__.py";
          id = 1;
        };
        {
          ModuleQualifierInput.module_name = "a.b";
          path = filesystem_module_path "/third_root/a/b.py";
          id = 2;
        };
      ]
    ~expected:
      [
        {
          ModuleQualifierExpected.qualifier = "first_root/a/b.py:a.b";
          module_name = "a.b";
          source_path = Some "/first_root/a/b.py";
          id = 0;
        };
        {
          ModuleQualifierExpected.qualifier = "a/b/__init__.py:a.b";
          module_name = "a.b";
          source_path = Some "/second_root/a/b/__init__.py";
          id = 1;
        };
        {
          ModuleQualifierExpected.qualifier = "third_root/a/b.py:a.b";
          module_name = "a.b";
          source_path = Some "/third_root/a/b.py";
          id = 2;
        };
      ]
    ();
  (* filesystem vs typeshed *)
  assert_module_qualifiers
    ~inputs:
      [
        {
          ModuleQualifierInput.module_name = "typing";
          path = filesystem_module_path "/root/stdlib/typing.py";
          id = 0;
        };
        {
          ModuleQualifierInput.module_name = "typing";
          path = ModulePath.BundledTypeshed (PyrePath.create_absolute "typing.py");
          id = 1;
        };
      ]
    ~expected:
      [
        {
          ModuleQualifierExpected.qualifier = "stdlib/typing.py:typing";
          module_name = "typing";
          source_path = Some "/root/stdlib/typing.py";
          id = 0;
        };
        {
          ModuleQualifierExpected.qualifier = "typeshed://typing.py:typing";
          module_name = "typing";
          source_path = Some "/pyrefly/typeshed/typing.py";
          id = 1;
        };
      ]
    ();
  (* filesystem vs namespace *)
  assert_module_qualifiers
    ~inputs:
      [
        {
          ModuleQualifierInput.module_name = "a";
          path = filesystem_module_path "/root/a.py";
          id = 0;
        };
        {
          ModuleQualifierInput.module_name = "a";
          path = ModulePath.Namespace (PyrePath.create_absolute "a.py");
          id = 1;
        };
      ]
    ~expected:
      [
        {
          ModuleQualifierExpected.qualifier = "root/a.py:a";
          module_name = "a";
          source_path = Some "/root/a.py";
          id = 0;
        };
        {
          ModuleQualifierExpected.qualifier = "namespace://a.py:a";
          module_name = "a";
          source_path = None;
          id = 1;
        };
      ]
    ();
  (* filesystem vs memory *)
  assert_module_qualifiers
    ~inputs:
      [
        {
          ModuleQualifierInput.module_name = "a";
          path = filesystem_module_path "/root/a.py";
          id = 0;
        };
        {
          ModuleQualifierInput.module_name = "a";
          path = ModulePath.Memory (PyrePath.create_absolute "a.py");
          id = 1;
        };
      ]
    ~expected:
      [
        {
          ModuleQualifierExpected.qualifier = "root/a.py:a";
          module_name = "a";
          source_path = Some "/root/a.py";
          id = 0;
        };
        {
          ModuleQualifierExpected.qualifier = "memory://a.py:a";
          module_name = "a";
          source_path = None;
          id = 1;
        };
      ]
    ();
  (* add implicit toplevel modules *)
  assert_module_qualifiers
    ~add_toplevel_modules:true
    ~inputs:
      [
        {
          ModuleQualifierInput.module_name = "a.b";
          path = filesystem_module_path "/root/a/b.py";
          id = 0;
        };
      ]
    ~expected:
      [
        { ModuleQualifierExpected.qualifier = "a"; module_name = "a"; source_path = None; id = 1 };
        {
          ModuleQualifierExpected.qualifier = "a.b";
          module_name = "a.b";
          source_path = Some "/root/a/b.py";
          id = 0;
        };
      ]
    ();
  ()


let test_fully_qualified_names _ =
  let location_at_line line =
    let position = { Ast.Location.line; column = 0 } in
    { Ast.Location.start = position; stop = position }
  in
  let assert_fully_qualified_names ?(module_name = "test") ?(modules = []) ~definitions ~expected ()
    =
    let module_qualifier =
      module_name |> Reference.create |> ModuleQualifier.from_reference_unchecked
    in
    let existing_modules = List.map modules ~f:Reference.create |> Reference.Set.of_list in
    let class_definitions =
      definitions
      |> List.filter_map ~f:(fun definition ->
             match definition with
             | PyreflyApi.Testing.Definition.Class
                 ({ ModuleDefinitionsFile.ClassDefinition.local_class_id; _ } as class_definition)
               ->
                 Some (local_class_id, class_definition)
             | _ -> None)
      |> PyreflyApi.LocalClassId.Map.of_alist_exn
    in
    let function_definitions =
      definitions
      |> List.filter_map ~f:(fun definition ->
             match definition with
             | PyreflyApi.Testing.Definition.Function
                 ({ ModuleDefinitionsFile.FunctionDefinition.local_function_id; _ } as
                 function_definition) ->
                 Some (local_function_id, function_definition)
             | _ -> None)
      |> PyreflyApi.LocalFunctionId.Map.of_alist_exn
    in
    let actual =
      PyreflyApi.Testing.create_fully_qualified_names
        ~module_qualifier
        ~module_exists:(fun qualifier ->
          Set.mem existing_modules (ModuleQualifier.to_reference qualifier))
        ~class_definitions
        ~function_definitions
      |> List.map ~f:(fun { PyreflyApi.Testing.QualifiedDefinition.qualified_name; _ } ->
             qualified_name |> FullyQualifiedName.to_reference |> Reference.show)
    in
    assert_equal ~cmp:[%compare.equal: string list] ~printer:[%show: string list] expected actual
  in
  let create_function
      ?(parent = ModuleDefinitionsFile.ParentScope.TopLevel)
      ?(is_overload = false)
      ?(is_property_getter = false)
      ?(is_property_setter = false)
      ~line
      name
    =
    PyreflyApi.Testing.Definition.Function
      {
        ModuleDefinitionsFile.FunctionDefinition.name;
        name_location = Some (location_at_line line);
        local_function_id = PyreflyApi.LocalFunctionId.create_function (FuncDefIndex.from_int line);
        parent;
        is_overload;
        undecorated_signatures = [];
        captured_variables = [];
        is_staticmethod = false;
        is_classmethod = false;
        is_property_getter;
        is_property_setter;
        is_stub = false;
        is_def_statement = true;
        is_toplevel = false;
        is_class_toplevel = false;
        overridden_base_method = None;
        defining_class = None;
        decorator_callees = Location.SerializableMap.empty;
      }
  in
  let create_class ?(parent = ModuleDefinitionsFile.ParentScope.TopLevel) ~line name =
    PyreflyApi.Testing.Definition.Class
      {
        ModuleDefinitionsFile.ClassDefinition.name;
        name_location = location_at_line line;
        parent;
        local_class_id = PyreflyApi.LocalClassId.from_int line;
        bases = [];
        mro = ModuleDefinitionsFile.ClassMro.Resolved [];
        is_synthesized = false;
        is_dataclass = false;
        is_named_tuple = false;
        is_typed_dict = false;
        fields = [];
        decorator_callees = Location.SerializableMap.empty;
      }
  in
  let class_parent ~line =
    ModuleDefinitionsFile.ParentScope.Class (PyreflyApi.LocalClassId.from_int line)
  in
  let function_parent ~line =
    ModuleDefinitionsFile.ParentScope.Function (FuncDefIndex.from_int line)
  in
  assert_fully_qualified_names
    ~definitions:
      [
        create_function ~line:1 "foo";
        create_function ~line:2 "bar";
        create_class ~line:3 "MyClass";
        create_function ~line:4 ~parent:(class_parent ~line:3) "__init__";
        create_function ~line:5 ~parent:(class_parent ~line:3) "method";
      ]
    ~expected:
      [
        "test.$toplevel";
        "test.MyClass";
        "test.MyClass.$class_toplevel";
        "test.MyClass.__init__";
        "test.MyClass.method";
        "test.foo";
        "test.bar";
      ]
    ();
  (* Multiple definitions with the same name *)
  assert_fully_qualified_names
    ~definitions:
      [create_function ~line:1 "foo"; create_function ~line:2 "foo"; create_function ~line:3 "foo"]
    ~expected:["test.$toplevel"; "test.foo"; "test.foo$2"; "test.foo$3"]
    ();
  (* Nested definitions *)
  assert_fully_qualified_names
    ~definitions:
      [
        create_function ~line:1 "decorator";
        create_function ~line:2 ~parent:(function_parent ~line:1) "inner";
        create_function ~line:3 ~parent:(function_parent ~line:1) "wrapper";
      ]
    ~expected:["test.$toplevel"; "test.decorator"; "test.decorator.inner"; "test.decorator.wrapper"]
    ();
  (* Nested definitions with the same name *)
  assert_fully_qualified_names
    ~definitions:
      [
        create_function ~line:1 "decorator";
        create_function ~line:2 ~parent:(function_parent ~line:1) "inner";
        create_function ~line:3 ~parent:(function_parent ~line:1) "inner";
      ]
    ~expected:["test.$toplevel"; "test.decorator"; "test.decorator.inner"; "test.decorator.inner$2"]
    ();
  assert_fully_qualified_names
    ~definitions:
      [
        create_function ~line:1 "decorator";
        create_function ~line:2 ~parent:(function_parent ~line:1) "inner";
        create_function ~line:3 ~parent:(function_parent ~line:2) "inner";
        create_function ~line:4 ~parent:(function_parent ~line:2) "inner";
        create_function ~line:5 ~parent:(function_parent ~line:1) "inner";
        create_function ~line:6 ~parent:(function_parent ~line:5) "inner";
        create_function ~line:7 ~parent:(function_parent ~line:5) "inner";
      ]
    ~expected:
      [
        "test.$toplevel";
        "test.decorator";
        "test.decorator.inner";
        "test.decorator.inner.inner";
        "test.decorator.inner.inner$2";
        "test.decorator.inner$2";
        "test.decorator.inner$2.inner";
        "test.decorator.inner$2.inner$2";
      ]
    ();
  (* Nested classes *)
  assert_fully_qualified_names
    ~definitions:
      [
        create_class ~line:1 "a";
        create_class ~line:2 ~parent:(class_parent ~line:1) "b";
        create_function ~line:3 ~parent:(class_parent ~line:2) "__init__";
        create_function ~line:4 ~parent:(class_parent ~line:1) "__init__";
      ]
    ~expected:
      [
        "test.$toplevel";
        "test.a";
        "test.a.$class_toplevel";
        "test.a.b";
        "test.a.b.$class_toplevel";
        "test.a.b.__init__";
        "test.a.__init__";
      ]
    ();
  (* Nested classes with the same name *)
  assert_fully_qualified_names
    ~definitions:
      [
        create_class ~line:1 "a";
        create_class ~line:2 ~parent:(class_parent ~line:1) "b";
        create_class ~line:3 ~parent:(class_parent ~line:1) "b";
      ]
    ~expected:
      [
        "test.$toplevel";
        "test.a";
        "test.a.$class_toplevel";
        "test.a.b";
        "test.a.b.$class_toplevel";
        "test.a.b$2";
        "test.a.b$2.$class_toplevel";
      ]
    ();
  (* Nested classes that conflicts with another module *)
  assert_fully_qualified_names
    ~definitions:
      [
        create_class ~line:1 "conflict";
        create_function ~line:2 ~parent:(class_parent ~line:1) "foo";
        create_class ~line:3 "no_conflict";
        create_function ~line:4 ~parent:(class_parent ~line:3) "foo";
      ]
    ~modules:["test.conflict"]
    ~expected:
      [
        "test.$toplevel";
        "test#conflict";
        "test#conflict.$class_toplevel";
        "test#conflict.foo";
        "test.no_conflict";
        "test.no_conflict.$class_toplevel";
        "test.no_conflict.foo";
      ]
    ();
  assert_fully_qualified_names
    ~definitions:
      [
        create_class ~line:1 "a";
        create_function ~line:2 ~parent:(class_parent ~line:1) "b";
        create_function ~line:3 ~parent:(function_parent ~line:2) "c";
      ]
    ~modules:["test.a.b"]
    ~expected:["test.$toplevel"; "test.a"; "test.a.$class_toplevel"; "test#a.b"; "test#a.b.c"]
    ();
  (* Conflict between class and function name *)
  assert_fully_qualified_names
    ~definitions:
      [
        create_class ~line:1 "a";
        create_function ~line:2 ~parent:(class_parent ~line:1) "__init__";
        create_function ~line:3 "a";
      ]
    ~expected:["test.$toplevel"; "test.a"; "test.a.$class_toplevel"; "test.a.__init__"; "test.a$2"]
    ();
  (* Type overloads *)
  (* Note: pyrefly only exports one definition per overload set, so this test isn't relevant
     anymore. *)
  assert_fully_qualified_names
    ~definitions:
      [
        create_function ~line:1 "a";
        create_function ~line:2 ~is_overload:true "a";
        create_function ~line:3 ~is_overload:true "a";
      ]
    ~expected:["test.$toplevel"; "test.a"; "test.a$2"; "test.a$3"]
    ();
  (* Property getter and setter *)
  assert_fully_qualified_names
    ~definitions:
      [
        create_class ~line:1 "A";
        create_function ~line:2 ~parent:(class_parent ~line:1) ~is_property_getter:true "x";
        create_function ~line:3 ~parent:(class_parent ~line:1) ~is_property_setter:true "x";
      ]
    ~expected:["test.$toplevel"; "test.A"; "test.A.$class_toplevel"; "test.A.x"; "test.A.x@setter"]
    ();
  ()


let test_strip_path_prefix _ =
  let assert_strip_string ~expected name =
    assert_equal ~cmp:String.equal ~printer:Fn.id expected (PyreflyApi.strip_path_prefix name)
  in
  (* A path prefix is stripped. *)
  assert_strip_string ~expected:"a.b.c.foo" "a/b/c.py:a.b.c.foo";
  (* A name without a path prefix is unchanged. *)
  assert_strip_string ~expected:"a.b.c.foo" "a.b.c.foo";
  (* When the path itself contains a colon, we split on the last colon. *)
  assert_strip_string ~expected:"typing.foo" "typeshed://typing.py:typing.foo";
  let assert_strip_target ~expected ~target =
    assert_equal
      ~cmp:(fun a b -> Target.compare a b = 0)
      ~printer:Target.show_pretty
      expected
      (PyreflyApi.strip_target_path_prefix target)
  in
  (* Function with a path prefix is stripped. *)
  assert_strip_target
    ~expected:
      (Target.Regular.Function { name = "a.b.c.foo"; kind = Target.Normal } |> Target.from_regular)
    ~target:
      (Target.Regular.Function { name = "a/b/c.py:a.b.c.foo"; kind = Target.Normal }
      |> Target.from_regular);
  (* Method has the path prefix in its class_name. *)
  assert_strip_target
    ~expected:
      (Target.Regular.Method { class_name = "a.b.c.C"; method_name = "foo"; kind = Target.Normal }
      |> Target.from_regular)
    ~target:
      (Target.Regular.Method
         { class_name = "a/b/c.py:a.b.c.C"; method_name = "foo"; kind = Target.Normal }
      |> Target.from_regular);
  (* Object with a path prefix is stripped. *)
  assert_strip_target
    ~expected:(Target.Regular.Object "a.b.c.x" |> Target.from_regular)
    ~target:(Target.Regular.Object "a/b/c.py:a.b.c.x" |> Target.from_regular);
  (* A parameterized target has the path prefix stripped from both the regular target and its
     parameter targets. *)
  assert_strip_target
    ~expected:
      (Target.Parameterized
         {
           regular = Target.Regular.Function { name = "a.b.c.foo"; kind = Target.Normal };
           parameters =
             [
               ( AccessPath.Root.Variable "x",
                 Target.Regular.Function { name = "d.e.f.bar"; kind = Target.Normal }
                 |> Target.from_regular
                 |> Target.ParameterValue.create );
             ]
             |> Target.ParameterMap.of_alist_exn;
         })
    ~target:
      (Target.Parameterized
         {
           regular = Target.Regular.Function { name = "a/b/c.py:a.b.c.foo"; kind = Target.Normal };
           parameters =
             [
               ( AccessPath.Root.Variable "x",
                 Target.Regular.Function { name = "d/e/f.py:d.e.f.bar"; kind = Target.Normal }
                 |> Target.from_regular
                 |> Target.ParameterValue.create );
             ]
             |> Target.ParameterMap.of_alist_exn;
         });
  ()


module PyrePysaApi = struct
  open Pyre
  open Analysis
  module PyreflyType = PyreflyApi.PyreflyType
  module ScalarTypeProperties = PyreflyApi.ScalarTypeProperties
  module Function = PyreflyApi.ModelQueries.Function
  module Global = PyreflyApi.ModelQueries.Global

  (* Build the Pyrefly type representation for the small set of types used in this file. *)
  let pyrefly_type = function
    | Type.NoneType ->
        {
          PyreflyType.string = "None";
          scalar_properties = ScalarTypeProperties.none;
          class_names = None;
        }
    | Type.Any ->
        {
          PyreflyType.string = "Any";
          scalar_properties = ScalarTypeProperties.none;
          class_names = None;
        }
    | Type.Primitive "int" ->
        {
          PyreflyType.string = "int";
          scalar_properties = ScalarTypeProperties.integer;
          class_names = Some (PyreflyType.ClassNamesFromType.from_class (116, 5));
        }
    | Type.Primitive "str" ->
        {
          PyreflyType.string = "str";
          scalar_properties = ScalarTypeProperties.none;
          class_names = Some (PyreflyType.ClassNamesFromType.from_class (116, 10));
        }
    | Type.Primitive "test.Foo" ->
        {
          PyreflyType.string = "test.Foo";
          scalar_properties = ScalarTypeProperties.none;
          class_names = Some (PyreflyType.ClassNamesFromType.from_class (1000, 0));
        }
    | Type.Primitive "test.Bar" ->
        {
          PyreflyType.string = "test.Bar";
          scalar_properties = ScalarTypeProperties.none;
          class_names = Some (PyreflyType.ClassNamesFromType.from_class (1000, 1));
        }
    | annotation ->
        (* Fallback used only for expectations that are discarded under Pyrefly (e.g. cases with
           `~pyrefly_expect:None`). *)
        {
          PyreflyType.string = Format.asprintf "%a" Type.pp annotation;
          scalar_properties = ScalarTypeProperties.none;
          class_names = None;
        }


  (* Pyrefly does not have the concept of imported names. *)
  let convert_to_pyrefly_global = function
    | Global.Function function_ -> Global.Function { function_ with Function.imported_name = None }
    | global -> global


  let test_resolve_user_qualified_name context =
    let assert_resolve ~context ?pyrefly_expect sources name ~expect =
      let pyrefly_api =
        InterproceduralTest.ScratchPyrePysaProject.setup
          ~context
          ~requires_type_of_expressions:false
          sources
        |> InterproceduralTest.ScratchPyrePysaProject.read_only_api
      in
      let module ResolutionResult = PyreflyApi.ModelQueries.ResolutionResult in
      let module ModuleResolutionResult = PyreflyApi.ModelQueries.ModuleResolutionResult in
      let actual =
        PyreflyApi.ModelQueries.resolve_user_qualified_name
          ~verify_class_attributes:false
          pyrefly_api
          ~is_property_getter:false
          ~is_property_setter:false
          (Ast.Reference.create name)
        |> (function
             | ResolutionResult.ModuleFound { results; _ } ->
                 List.filter_map results ~f:(function
                     | ModuleResolutionResult.Resolved global -> Some global
                     | ModuleResolutionResult.Unresolved _ -> None)
             | ResolutionResult.BaseModuleNotFound -> [])
        |> List.map ~f:Global.strip_location_and_module
      in
      let expect =
        match pyrefly_expect with
        | Some pyrefly_expect -> pyrefly_expect
        | None -> expect
      in
      let expect = expect >>| convert_to_pyrefly_global in
      let expected_list = Option.to_list expect in
      let printer globals =
        List.map globals ~f:Global.show |> String.concat ~sep:", " |> Format.asprintf "[%s]"
      in
      assert_equal ~printer expected_list actual
    in
    let create_parameter ?(annotation = Type.Any) ?(position = 0) name =
      PyreflyApi.ModelQueries.FunctionParameter.Named
        { name; position; annotation = pyrefly_type annotation; has_default = false }
    in
    let create_signature ?(return_annotation = Type.NoneType) parameters =
      {
        PyreflyApi.ModelQueries.FunctionSignature.parameters =
          PyreflyApi.ModelQueries.FunctionParameters.List parameters;
        return_annotation = pyrefly_type return_annotation;
      }
    in
    let create_callable
        ~define_name
        ?imported_name
        ?(is_method = false)
        ?(signatures = [create_signature []])
        ()
      =
      let define_name = Reference.create define_name in
      {
        Function.define_name;
        imported_name = imported_name >>| Reference.create;
        undecorated_signatures = Some signatures;
        is_property_getter = false;
        is_property_setter = false;
        is_method;
        module_qualifier = None;
        location = None;
      }
    in
    (* Most common cases. *)
    assert_resolve
      ~context
      ["test.py", {|
      def foo() -> None:
        return
    |}]
      "test.foo"
      ~expect:
        (Some
           (Global.Function (create_callable ~define_name:"test.foo" ~imported_name:"test.foo" ())));
    assert_resolve
      ~context
      ["test.py", {|
      class Foo:
        def bar(self) -> None:
          return
    |}]
      "test.Foo.bar"
      ~expect:
        (Some
           (Global.Function
              (create_callable
                 ~define_name:"test.Foo.bar"
                 ~is_method:true
                 ~signatures:
                   [
                     create_signature
                       [create_parameter ~annotation:(Type.Primitive "test.Foo") "self"];
                   ]
                 ())));
    assert_resolve
      ~context
      [
        ( "test.py",
          {|
          from typing import Callable
          foo: Callable[[], None] = lambda: None
        |}
        );
      ]
      "test.foo"
      ~expect:
        (Some
           (Global.Function
              (create_callable
                 ~define_name:"test.foo"
                 ~signatures:[create_signature ~return_annotation:Type.NoneType []]
                 ())))
      ~pyrefly_expect:
        (Some
           (Global.ModuleGlobal
              { name = Reference.create "test.foo"; module_qualifier = None; location = None }));
    assert_resolve
      ~context
      [
        ( "test.py",
          {|
          from typing import Callable
          class Foo:
            bar: Callable[[], None] = lambda: None
        |}
        );
      ]
      "test.Foo.bar"
      ~expect:
        (Some
           (Global.Function
              (create_callable
                 ~define_name:"test.Foo.bar"
                 ~is_method:true
                 ~signatures:[create_signature ~return_annotation:Type.NoneType []]
                 ())));
    assert_resolve
      ~context
      ["test.py", {|
      class Foo:
        pass
    |}]
      "test.Foo"
      ~expect:
        (Some (Global.Class { class_name = "test.Foo"; module_qualifier = None; location = None }));
    assert_resolve
      ~context
      ["test.py", {|
      class Foo:
        class Bar:
          pass
    |}]
      "test.Foo.Bar"
      ~expect:
        (Some
           (Global.Class { class_name = "test.Foo.Bar"; module_qualifier = None; location = None }));
    assert_resolve
      ~context
      ["test.py", {|
      def foo() -> None:
        return None
    |}]
      "test"
      ~expect:(Some (Global.Module { qualifier = Reference.create "test" }));
    assert_resolve
      ~context
      ["test.py", {|
      class Foo:
        x: int = 1
    |}]
      "test.Foo.x"
      ~expect:
        (Some
           (Global.ClassAttribute
              { name = Reference.create "test.Foo.x"; module_qualifier = None; location = None }));
    assert_resolve
      ~context
      ["test.py", {|
      x: int = 1
    |}]
      "test.x"
      ~expect:
        (Some
           (Global.ModuleGlobal
              { name = Reference.create "test.x"; module_qualifier = None; location = None }));
    assert_resolve
      ~context
      ["test.py", {|
      from typing import Any
      x: Any = 1
    |}]
      "test.x"
      ~expect:
        (Some
           (Global.UnknownModuleGlobal
              { name = Reference.create "test.x"; module_qualifier = None; location = None }))
      ~pyrefly_expect:
        (Some
           (Global.ModuleGlobal
              { name = Reference.create "test.x"; module_qualifier = None; location = None }));
    assert_resolve
      ~context
      [
        ( "test.py",
          {|
          from typing import Type
          class Foo:
            pass
          x: Type[Foo] = Foo
        |}
        );
      ]
      "test.x"
      ~expect:
        (Some
           (Global.ModuleGlobal
              { name = Reference.create "test.x"; module_qualifier = None; location = None }));

    (* Symbol is not found. *)
    assert_resolve
      ~context
      ["test.py", {|
      def foo() -> None:
        return
    |}]
      "test.bar"
      ~expect:None;
    assert_resolve
      ~context
      ["test.py", {|
      class Foo:
        def bar() -> None:
          return
    |}]
      "test.Foo.baz"
      ~expect:None;
    assert_resolve
      ~context
      ["test.py", {|
      class Foo:
        pass
    |}]
      "test.Bar"
      ~expect:None;
    assert_resolve ~context ["foo.py", "x: int = 1"] "bar" ~expect:None;

    (* Decorators. *)
    assert_resolve
      ~context
      [
        ( "test.py",
          {|
          class Memoize:
            def __init__(self, f):
              self.f = f
            def __call__(self, *args, **kwargs):
              pass

          def memoize(f) -> Memoize:
            return Memoize(f)

          @memoize
          def foo(x: int) -> int:
            return x
        |}
        );
      ]
      "test.foo"
      ~expect:
        (Some
           (Global.Function
              (create_callable
                 ~define_name:"test.foo"
                 ~imported_name:"test.foo"
                 ~signatures:
                   [
                     create_signature
                       ~return_annotation:Type.integer
                       [create_parameter ~annotation:Type.integer "x"];
                   ]
                 ())));
    assert_resolve
      ~context
      [
        ( "test.py",
          {|
          class Memoize:
            def __init__(self, f):
              self.f = f
            def __call__(self, *args, **kwargs):
              pass

          def memoize(f) -> Memoize:
            return Memoize(f)

          class Bar:
            @memoize
            def baz(self, x: int) -> int:
              return x
        |}
        );
      ]
      "test.Bar.baz"
      ~expect:
        (Some
           (Global.Function
              (create_callable
                 ~define_name:"test.Bar.baz"
                 ~is_method:true
                 ~signatures:
                   [
                     create_signature
                       ~return_annotation:Type.integer
                       [
                         create_parameter ~annotation:(Type.Primitive "test.Bar") "self";
                         create_parameter ~annotation:Type.integer ~position:1 "x";
                       ];
                   ]
                 ())));

    (* Overloads *)
    assert_resolve
      ~context
      [
        ( "test.py",
          {|
          from typing import overload
          class Foo:
            @overload
            def bar(self, x: int) -> str: ...
            @overload
            def bar(self, x: str) -> int: ...
        |}
        );
      ]
      "test.Foo.bar"
      ~expect:
        (Some
           (Global.Function
              (create_callable
                 ~define_name:"test.Foo.bar"
                 ~is_method:true
                 ~signatures:
                   [
                     create_signature
                       ~return_annotation:Type.string
                       [
                         create_parameter ~annotation:(Type.Primitive "test.Foo") "self";
                         create_parameter ~annotation:Type.integer ~position:1 "x";
                       ];
                     create_signature
                       ~return_annotation:Type.string
                       [
                         create_parameter ~annotation:(Type.Primitive "test.Foo") "self";
                         create_parameter ~annotation:Type.integer ~position:1 "x";
                       ];
                     create_signature
                       ~return_annotation:Type.integer
                       [
                         create_parameter ~annotation:(Type.Primitive "test.Foo") "self";
                         create_parameter ~annotation:Type.string ~position:1 "x";
                       ];
                   ]
                 ())))
      ~pyrefly_expect:
        (Some
           (Global.Function
              (create_callable
                 ~define_name:"test.Foo.bar"
                 ~is_method:true
                 ~signatures:
                   [
                     create_signature
                       ~return_annotation:Type.string
                       [
                         create_parameter ~annotation:(Type.Primitive "test.Foo") "self";
                         create_parameter ~annotation:Type.integer ~position:1 "x";
                       ];
                     create_signature
                       ~return_annotation:Type.integer
                       [
                         create_parameter ~annotation:(Type.Primitive "test.Foo") "self";
                         create_parameter ~annotation:Type.string ~position:1 "x";
                       ];
                   ]
                 ())));

    (* Top. *)
    assert_resolve
      ~context
      [
        ( "test.py",
          {|
          class Foo:
            def bar(self):
              pass
            baz = bar
        |}
        );
      ]
      "test.Foo.baz"
      ~expect:
        (Some
           (Global.UnknownClassAttribute
              { name = Reference.create "test.Foo.baz"; module_qualifier = None; location = None }))
      ~pyrefly_expect:
        (Some
           (Global.Function
              (create_callable
                 ~define_name:"test.Foo.baz"
                 ~is_method:true
                 ~signatures:
                   [
                     create_signature
                       ~return_annotation:Type.NoneType
                       [create_parameter ~annotation:(Type.Primitive "test.Foo") "self"];
                   ]
                 ())));

    (* Definition in type stub. *)
    assert_resolve
      ~context
      ["test.pyi", {|
      def foo() -> None: ...
    |}]
      "test.foo"
      ~expect:
        (Some
           (Global.Function
              (create_callable
                 ~define_name:"test.foo"
                 ~imported_name:"test.foo"
                 ~signatures:[create_signature ~return_annotation:Type.NoneType []]
                 ())));
    assert_resolve
      ~context
      ["test.pyi", {|
      class Foo:
        def bar(self) -> None: ...
    |}]
      "test.Foo.bar"
      ~expect:
        (Some
           (Global.Function
              (create_callable
                 ~define_name:"test.Foo.bar"
                 ~is_method:true
                 ~signatures:
                   [
                     create_signature
                       ~return_annotation:Type.NoneType
                       [create_parameter ~annotation:(Type.Primitive "test.Foo") "self"];
                   ]
                 ())));
    assert_resolve
      ~context
      [
        ( "test.pyi",
          {|
          from typing import Callable
          foo: Callable[[], None]
        |} );
      ]
      "test.foo"
      ~expect:
        (Some
           (Global.Function
              (create_callable
                 ~define_name:"test.foo"
                 ~signatures:[create_signature ~return_annotation:Type.NoneType []]
                 ())))
      ~pyrefly_expect:
        (Some
           (Global.ModuleGlobal
              { name = Reference.create "test.foo"; module_qualifier = None; location = None }));
    assert_resolve
      ~context
      ["test.pyi", {|
      x: int = 1
    |}]
      "test.x"
      ~expect:
        (Some
           (Global.ModuleGlobal
              { name = Reference.create "test.x"; module_qualifier = None; location = None }));

    (* Deeply nested code, where outer packages are not importable *)
    assert_resolve
      ~context
      [
        "outer/middle/inner/a.py", {|
      def foo() -> None: ...
    |};
        "outer/middle/inner/b.py", {|
      from .a import foo
    |};
      ]
      "outer.middle.inner.b.foo"
      ~expect:
        (Some
           (Global.Function
              (create_callable
                 ~define_name:"outer.middle.inner.b.foo"
                 ~imported_name:"outer.middle.inner.a.foo"
                 ~signatures:[create_signature ~return_annotation:Type.NoneType []]
                 ())))
      ~pyrefly_expect:None;
    assert_resolve
      ~context
      [
        "outer/middle/inner/a.py", {|
      class Foo:
        def bar(self) -> None: ...
    |};
        "outer/middle/inner/b.py", {|
      from .a import Foo
    |};
      ]
      "outer.middle.inner.b.Foo.bar"
      ~expect:
        (Some
           (Global.Function
              (create_callable
                 ~define_name:"outer.middle.inner.b.Foo.bar"
                 ~imported_name:"outer.middle.inner.a.Foo.bar"
                 ~is_method:true
                 ~signatures:
                   [
                     create_signature
                       [
                         create_parameter
                           ~annotation:(Type.Primitive "outer.middle.inner.a.Foo")
                           "self";
                       ];
                   ]
                 ())))
      ~pyrefly_expect:None;
    ()
end

let () =
  "pyreflyApi"
  >::: [
         "module_qualifiers" >:: test_module_qualifiers;
         "fully_qualified_names" >:: test_fully_qualified_names;
         "strip_path_prefix" >:: test_strip_path_prefix;
         "resolve_user_qualified_name" >:: PyrePysaApi.test_resolve_user_qualified_name;
       ]
  |> Test.run
