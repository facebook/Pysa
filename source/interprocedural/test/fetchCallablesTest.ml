(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2
open Core
open Interprocedural

let define_target pyrefly_api name =
  PyreflyApi.ReadOnly.Target.target_from_define_name
    pyrefly_api
    ~override:false
    (Ast.Reference.create name)


let test_callables context =
  let assert_callables ?(additional_sources = []) ?(source_filename = "test.py") source ~expected =
    let pyrefly_api =
      let project =
        InterproceduralTest.ScratchPyrePysaProject.setup
          ~context
          ~requires_type_of_expressions:false
          ((source_filename, source) :: additional_sources)
      in
      InterproceduralTest.ScratchPyrePysaProject.read_only_api project
    in
    FetchCallables.from_qualifier ~pyrefly_api ~qualifier:(Ast.Reference.create "test")
    |> FetchCallables.get ~definitions:true ~stubs:true
    |> List.sort ~compare:Target.compare
    |> assert_equal
         ~printer:(List.to_string ~f:Target.show_internal)
         ~cmp:(List.equal Target.equal)
         (expected pyrefly_api |> List.sort ~compare:Target.compare)
  in
  assert_callables
    {|
    class C:
      def foo() -> int:
        ...
    |}
    ~expected:(fun pyrefly_api ->
      [
        define_target pyrefly_api "test.$toplevel";
        define_target pyrefly_api "test.C.$class_toplevel";
        define_target pyrefly_api "test.C.foo";
      ]);
  assert_callables
    {|
      import unittest
      class C(unittest.case.TestCase):
        def foo() -> int:
          ...
    |}
    ~expected:(fun _ -> []);
  assert_callables
    {|
      import pytest
      class C:
        def foo() -> int:
          ...
    |}
    ~expected:(fun pyrefly_api ->
      [
        define_target pyrefly_api "test.$toplevel";
        define_target pyrefly_api "test.C.$class_toplevel";
        define_target pyrefly_api "test.C.foo";
      ]);

  assert_callables
    {|
      import pytest
      def test_int() -> int:
        return 0
      class C:
        def foo() -> int:
          ...
    |}
    ~expected:(fun _ -> []);
  assert_callables
    {|
      from pytest import raises
      def test_int() -> int:
        return 0
      class C:
        def foo() -> int:
          ...
    |}
    ~expected:(fun _ -> []);
  assert_callables
    {|
      import pytest.raises as throws
      def test_int() -> int:
        return 0
      class C:
        def foo() -> int:
          ...
    |}
    ~expected:(fun _ -> []);
  assert_callables
    {|
      import pytest
      class C:
        def foo() -> int:
          ...
        def test_int() -> int:
          return 0
    |}
    ~expected:(fun _ -> []);
  assert_callables
    {|
      import foo.pytest
      def test_int() -> int:
        return 0
      class C:
        def foo() -> int:
          ...
    |}
    ~expected:(fun pyrefly_api ->
      [
        define_target pyrefly_api "test.$toplevel";
        define_target pyrefly_api "test.C.$class_toplevel";
        define_target pyrefly_api "test.test_int";
        define_target pyrefly_api "test.C.foo";
      ]);

  assert_callables
    "pass"
    ~additional_sources:
      [
        ( "stub.pyi",
          {|
            class Toplevel:
              some_field: int
              def foo() -> int:
                ...
          |}
        );
      ]
    ~expected:(fun pyrefly_api -> [define_target pyrefly_api "test.$toplevel"]);

  assert_callables
    ~source_filename:"test.pyi"
    {|
      class Toplevel:
        some_field: int
        def foo() -> int:
          ...
    |}
    ~expected:(fun pyrefly_api -> [define_target pyrefly_api "test.Toplevel.foo"])


let () = "staticAnalysis" >::: ["callables" >:: test_callables] |> Test.run
