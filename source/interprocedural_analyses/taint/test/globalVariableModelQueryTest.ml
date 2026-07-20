(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2
open Core
open Ast
open Test
open Taint

let test_find_globals =
  let assert_found_globals ~source ~expected context =
    let project =
      InterproceduralTest.ScratchPyrePysaProject.setup
        ~context
        ~requires_type_of_expressions:true
        ["test.py", source]
    in
    let pyrefly_api = InterproceduralTest.ScratchPyrePysaProject.read_only_api project in
    (* Pyrefly pulls in the full typeshed, so restrict the discovered globals to the `test` module.
       We only verify the discovered set of globals: unlike the Pyre1 backend, the Pyrefly backend's
       `Modelable.create_global` returns `ExplicitAnnotation.Unsupported`, so the
       explicit-annotation values are not asserted here. *)
    let is_test_global name = Reference.is_prefix ~prefix:!&"test" name in
    let actual =
      ModelQueryExecution.GlobalVariableQueryExecutor.get_globals
        ~scheduler:(Test.mock_scheduler ())
        ~pyrefly_api
      |> List.map ~f:Target.object_name
      |> List.filter ~f:is_test_global
      |> List.sort ~compare:Reference.compare
    in
    let expected = List.sort ~compare:Reference.compare expected in
    assert_equal
      ~cmp:(List.equal Reference.equal)
      ~printer:[%show: Reference.t list]
      expected
      actual
  in

  test_list
    [
      labeled_test_case __FILE__ __LINE__
      @@ assert_found_globals
           ~source:{|
      foo = []
      bar: typing.List[typing.Any] = []
    |}
           ~expected:[!&"test.bar"; !&"test.foo"];
      (* Note that functions are not selected *)
      labeled_test_case __FILE__ __LINE__
      @@ assert_found_globals ~source:{|
      def foo():
        pass
    |} ~expected:[];
      labeled_test_case __FILE__ __LINE__
      @@ assert_found_globals
           ~source:
             {|
      foo = []
      bar = {}

      baz: typing.List[typing.Any] = []
      abc: typing.Dict[typing.Any, typing.Any] = {}
    |}
           ~expected:[!&"test.abc"; !&"test.bar"; !&"test.baz"; !&"test.foo"];
      (* TODO(T132423781): Classes are not recognized as globals *)
      labeled_test_case __FILE__ __LINE__
      @@ assert_found_globals
           ~source:{|
      class C:
        def f():
          pass
      C.bar = 1
    |}
           ~expected:[];
      labeled_test_case __FILE__ __LINE__
      @@ assert_found_globals
           ~source:
             {|
      class C:
        def f():
          pass
      c = C()
      annotated_c: C = C()
    |}
           ~expected:[!&"test.annotated_c"; !&"test.c"];
      labeled_test_case __FILE__ __LINE__
      @@ assert_found_globals
           ~source:
             {|
      x, y = [], {}

      annotated_x: typing.List[typing.Any]
      annotated_y: typing.Dict[typing.Any, typing.Any]
      annotated_x, annotated_y = [], {}
    |}
           ~expected:[!&"test.annotated_x"; !&"test.annotated_y"; !&"test.x"; !&"test.y"];
      labeled_test_case __FILE__ __LINE__
      @@ assert_found_globals
           ~source:
             {|
      def setup() -> int:
        return 5

      global_1: typing.Dict[str, int] = setup()
      global_2 = setup()
    |}
           ~expected:[!&"test.global_1"; !&"test.global_2"];
      labeled_test_case __FILE__ __LINE__
      @@ assert_found_globals
           ~source:
             {|
    from typing import List, Callable

    x: int
    y: List[bool]
    z: Callable[[], str]
    |}
           ~expected:[!&"test.x"; !&"test.y"; !&"test.z"];
      labeled_test_case __FILE__ __LINE__
      @@ assert_found_globals
           ~source:
             {|
      x = lambda x, y: x + int(y)

      def fun(x: int, y: str) -> int:
        return x + int(y)

      y = fun

      a = fun(1, "2")
      b: int = fun(1, "2")
      |}
           ~expected:[!&"test.a"; !&"test.b"; !&"test.x"; !&"test.y"];
      labeled_test_case __FILE__ __LINE__
      @@ assert_found_globals
           ~source:
             {|
    x = 1
    x = "abc"

    y: int = 1
    y = "abc"
    y: str = "abc"
    |}
           ~expected:[!&"test.x"; !&"test.y"];
    ]


let () = "globalVariableQuery" >::: [test_find_globals] |> Test.run
