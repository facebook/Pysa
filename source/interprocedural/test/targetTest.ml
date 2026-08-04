(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open Pyre
open OUnit2
open Ast
open Test
open Interprocedural

(* Targets store packed ids; mint a distinct synthetic `CallableId` per name (stable across calls,
   so the same name is the same target -- needed by the recursion tests). *)
let synthetic_callables = InterproceduralTest.SyntheticCallables.create ()

let display_api = InterproceduralTest.SyntheticCallables.display_api synthetic_callables

let function_id name = InterproceduralTest.SyntheticCallables.callable_id synthetic_callables name

let function_regular name = Target.Regular.Function (function_id name)

let test_get_module_and_definition context =
  let assert_get_module_and_definition ~source ~target ~expected () =
    let pyrefly_api =
      InterproceduralTest.ScratchPyrePysaProject.setup
        ~context
        ~requires_type_of_expressions:false
        ["test.py", source]
      |> InterproceduralTest.ScratchPyrePysaProject.read_only_api
    in
    let actual =
      target pyrefly_api
      |> CallablesSharedMemory.get_signature_and_definition_for_test ~pyrefly_api
      >>= fun ({ CallablesSharedMemory.CallableSignature.module_id; _ }, define) ->
      PyreflyApi.AstResult.to_option define
      >>| (fun { Node.value = { Statement.Define.body; _ }; _ } -> body)
      >>| fun define -> PyreflyApi.ReadOnly.module_qualifier_of_id pyrefly_api module_id, define
    in
    let equal (first_qualifier, first_body) (second_qualifier, second_body) =
      Reference.equal first_qualifier second_qualifier
      && List.equal
           (fun left right -> Statement.location_insensitive_compare left right = 0)
           first_body
           second_body
    in
    let printer = function
      | None -> "None"
      | Some (qualifier, body) ->
          Format.sprintf "%s: %s" (Reference.show qualifier) (List.to_string body ~f:Statement.show)
    in
    assert_equal ~printer ~cmp:(Option.equal equal) expected actual
  in
  assert_get_module_and_definition
    ~source:
      {|
    class C:
      @property
      def foo(self) -> int:
        return 0
      @foo.setter
      def foo(self, value: int) -> None:
        self._foo = value
  |}
    ~target:(fun pyrefly_api ->
      let class_id =
        PyreflyApi.ReadOnly.class_id_from_name pyrefly_api (Reference.create "test.C")
      in
      PyreflyApi.ReadOnly.Target.resolve_method_target
        pyrefly_api
        ~class_id
        ~method_name:"foo"
        ~is_property_setter:true
      |> Option.value_exn ~message:"expected test.C.foo@setter to resolve")
    ~expected:
      (Some
         ( Reference.create "test",
           [
             +Statement.Statement.Assign
                {
                  Statement.Assign.target = !"self._foo";
                  annotation = None;
                  value = Some !"value";
                  origin = None;
                };
           ] ))
    ()


let test_pretty_print _ =
  let normalize string = String.split_lines string |> List.map ~f:String.lstrip |> String.concat in
  let assert_equal ~expected ~actual =
    assert_equal
      ~cmp:String.equal
      ~printer:Fn.id
      (normalize expected)
      (normalize ((Target.show_pretty ~display_api) actual))
  in
  assert_equal
    ~expected:"Function{foo}[]"
    ~actual:
      (Target.Parameterized
         { regular = function_regular "foo"; parameters = Target.ParameterMap.empty });
  assert_equal
    ~expected:"Function{foo}[local(x)=Function{bar}, local(y)=Function{baz}]"
    ~actual:
      (Target.Parameterized
         {
           regular = function_regular "foo";
           parameters =
             [
               ( AccessPath.Root.Variable "x",
                 function_regular "bar" |> Target.from_regular |> Target.ParameterValue.create );
               ( AccessPath.Root.Variable "y",
                 function_regular "baz" |> Target.from_regular |> Target.ParameterValue.create );
             ]
             |> Target.ParameterMap.of_alist_exn;
         });
  assert_equal
    ~expected:"Function{foo}[local(x)=Function{bar} (bound), local(y)=Function{baz}]"
    ~actual:
      (Target.Parameterized
         {
           regular = function_regular "foo";
           parameters =
             [
               ( AccessPath.Root.Variable "x",
                 function_regular "bar"
                 |> Target.from_regular
                 |> Target.ParameterValue.create ~implicit_receiver:true );
               ( AccessPath.Root.Variable "y",
                 function_regular "baz" |> Target.from_regular |> Target.ParameterValue.create );
             ]
             |> Target.ParameterMap.of_alist_exn;
         });
  ()


let test_contain_recursive_targets _ =
  let assert_contain_recursive_target ~result ~target =
    assert_equal ~printer:Bool.to_string result (Target.contain_recursive_target target)
  in
  assert_contain_recursive_target
    ~result:false
    ~target:(function_regular "foo" |> Target.from_regular);
  assert_contain_recursive_target
    ~result:false
    ~target:
      (Target.Parameterized
         {
           regular = function_regular "foo";
           parameters =
             [
               ( AccessPath.Root.Variable "x",
                 function_regular "bar" |> Target.from_regular |> Target.ParameterValue.create );
             ]
             |> Target.ParameterMap.of_alist_exn;
         });
  assert_contain_recursive_target
    ~result:true
    ~target:
      (Target.Parameterized
         {
           regular = function_regular "foo";
           parameters =
             [
               ( AccessPath.Root.Variable "x",
                 function_regular "foo" |> Target.from_regular |> Target.ParameterValue.create );
             ]
             |> Target.ParameterMap.of_alist_exn;
         });
  assert_contain_recursive_target
    ~result:true
    ~target:
      (Target.Parameterized
         {
           regular = function_regular "foo";
           parameters =
             [
               ( AccessPath.Root.Variable "x",
                 Target.Parameterized
                   {
                     regular = function_regular "bar";
                     parameters =
                       [
                         ( AccessPath.Root.Variable "y",
                           function_regular "foo"
                           |> Target.from_regular
                           |> Target.ParameterValue.create );
                       ]
                       |> Target.ParameterMap.of_alist_exn;
                   }
                 |> Target.ParameterValue.create );
             ]
             |> Target.ParameterMap.of_alist_exn;
         })


let test_target_depth _ =
  let assert_depth ~result ~target =
    assert_equal ~printer:Int.to_string result (Target.depth target)
  in
  assert_depth ~result:1 ~target:(function_regular "foo" |> Target.from_regular);
  assert_depth
    ~result:1
    ~target:
      (Target.Parameterized
         { regular = function_regular "foo"; parameters = Target.ParameterMap.empty });
  assert_depth
    ~result:2
    ~target:
      (Target.Parameterized
         {
           regular = function_regular "foo";
           parameters =
             [
               ( AccessPath.Root.Variable "x",
                 function_regular "bar" |> Target.from_regular |> Target.ParameterValue.create );
             ]
             |> Target.ParameterMap.of_alist_exn;
         });
  assert_depth
    ~result:3
    ~target:
      (Target.Parameterized
         {
           regular = function_regular "foo";
           parameters =
             [
               ( AccessPath.Root.Variable "x",
                 Target.Parameterized
                   {
                     regular = function_regular "bar";
                     parameters =
                       [
                         ( AccessPath.Root.Variable "y",
                           function_regular "baz"
                           |> Target.from_regular
                           |> Target.ParameterValue.create );
                       ]
                       |> Target.ParameterMap.of_alist_exn;
                   }
                 |> Target.ParameterValue.create );
             ]
             |> Target.ParameterMap.of_alist_exn;
         });
  assert_depth
    ~result:3
    ~target:
      (Target.Parameterized
         {
           regular = function_regular "foo";
           parameters =
             [
               ( AccessPath.Root.Variable "x",
                 Target.Parameterized
                   {
                     regular = function_regular "bar";
                     parameters =
                       [
                         ( AccessPath.Root.Variable "y",
                           function_regular "baz"
                           |> Target.from_regular
                           |> Target.ParameterValue.create );
                       ]
                       |> Target.ParameterMap.of_alist_exn;
                   }
                 |> Target.ParameterValue.create );
               ( AccessPath.Root.Variable "y",
                 function_regular "test" |> Target.from_regular |> Target.ParameterValue.create );
             ]
             |> Target.ParameterMap.of_alist_exn;
         })


let () =
  "callable"
  >::: [
         "get_module_and_definition" >:: test_get_module_and_definition;
         "pretty_print" >:: test_pretty_print;
         "contain_recursive_targets" >:: test_contain_recursive_targets;
         "target_depth" >:: test_target_depth;
       ]
  |> Test.run
