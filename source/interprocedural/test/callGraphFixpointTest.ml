(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open OUnit2
open Test
open Interprocedural
open CallGraph
open CallGraphBuilder
open CallGraphTestHelper

let class_id_exn pyrefly_api class_name =
  PyreflyApi.ReadOnly.class_id_from_name_opt pyrefly_api (Ast.Reference.create class_name)
  |> Option.value_exn ~message:(Format.sprintf "unknown class `%s`" class_name)


let function_regular pyrefly_api name =
  InterproceduralTest.resolve_function_regular_exn ~pyrefly_api (Ast.Reference.create name)


let function_regular_decorated pyrefly_api name =
  InterproceduralTest.resolve_function_regular_decorated_exn
    ~pyrefly_api
    (Ast.Reference.create name)


let method_regular pyrefly_api ~class_name ~method_name =
  InterproceduralTest.resolve_method_regular_exn
    ~pyrefly_api
    ~class_name:(Ast.Reference.create class_name)
    ~method_name
    ()


let method_regular_decorated pyrefly_api ~class_name ~method_name =
  InterproceduralTest.resolve_method_regular_decorated_exn
    ~pyrefly_api
    ~class_name:(Ast.Reference.create class_name)
    ~method_name


let override_regular pyrefly_api ~class_name ~method_name =
  method_regular pyrefly_api ~class_name ~method_name
  |> Target.Regular.get_corresponding_override_exn


module Expected = struct
  type t = {
    callable: Target.t;
    returned_callables: CallTarget.t list;
    call_graph: (string * ExpressionCallees.t) list;
  }
end

let assert_higher_order_call_graph_fixpoint
    ?(max_iterations = 10)
    ?(skip_analysis_targets = fun _ -> Target.HashSet.create ())
    ?(called_when_parameter = fun _ -> Target.HashSet.create ())
    ?(maximum_target_depth = Configuration.StaticAnalysis.default_maximum_target_depth)
    ~source
    ~expected
    ()
    context
  =
  let _, pyrefly_api, configuration =
    TestHelper.setup_single_py_file
      ~requires_type_of_expressions:true
      ~file_name:"test.py"
      ~context
      ~source
      ()
  in
  let skip_analysis_targets = skip_analysis_targets pyrefly_api in
  let called_when_parameter = called_when_parameter pyrefly_api in
  let static_analysis_configuration =
    Configuration.StaticAnalysis.create
      ~maximum_target_depth
      ~higher_order_call_graph_max_iterations:max_iterations
      ~pyrefly_results:(PyrePath.create_absolute "/pyrefly_results")
      configuration
      ()
  in
  let qualifier = Ast.Reference.create "test" in
  let override_graph_heap =
    OverrideGraph.Heap.from_qualifier
      ~pyrefly_api
      ~skip_overrides_targets:Ast.Reference.SerializableSet.empty
      qualifier
  in
  let override_graph_shared_memory = OverrideGraph.SharedMemory.from_heap override_graph_heap in
  let initial_callables = FetchCallables.from_qualifier ~pyrefly_api ~qualifier in
  let definitions = FetchCallables.get_definitions initial_callables in
  let scheduler = Test.mock_scheduler () in
  let scheduler_policy = Scheduler.Policy.legacy_fixed_chunk_count () in
  let callables_to_definitions_map =
    CallablesSharedMemory.ReadWrite.from_pyrefly_api ~pyrefly_api
  in
  let callables_to_decorators_map =
    CallableToDecoratorsMap.SharedMemory.create
      ~scheduler
      ~scheduler_policy
      ~pyrefly_api
      ~callables_to_definitions_map:
        (CallablesSharedMemory.ReadOnly.read_only callables_to_definitions_map)
      ~skip_analysis_targets
      definitions
  in
  let ({ SharedMemory.whole_program_call_graph; define_call_graphs } as call_graph) =
    CallGraphBuilder.build_whole_program_call_graph
      ~scheduler
      ~static_analysis_configuration
      ~pyrefly_api
      ~resolve_module_path:None
      ~override_graph:(Some (OverrideGraph.SharedMemory.read_only override_graph_shared_memory))
      ~store_shared_memory:true
      ~attribute_targets:Target.Set.empty
      ~skip_analysis_targets
      ~skip_call_higher_order_functions:(Target.HashSet.create ())
      ~definitions
      ~callables_to_definitions_map:
        (CallablesSharedMemory.ReadOnly.read_only callables_to_definitions_map)
      ~callables_to_decorators_map:
        (CallableToDecoratorsMap.SharedMemory.read_only callables_to_decorators_map)
      ~global_constants:
        (GlobalConstants.SharedMemory.create () |> GlobalConstants.SharedMemory.read_only)
      ~create_dependency_for:Interprocedural.CallGraph.AllTargetsUseCase.CallGraphDependency
  in
  let dependency_graph =
    DependencyGraph.build_whole_program_dependency_graph
      ~display_api:(Interprocedural.PyreflyApi.ReadOnly.display_api pyrefly_api)
      ~static_analysis_configuration
      ~prune:DependencyGraph.PruneMethod.None
      ~initial_callables
      ~call_graph:whole_program_call_graph
      ~overrides:override_graph_heap
      ~ignore_decorated_targets:false
  in
  let fixpoint_state =
    CallGraphFixpoint.compute
      ~scheduler
      ~scheduler_policy
      ~static_analysis_configuration
      ~resolve_module_path:None
      ~pyrefly_api
      ~call_graph
      ~dependency_graph
      ~override_graph_shared_memory
      ~skip_analysis_targets
      ~called_when_parameter
      ~skip_inlining_higher_order_functions:(Target.HashSet.create ())
      ~callables_to_definitions_map
      ~callables_to_decorators_map
  in
  List.iter (expected pyrefly_api) ~f:(fun { Expected.callable; call_graph; returned_callables } ->
      let actual_call_graph =
        callable
        |> CallGraphFixpoint.get_model
             ~drop_decorated_targets:true
             ~apply_decorator_fallback:false
             fixpoint_state
        |> Option.value ~default:HigherOrderCallGraph.empty
        |> HigherOrderCallGraphForTest.from_actual
      in
      let expected_call_graph =
        HigherOrderCallGraphForTest.from_expected
          { HigherOrderCallGraphForTest.Expected.call_graph; returned_callables }
      in
      assert_equal
        ~cmp:HigherOrderCallGraphForTest.equal
        ~printer:(fun call_graph ->
          Format.asprintf
            "For callable %a: %a"
            Target.pp
            callable
            HigherOrderCallGraphForTest.pp
            call_graph)
        ~pp_diff:(Test.diff ~print:HigherOrderCallGraphForTest.pp)
        expected_call_graph
        actual_call_graph);
  OverrideGraph.SharedMemory.cleanup override_graph_shared_memory;
  SharedMemory.cleanup define_call_graphs;
  CallGraphFixpoint.cleanup ~keep_models:false fixpoint_state.CallGraphFixpoint.fixpoint;
  CallablesSharedMemory.ReadWrite.cleanup callables_to_definitions_map;
  ()


let test_higher_order_call_graph_fixpoint =
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_higher_order_call_graph_fixpoint
           ~source:
             {|
     def foo():
       return 0
     def bar():
       return foo
     def baz():
       return bar()
  |}
           ~expected:(fun pyrefly_api ->
             [
               {
                 Expected.callable = function_regular pyrefly_api "test.bar" |> Target.from_regular;
                 call_graph =
                   [
                     ( "5:9-5:12|identifier|foo",
                       ExpressionCallees.from_identifier
                         (IdentifierCallees.create
                            ~if_called:
                              (CallCallees.create
                                 ~call_targets:
                                   [
                                     CallTarget.create_regular
                                       ~return_type:(Some ReturnType.integer)
                                       (function_regular pyrefly_api "test.foo");
                                   ]
                                 ())
                            ()) );
                   ];
                 returned_callables =
                   [
                     CallTarget.create_regular
                       ~return_type:(Some ReturnType.integer)
                       (function_regular pyrefly_api "test.foo");
                   ];
               };
               {
                 Expected.callable = function_regular pyrefly_api "test.baz" |> Target.from_regular;
                 call_graph =
                   [
                     ( "7:9-7:14",
                       ExpressionCallees.from_call
                         (CallCallees.create
                            ~call_targets:
                              [CallTarget.create_regular (function_regular pyrefly_api "test.bar")]
                            ()) );
                   ];
                 returned_callables =
                   [
                     CallTarget.create_regular
                       ~return_type:(Some ReturnType.integer)
                       (function_regular pyrefly_api "test.foo");
                   ];
               };
             ])
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_higher_order_call_graph_fixpoint
           ~source:
             {|
     def foo():
       return 0
     def bar(arg):
       return foo
     def baz():
       return bar(foo)
  |}
           ~max_iterations:1
           ~expected:(fun pyrefly_api ->
             [
               {
                 Expected.callable = function_regular pyrefly_api "test.baz" |> Target.from_regular;
                 call_graph =
                   [
                     ( "7:9-7:17",
                       ExpressionCallees.from_call
                         (CallCallees.create
                            ~call_targets:
                              [
                                CallTarget.create
                                  (create_parameterized_target
                                     ~regular:(function_regular pyrefly_api "test.bar")
                                     ~parameters:
                                       [
                                         ( create_positional_parameter 0 "arg",
                                           function_regular pyrefly_api "test.foo"
                                           |> Target.from_regular );
                                       ]);
                              ]
                            ()) );
                     ( "7:13-7:16|identifier|foo",
                       ExpressionCallees.from_identifier
                         (IdentifierCallees.create
                            ~if_called:
                              (CallCallees.create
                                 ~call_targets:
                                   [
                                     CallTarget.create_regular
                                       ~return_type:(Some ReturnType.integer)
                                       (function_regular pyrefly_api "test.foo");
                                   ]
                                 ())
                            ()) );
                   ];
                 returned_callables = [];
               };
             ])
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_higher_order_call_graph_fixpoint
           ~source:{|
     def foo():
       return foo
  |}
           ~expected:(fun pyrefly_api ->
             [
               {
                 Expected.callable = function_regular pyrefly_api "test.foo" |> Target.from_regular;
                 call_graph =
                   [
                     ( "3:9-3:12|identifier|foo",
                       ExpressionCallees.from_identifier
                         (IdentifierCallees.create
                            ~if_called:
                              (CallCallees.create
                                 ~call_targets:
                                   [
                                     CallTarget.create_regular
                                       (function_regular pyrefly_api "test.foo");
                                   ]
                                 ())
                            ()) );
                   ];
                 returned_callables =
                   [CallTarget.create_regular (function_regular pyrefly_api "test.foo")];
               };
             ])
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_higher_order_call_graph_fixpoint
           ~source:{|
     def foo():
       return foo()
  |}
           ~expected:(fun pyrefly_api ->
             [
               {
                 Expected.callable = function_regular pyrefly_api "test.foo" |> Target.from_regular;
                 call_graph =
                   [
                     ( "3:9-3:14",
                       ExpressionCallees.from_call
                         (CallCallees.create
                            ~call_targets:
                              [CallTarget.create_regular (function_regular pyrefly_api "test.foo")]
                            ()) );
                   ];
                 returned_callables = [];
               };
             ])
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_higher_order_call_graph_fixpoint
           ~source:
             {|
     def bar():
       return 0
     def foo(b):
       if b:
         return bar
       else:
         return foo()
  |}
           ~expected:(fun pyrefly_api ->
             [
               {
                 Expected.callable = function_regular pyrefly_api "test.foo" |> Target.from_regular;
                 call_graph =
                   [
                     ( "6:11-6:14|identifier|bar",
                       ExpressionCallees.from_identifier
                         (IdentifierCallees.create
                            ~if_called:
                              (CallCallees.create
                                 ~call_targets:
                                   [
                                     CallTarget.create_regular
                                       ~return_type:(Some ReturnType.integer)
                                       (function_regular pyrefly_api "test.bar");
                                   ]
                                 ())
                            ()) );
                     ( "8:11-8:16",
                       ExpressionCallees.from_call
                         (CallCallees.create
                            ~call_targets:
                              [
                                CallTarget.create_regular
                                  ~return_type:(Some ReturnType.none)
                                  (function_regular pyrefly_api "test.foo");
                              ]
                            ()) );
                   ];
                 returned_callables =
                   [
                     CallTarget.create_regular
                       ~return_type:(Some ReturnType.integer)
                       (function_regular pyrefly_api "test.bar");
                   ];
               };
             ])
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_higher_order_call_graph_fixpoint
           ~source:
             {|
     def propagate(x):
       return x
     def bar():
       return 0
     def foo():
       return propagate(bar)
  |}
           ~expected:(fun pyrefly_api ->
             [
               {
                 Expected.callable = function_regular pyrefly_api "test.foo" |> Target.from_regular;
                 call_graph =
                   [
                     ( "7:9-7:23",
                       ExpressionCallees.from_call
                         (CallCallees.create
                            ~call_targets:
                              [
                                CallTarget.create
                                  (create_parameterized_target
                                     ~regular:(function_regular pyrefly_api "test.propagate")
                                     ~parameters:
                                       [
                                         ( create_positional_parameter 0 "x",
                                           function_regular pyrefly_api "test.bar"
                                           |> Target.from_regular );
                                       ]);
                              ]
                            ()) );
                     ( "7:19-7:22|identifier|bar",
                       ExpressionCallees.from_identifier
                         (IdentifierCallees.create
                            ~if_called:
                              (CallCallees.create
                                 ~call_targets:
                                   [
                                     CallTarget.create_regular
                                       ~return_type:(Some ReturnType.integer)
                                       (function_regular pyrefly_api "test.bar");
                                   ]
                                 ())
                            ()) );
                   ];
                 returned_callables =
                   [CallTarget.create_regular (function_regular pyrefly_api "test.bar")];
               };
               {
                 Expected.callable =
                   create_parameterized_target
                     ~regular:(function_regular pyrefly_api "test.propagate")
                     ~parameters:
                       [
                         ( create_positional_parameter 0 "x",
                           function_regular pyrefly_api "test.bar" |> Target.from_regular );
                       ];
                 call_graph = [];
                 returned_callables =
                   [CallTarget.create_regular (function_regular pyrefly_api "test.bar")];
               };
             ])
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_higher_order_call_graph_fixpoint
           ~source:
             {|
     def propagate(x):
       return x
     def wrap_propagate(x):
       return propagate(x)
     def bar():
       return 0
     def foo():
       return wrap_propagate(bar)
  |}
           ~expected:(fun pyrefly_api ->
             [
               {
                 Expected.callable = function_regular pyrefly_api "test.foo" |> Target.from_regular;
                 call_graph =
                   [
                     ( "9:9-9:28",
                       ExpressionCallees.from_call
                         (CallCallees.create
                            ~call_targets:
                              [
                                CallTarget.create
                                  (create_parameterized_target
                                     ~regular:(function_regular pyrefly_api "test.wrap_propagate")
                                     ~parameters:
                                       [
                                         ( create_positional_parameter 0 "x",
                                           function_regular pyrefly_api "test.bar"
                                           |> Target.from_regular );
                                       ]);
                              ]
                            ()) );
                     ( "9:24-9:27|identifier|bar",
                       ExpressionCallees.from_identifier
                         (IdentifierCallees.create
                            ~if_called:
                              (CallCallees.create
                                 ~call_targets:
                                   [
                                     CallTarget.create_regular
                                       ~return_type:(Some ReturnType.integer)
                                       (function_regular pyrefly_api "test.bar");
                                   ]
                                 ())
                            ()) );
                   ];
                 returned_callables =
                   [CallTarget.create_regular (function_regular pyrefly_api "test.bar")];
               };
             ])
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_higher_order_call_graph_fixpoint
           ~source:
             {|
     def decorator(f):
       g = f
       def inner():
         f()
       return inner
     def bar():
       return 0
     def foo():
       return decorator(bar)
  |}
           ~expected:(fun pyrefly_api ->
             [
               {
                 Expected.callable = function_regular pyrefly_api "test.foo" |> Target.from_regular;
                 call_graph =
                   [
                     ( "10:9-10:23",
                       ExpressionCallees.from_call
                         (CallCallees.create
                            ~call_targets:
                              [
                                CallTarget.create
                                  (create_parameterized_target
                                     ~regular:(function_regular pyrefly_api "test.decorator")
                                     ~parameters:
                                       [
                                         ( create_positional_parameter 0 "f",
                                           function_regular pyrefly_api "test.bar"
                                           |> Target.from_regular );
                                       ]);
                              ]
                            ()) );
                     ( "10:19-10:22|identifier|bar",
                       ExpressionCallees.from_identifier
                         (IdentifierCallees.create
                            ~if_called:
                              (CallCallees.create
                                 ~call_targets:
                                   [
                                     CallTarget.create_regular
                                       ~return_type:(Some ReturnType.integer)
                                       (function_regular pyrefly_api "test.bar");
                                   ]
                                 ())
                            ()) );
                   ];
                 returned_callables =
                   [
                     CallTarget.create
                       (create_parameterized_target
                          ~regular:(function_regular pyrefly_api "test.decorator.inner")
                          ~parameters:
                            [
                              ( AccessPath.Root.CapturedVariable
                                  (AccessPath.CapturedVariable.FromFunction
                                     {
                                       name = "f";
                                       defining_function = Ast.Reference.create "test.decorator";
                                     }),
                                function_regular pyrefly_api "test.bar" |> Target.from_regular );
                            ]);
                   ];
               };
             ])
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_higher_order_call_graph_fixpoint
           ~source:
             {|
     def decorator(f):
       g = f
       def inner(x, y):
         return f(y, x)
       return inner
     @decorator
     def foo(x, y):
       return y
     def baz():
       pass
     def bar():
        return foo(baz, 0)
  |}
           ~expected:(fun pyrefly_api ->
             [
               {
                 Expected.callable = function_regular pyrefly_api "test.bar" |> Target.from_regular;
                 call_graph =
                   [
                     ( "13:10-13:21",
                       ExpressionCallees.from_call
                         (CallCallees.create
                            ~call_targets:
                              [
                                CallTarget.create
                                  (create_parameterized_target
                                     ~regular:(function_regular pyrefly_api "test.decorator.inner")
                                     ~parameters:
                                       [
                                         ( create_positional_parameter 0 "x",
                                           function_regular pyrefly_api "test.baz"
                                           |> Target.from_regular );
                                         ( AccessPath.Root.CapturedVariable
                                             (AccessPath.CapturedVariable.FromFunction
                                                {
                                                  name = "f";
                                                  defining_function =
                                                    Ast.Reference.create "test.decorator";
                                                }),
                                           function_regular pyrefly_api "test.foo"
                                           |> Target.from_regular );
                                       ]);
                              ]
                            ()) );
                     ( "13:14-13:17|identifier|baz",
                       ExpressionCallees.from_identifier
                         (IdentifierCallees.create
                            ~if_called:
                              (CallCallees.create
                                 ~call_targets:
                                   [
                                     CallTarget.create_regular
                                       (function_regular pyrefly_api "test.baz");
                                   ]
                                 ())
                            ()) );
                   ];
                 returned_callables =
                   [CallTarget.create_regular (function_regular pyrefly_api "test.baz")];
               };
             ])
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_higher_order_call_graph_fixpoint
           ~source:
             {|
     from typing import Callable
     def outer(f: Callable):
       def inner():
         g = f
         def inner_most():
           return 0
         return inner_most
       return inner()
  |}
           ~expected:(fun pyrefly_api ->
             [
               {
                 Expected.callable = function_regular pyrefly_api "test.outer" |> Target.from_regular;
                 call_graph =
                   [
                     ( "4:2-8:21",
                       ExpressionCallees.from_define
                         (DefineCallees.create
                            ~define_targets:
                              [
                                CallTarget.create_regular
                                  (function_regular pyrefly_api "test.outer.inner");
                              ]
                            ()) );
                     ( "9:9-9:16",
                       ExpressionCallees.from_call
                         (CallCallees.create
                            ~call_targets:
                              [
                                CallTarget.create_regular
                                  (function_regular pyrefly_api "test.outer.inner");
                              ]
                            ()) );
                   ];
                 returned_callables =
                   [
                     CallTarget.create_regular
                       ~return_type:(Some ReturnType.integer)
                       (function_regular pyrefly_api "test.outer.inner.inner_most");
                   ];
               };
             ])
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_higher_order_call_graph_fixpoint
           ~source:
             {|
     def foo(x: int):
       pass
     def bar(x: int):
       pass
     def decorator(f):
       def wrapper():
         return foo
       return wrapper
     @decorator
     def decorated(x: int):
       return bar
     def baz():
       return decorated()(1)
  |}
           ~expected:(fun pyrefly_api ->
             [
               {
                 Expected.callable = function_regular pyrefly_api "test.baz" |> Target.from_regular;
                 call_graph =
                   [
                     ( "14:9-14:20",
                       ExpressionCallees.from_call
                         (CallCallees.create
                            ~call_targets:
                              [
                                CallTarget.create_regular
                                  (function_regular pyrefly_api "test.decorator.wrapper");
                              ]
                            ()) );
                     ( "14:9-14:23",
                       ExpressionCallees.from_call
                         (CallCallees.create
                            ~call_targets:
                              [CallTarget.create_regular (function_regular pyrefly_api "test.foo")]
                            ~unresolved:
                              (CallGraph.Unresolved.True
                                 CallGraph.Unresolved.UnexpectedCalleeExpression)
                            ()) );
                   ];
                 returned_callables = [];
               };
             ])
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_higher_order_call_graph_fixpoint
           ~source:
             {|
     def decorator(f):
       def inner():
         return f
       return inner
     @decorator
     def foo1():
       return 0
     class C:
       @decorator
       @classmethod
       def foo2(cls):
         return 0
     @decorator
     def foo3():
       return 0
     def bar(x: bool, y: bool):
       if x:
         return foo1  # Redirect `Decorated` target from attribute access
       elif y:
         return C.foo2  # Redirect `Decorated` target from attribute access
       else:
         f = foo3
         return f  # Return `Decorated` target from identifiers
  |}
           ~expected:(fun pyrefly_api ->
             [
               {
                 Expected.callable = function_regular pyrefly_api "test.bar" |> Target.from_regular;
                 call_graph =
                   [
                     ( "19:11-19:15|identifier|foo1",
                       ExpressionCallees.from_identifier (IdentifierCallees.create ()) );
                     ( "21:11-21:12|identifier|C",
                       ExpressionCallees.from_identifier
                         (IdentifierCallees.create
                            ~if_called:
                              (CallCallees.create
                                 ~new_targets:
                                   [
                                     CallTarget.create_regular
                                       ~is_static_method:true
                                       (method_regular
                                          pyrefly_api
                                          ~class_name:"builtins.object"
                                          ~method_name:"__new__");
                                   ]
                                 ~init_targets:
                                   [
                                     CallTarget.create_regular
                                       ~implicit_receiver:true
                                       ~receiver_class:(class_id_exn pyrefly_api "test.C")
                                       (method_regular
                                          pyrefly_api
                                          ~class_name:"builtins.object"
                                          ~method_name:"__init__");
                                   ]
                                 ())
                            ()) );
                     ( "21:11-21:17",
                       ExpressionCallees.from_attribute_access
                         (AttributeAccessCallees.create ~is_attribute:false ()) );
                     ( "23:8-23:12|identifier|foo3",
                       ExpressionCallees.from_identifier (IdentifierCallees.create ()) );
                     ( "24:11-24:12|identifier|f",
                       ExpressionCallees.from_identifier
                         (IdentifierCallees.create
                            ~if_called:
                              (CallCallees.create
                                 ~call_targets:
                                   [
                                     CallTarget.create_regular
                                       (function_regular pyrefly_api "test.decorator.inner");
                                   ]
                                 ())
                            ()) );
                   ];
                 returned_callables =
                   [
                     CallTarget.create
                       (create_parameterized_target
                          ~regular:(function_regular pyrefly_api "test.decorator.inner")
                          ~parameters:
                            [
                              ( AccessPath.Root.CapturedVariable
                                  (AccessPath.CapturedVariable.FromFunction
                                     {
                                       name = "f";
                                       defining_function = Ast.Reference.create "test.decorator";
                                     }),
                                function_regular pyrefly_api "test.foo1" |> Target.from_regular );
                            ]);
                     CallTarget.create
                       (create_parameterized_target
                          ~regular:(function_regular pyrefly_api "test.decorator.inner")
                          ~parameters:
                            [
                              ( AccessPath.Root.CapturedVariable
                                  (AccessPath.CapturedVariable.FromFunction
                                     {
                                       name = "f";
                                       defining_function = Ast.Reference.create "test.decorator";
                                     }),
                                function_regular pyrefly_api "test.foo3" |> Target.from_regular );
                            ]);
                     CallTarget.create
                       ~implicit_receiver:true
                       (create_parameterized_target
                          ~regular:(function_regular pyrefly_api "test.decorator.inner")
                          ~parameters:
                            [
                              ( AccessPath.Root.CapturedVariable
                                  (AccessPath.CapturedVariable.FromFunction
                                     {
                                       name = "f";
                                       defining_function = Ast.Reference.create "test.decorator";
                                     }),
                                method_regular pyrefly_api ~class_name:"test.C" ~method_name:"foo2"
                                |> Target.from_regular );
                            ]);
                   ];
               };
             ])
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_higher_order_call_graph_fixpoint
           ~source:
             {|
     def foo():
       return
     def bar():
       return
     class A:
       def m():
         return foo
     class B(A):
       def m():
         return bar
     def baz(a: A):
       return a.m()  # Test `Override` target
  |}
           ~expected:(fun pyrefly_api ->
             [
               {
                 Expected.callable = function_regular pyrefly_api "test.baz" |> Target.from_regular;
                 call_graph =
                   [
                     ( "13:9-13:14",
                       ExpressionCallees.from_call
                         (CallCallees.create
                            ~call_targets:
                              [
                                CallTarget.create_regular
                                  ~implicit_receiver:true
                                  ~receiver_class:(class_id_exn pyrefly_api "test.A")
                                  (override_regular
                                     pyrefly_api
                                     ~class_name:"test.A"
                                     ~method_name:"m");
                              ]
                            ()) );
                   ];
                 returned_callables =
                   [
                     CallTarget.create_regular (function_regular pyrefly_api "test.foo");
                     CallTarget.create_regular (function_regular pyrefly_api "test.bar");
                   ];
               };
             ])
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_higher_order_call_graph_fixpoint
           ~source:
             {|
     def decorator(f):
       def foo():
         bar()
         f()
         return
       def bar():
         return
       return foo  # Test the closure of `foo`
     @decorator
     def baz():
       return
     def main():
       baz()
  |}
           ~expected:(fun pyrefly_api ->
             [
               {
                 Expected.callable = function_regular pyrefly_api "test.main" |> Target.from_regular;
                 call_graph =
                   [
                     ( "14:2-14:7",
                       ExpressionCallees.from_call
                         (CallCallees.create
                            ~call_targets:
                              [
                                CallTarget.create
                                  (create_parameterized_target
                                     ~regular:(function_regular pyrefly_api "test.decorator.foo")
                                     ~parameters:
                                       [
                                         ( AccessPath.Root.CapturedVariable
                                             (AccessPath.CapturedVariable.FromFunction
                                                {
                                                  name = "f";
                                                  defining_function =
                                                    Ast.Reference.create "test.decorator";
                                                }),
                                           function_regular pyrefly_api "test.baz"
                                           |> Target.from_regular );
                                       ]);
                              ]
                            ()) );
                   ];
                 returned_callables = [];
               };
             ])
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_higher_order_call_graph_fixpoint
           ~source:
             {|
     def baz():
       return
     def foo(f):
       return baz
     def bar():
       def g():
         return
       return foo(g)  # Test skip analysis
  |}
           ~skip_analysis_targets:(fun pyrefly_api ->
             Target.HashSet.of_list [function_regular pyrefly_api "test.foo" |> Target.from_regular])
           ~expected:(fun pyrefly_api ->
             [
               {
                 Expected.callable = function_regular pyrefly_api "test.bar" |> Target.from_regular;
                 call_graph =
                   [
                     ( "7:2-8:10",
                       ExpressionCallees.from_define
                         (DefineCallees.create
                            ~define_targets:
                              [
                                CallTarget.create_regular (function_regular pyrefly_api "test.bar.g");
                              ]
                            ()) );
                     ( "9:13-9:14|identifier|g",
                       ExpressionCallees.from_identifier
                         (IdentifierCallees.create
                            ~if_called:
                              (CallCallees.create
                                 ~call_targets:
                                   [
                                     CallTarget.create_regular
                                       (function_regular pyrefly_api "test.bar.g");
                                   ]
                                 ())
                            ()) );
                     ( "9:9-9:15",
                       ExpressionCallees.from_call
                         (CallCallees.create
                            ~call_targets:
                              [CallTarget.create_regular (function_regular pyrefly_api "test.foo")]
                            ~higher_order_parameters:
                              (HigherOrderParameterMap.from_list
                                 [
                                   {
                                     index = 0;
                                     call_targets =
                                       [
                                         CallTarget.create_regular
                                           (function_regular pyrefly_api "test.bar.g");
                                       ];
                                     unresolved = CallGraph.Unresolved.False;
                                   };
                                 ])
                            ()) );
                   ];
                 returned_callables = [];
               };
             ])
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_higher_order_call_graph_fixpoint
           ~source:
             {|
     class A:
       @classmethod
       def f(x, g):
         return g
     def foo():
       return
     @functools.partial(A.f, "abc")
     def bar():
       return foo
     def baz():
       return bar()  # Test resolving calls that require redirecting expressions
  |}
           ~expected:(fun pyrefly_api ->
             [
               {
                 Expected.callable = function_regular pyrefly_api "test.baz" |> Target.from_regular;
                 call_graph =
                   [
                     ( "12:9-12:14",
                       ExpressionCallees.from_call
                         (CallCallees.create
                            ~call_targets:
                              [CallTarget.create_regular (function_regular pyrefly_api "test.bar")]
                            ()) );
                   ];
                 returned_callables =
                   [CallTarget.create_regular (function_regular pyrefly_api "test.foo")];
               };
             ])
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_higher_order_call_graph_fixpoint
           ~source:
             {|
     from typing import ParamSpec, Protocol, TypeVar
     T = TypeVar("T")
     P = ParamSpec("P")
     class DecoratorProtocol(Protocol):
       def __call__(self, func: Callable[P, T]) -> Callable[P, T]: ...
     def log(flag: bool) -> DecoratorProtocol:
       def inner(func: Callable[P, T]) -> Callable[P, T]:
         def wrapper(*args: P.args, **kwargs: P.kwargs) -> T:
           return func(*args, **kwargs)
         return wrapper
       return inner
     @log(flag=True)
     def foo():
       return  # Test building higher order call graphs for decorated targets
  |}
           ~expected:(fun pyrefly_api ->
             [
               {
                 Expected.callable = function_regular pyrefly_api "test.log" |> Target.from_regular;
                 call_graph =
                   [
                     ( "8:2-11:18",
                       ExpressionCallees.from_define
                         (DefineCallees.create
                            ~define_targets:
                              [
                                CallTarget.create_regular
                                  (function_regular pyrefly_api "test.log.inner");
                              ]
                            ()) );
                     ( "12:9-12:14|identifier|inner",
                       ExpressionCallees.from_identifier
                         (IdentifierCallees.create
                            ~if_called:
                              (CallCallees.create
                                 ~call_targets:
                                   [
                                     CallTarget.create_regular
                                       (function_regular pyrefly_api "test.log.inner");
                                   ]
                                 ())
                            ()) );
                   ];
                 returned_callables =
                   [CallTarget.create_regular (function_regular pyrefly_api "test.log.inner")];
               };
               {
                 Expected.callable =
                   function_regular_decorated pyrefly_api "test.foo" |> Target.from_regular;
                 call_graph =
                   [
                     ( "13:1-13:15",
                       ExpressionCallees.from_call
                         (CallCallees.create
                            ~call_targets:
                              [CallTarget.create_regular (function_regular pyrefly_api "test.log")]
                            ()) );
                     ( "13:1-13:15|artificial-call|for-decorated-target",
                       ExpressionCallees.from_call
                         (CallCallees.create
                            ~call_targets:
                              [
                                CallTarget.create
                                  (create_parameterized_target
                                     ~regular:(function_regular pyrefly_api "test.log.inner")
                                     ~parameters:
                                       [
                                         ( create_positional_parameter 0 "func",
                                           function_regular pyrefly_api "test.foo"
                                           |> Target.from_regular );
                                       ]);
                              ]
                            ~unresolved:
                              (CallGraph.Unresolved.True
                                 CallGraph.Unresolved.UnexpectedCalleeExpression)
                            ()) );
                     ( "14:0-15:8|identifier|foo",
                       ExpressionCallees.from_identifier
                         (IdentifierCallees.create
                            ~if_called:
                              (CallCallees.create
                                 ~call_targets:
                                   [
                                     CallTarget.create_regular
                                       (function_regular pyrefly_api "test.foo");
                                   ]
                                 ())
                            ()) );
                   ];
                 returned_callables =
                   [
                     CallTarget.create
                       (create_parameterized_target
                          ~regular:(function_regular pyrefly_api "test.log.inner.wrapper")
                          ~parameters:
                            [
                              ( AccessPath.Root.CapturedVariable
                                  (AccessPath.CapturedVariable.FromFunction
                                     {
                                       name = "func";
                                       defining_function = Ast.Reference.create "test.log.inner";
                                     }),
                                function_regular pyrefly_api "test.foo" |> Target.from_regular );
                            ]);
                     CallTarget.create_regular (function_regular pyrefly_api "test.foo");
                   ];
               };
             ])
           ();
      (* TODO: This test requires the higher order call graph analysis to handle attributes. *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_higher_order_call_graph_fixpoint
           ~source:
             {|
     from typing import TypeVar, Generic, Callable
     T = TypeVar("T")
     R = TypeVar("R")
     class classproperty(Generic[T, R]):
       def __init__(self, fget: Callable[[type[T]], R]) -> None:
         self.fget = fget
       # pyre-fixme[14]:
       def __get__(self, obj: object, obj_cls_type: type[T]) -> R:
         # pyre-fixme[16]:
         return self.fget.__get__(None, obj_cls_type)()
     def foo():
       return
     class MyClass:
       @classproperty
       def bar(cls):
         return foo
     def main(o: MyClass):
       return o.bar()  # Test storing decorated functions into object attributes
  |}
           ~expected:(fun pyrefly_api ->
             [
               {
                 Expected.callable = function_regular pyrefly_api "test.main" |> Target.from_regular;
                 call_graph =
                   [
                     ( "19:9-19:16",
                       ExpressionCallees.from_call (CallCallees.create ~call_targets:[] ()) );
                   ];
                 returned_callables = [];
               };
               {
                 Expected.callable =
                   create_parameterized_target_with_values
                     ~regular:
                       (method_regular
                          pyrefly_api
                          ~class_name:"test.classproperty"
                          ~method_name:"__init__")
                     ~parameters:
                       [
                         ( create_positional_parameter 1 "fget",
                           method_regular pyrefly_api ~class_name:"test.MyClass" ~method_name:"bar"
                           |> Target.from_regular
                           |> Target.ParameterValue.create ~implicit_receiver:true );
                       ];
                 call_graph = [];
                 returned_callables = [];
               };
               {
                 Expected.callable =
                   method_regular_decorated
                     pyrefly_api
                     ~class_name:"test.MyClass"
                     ~method_name:"bar"
                   |> Target.from_regular;
                 call_graph =
                   [
                     ( "15:3-15:16|artificial-call|for-decorated-target",
                       ExpressionCallees.from_call
                         (CallCallees.create
                            ~init_targets:
                              [
                                CallTarget.create
                                  ~implicit_receiver:true
                                  ~receiver_class:(class_id_exn pyrefly_api "test.classproperty")
                                  (create_parameterized_target_with_values
                                     ~regular:
                                       (method_regular
                                          pyrefly_api
                                          ~class_name:"test.classproperty"
                                          ~method_name:"__init__")
                                     ~parameters:
                                       [
                                         ( create_positional_parameter 1 "fget",
                                           method_regular
                                             pyrefly_api
                                             ~class_name:"test.MyClass"
                                             ~method_name:"bar"
                                           |> Target.from_regular
                                           |> Target.ParameterValue.create ~implicit_receiver:false
                                         );
                                       ]);
                              ]
                            ~new_targets:
                              [
                                CallTarget.create_regular
                                  ~is_static_method:true
                                  (method_regular
                                     pyrefly_api
                                     ~class_name:"builtins.object"
                                     ~method_name:"__new__");
                              ]
                            ()) );
                     ( "16:2-17:14|identifier|bar",
                       ExpressionCallees.from_identifier
                         (IdentifierCallees.create
                            ~if_called:
                              (CallCallees.create
                                 ~call_targets:
                                   [
                                     CallTarget.create_regular
                                       (method_regular
                                          pyrefly_api
                                          ~class_name:"test.MyClass"
                                          ~method_name:"bar");
                                   ]
                                 ())
                            ()) );
                   ];
                 returned_callables = [];
               };
             ])
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_higher_order_call_graph_fixpoint
           ~source:
             {|
     def decorator(f):
       def inner(*args, **kwargs):
         f(*args, **kwargs)
       return inner
     def foo():
       return
     @unknown_decorator
     @decorator
     def bar():
       return foo  # Cannot resolve callees on the decorator
     def main():
       return bar()
  |}
           ~expected:(fun pyrefly_api ->
             [
               {
                 Expected.callable = function_regular pyrefly_api "test.main" |> Target.from_regular;
                 call_graph =
                   [
                     ( "13:9-13:14",
                       ExpressionCallees.from_call
                         (CallCallees.create
                            ~call_targets:
                              [
                                CallTarget.create
                                  (create_parameterized_target
                                     ~regular:(function_regular pyrefly_api "test.decorator.inner")
                                     ~parameters:
                                       [
                                         ( AccessPath.Root.CapturedVariable
                                             (AccessPath.CapturedVariable.FromFunction
                                                {
                                                  name = "f";
                                                  defining_function =
                                                    Ast.Reference.create "test.decorator";
                                                }),
                                           function_regular pyrefly_api "test.bar"
                                           |> Target.from_regular );
                                       ]);
                              ]
                            ()) );
                   ];
                 returned_callables = [];
               };
             ])
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_higher_order_call_graph_fixpoint
           ~source:
             {|
     from typing import Callable, TypeVar
     _TClass = TypeVar("_TClass")
     _TReturnType = TypeVar("_TReturnType")
     class BaseCachedProperty(Generic[_TClass, _TReturnType]):
       def __init__(
           self, f: Callable[[_TClass], _TReturnType], doc: str = ...
       ) -> None: ...
       def __get__(
           self, obj: None, cls: Type[_TClass]
       ) -> BaseCachedProperty[_TClass, _TReturnType]: ...
     def foo():
       return
     class A:
       @BaseCachedProperty  # Test decorators with stub
       async def bar(self):
         return foo
     def main(a: A):
       return await a.bar()
  |}
           ~expected:(fun pyrefly_api ->
             [
               {
                 Expected.callable = function_regular pyrefly_api "test.main" |> Target.from_regular;
                 call_graph =
                   [
                     ( "19:15-19:22",
                       ExpressionCallees.from_call
                         (CallCallees.create
                            ~call_targets:
                              [
                                CallTarget.create_regular
                                  ~implicit_receiver:true
                                  (method_regular
                                     pyrefly_api
                                     ~class_name:"test.A"
                                     ~method_name:"bar");
                              ]
                            ()) );
                   ];
                 returned_callables =
                   [CallTarget.create_regular (function_regular pyrefly_api "test.foo")];
               };
             ])
           ();
      (* This test requires our higher order call graph analysis to handle attributes. *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_higher_order_call_graph_fixpoint
           ~source:
             {|
     from typing import Callable, TypeVar
     _TClass = TypeVar("_TClass")
     _TReturnType = TypeVar("_TReturnType")
     _TAwaitableReturnType = TypeVar("_TAwaitableReturnType", bound=Awaitable[object])
     class BaseCachedProperty(Generic[_TClass, _TReturnType]):
       def __init__(
           self, f: Callable[[_TClass], _TReturnType], doc: str = ...
       ) -> None:
         self.f = f
       def __get__(
           self, obj: None, cls: Type[_TClass]
       ) -> BaseCachedProperty[_TClass, _TReturnType]: ...
     class async_cached_property(BaseCachedProperty[_TClass, _TAwaitableReturnType]): ...
     def foo():
       return
     class A:
       @async_cached_property  # Test overrides
       async def bar(self):
         return foo
     def main(a: A):
       return await a.bar()
  |}
           ~expected:(fun pyrefly_api ->
             [
               {
                 Expected.callable = function_regular pyrefly_api "test.main" |> Target.from_regular;
                 call_graph =
                   [
                     ( "22:15-22:22",
                       ExpressionCallees.from_call (CallCallees.create ~call_targets:[] ()) );
                   ];
                 returned_callables = [];
               };
             ])
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_higher_order_call_graph_fixpoint
           ~source:
             {|
     from pyre_extensions import classproperty
     __classproperty__ = classproperty
     def foo():
       return
     class A:
       @__classproperty__  # Test property targets in `__classproperty__(test.A.name)`
       def name(cls):
         return foo
     def main(a: A):
       return A.name  # Test accessing a property
  |}
           ~expected:(fun pyrefly_api ->
             [
               {
                 Expected.callable =
                   method_regular_decorated pyrefly_api ~class_name:"test.A" ~method_name:"name"
                   |> Target.from_regular;
                 call_graph =
                   [
                     ( "7:3-7:20|artificial-call|for-decorated-target",
                       ExpressionCallees.from_call
                         (CallCallees.create
                            ~call_targets:
                              [
                                CallTarget.create_regular
                                  (function_regular pyrefly_api "pyre_extensions.classproperty");
                              ]
                            ()) );
                     ( "8:2-9:14|identifier|name",
                       ExpressionCallees.from_identifier
                         (IdentifierCallees.create
                            ~if_called:
                              (CallCallees.create
                                 ~call_targets:
                                   [
                                     CallTarget.create_regular
                                       (method_regular
                                          pyrefly_api
                                          ~class_name:"test.A"
                                          ~method_name:"name");
                                   ]
                                 ())
                            ()) );
                   ];
                 returned_callables =
                   [
                     CallTarget.create_regular
                       (method_regular pyrefly_api ~class_name:"test.A" ~method_name:"name");
                   ];
               };
               {
                 Expected.callable = function_regular pyrefly_api "test.main" |> Target.from_regular;
                 call_graph =
                   [
                     ( "11:9-11:10|identifier|A",
                       ExpressionCallees.from_identifier
                         (IdentifierCallees.create
                            ~if_called:
                              (CallCallees.create
                                 ~new_targets:
                                   [
                                     CallTarget.create_regular
                                       ~is_static_method:true
                                       (method_regular
                                          pyrefly_api
                                          ~class_name:"builtins.object"
                                          ~method_name:"__new__");
                                   ]
                                 ~init_targets:
                                   [
                                     CallTarget.create_regular
                                       ~implicit_receiver:true
                                       ~receiver_class:(class_id_exn pyrefly_api "test.A")
                                       (method_regular
                                          pyrefly_api
                                          ~class_name:"builtins.object"
                                          ~method_name:"__init__");
                                   ]
                                 ())
                            ()) );
                     ( "11:9-11:15",
                       ExpressionCallees.from_attribute_access
                         (AttributeAccessCallees.create ~is_attribute:false ()) );
                   ];
                 returned_callables =
                   [
                     (* TODO(T276351487): Handle `__classproperty__`. *)
                     CallTarget.create_regular
                       (method_regular pyrefly_api ~class_name:"test.A" ~method_name:"name");
                   ];
               };
             ])
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_higher_order_call_graph_fixpoint
           ~source:
             {|
     __property__ = property
     def foo():
       return
     class A:
       @__property__  # Test property targets in `__property__(test.A.name)`
       def name(self):
         return foo
     def main(a: A):
       return a.name  # Test accessing a property
  |}
           ~expected:(fun pyrefly_api ->
             [
               {
                 Expected.callable = function_regular pyrefly_api "test.main" |> Target.from_regular;
                 call_graph =
                   [
                     ( "10:9-10:15",
                       ExpressionCallees.from_attribute_access
                         (AttributeAccessCallees.create
                            ~property_targets:
                              [
                                CallTarget.create_regular
                                  ~implicit_receiver:true
                                  ~receiver_class:(class_id_exn pyrefly_api "test.A")
                                  (method_regular
                                     pyrefly_api
                                     ~class_name:"test.A"
                                     ~method_name:"name");
                              ]
                            ~if_called:
                              (CallCallees.create
                                 ~call_targets:
                                   [
                                     CallTarget.create_regular
                                       (function_regular pyrefly_api "test.foo");
                                   ]
                                 ())
                            ~is_attribute:false
                            ()) );
                   ];
                 returned_callables =
                   [CallTarget.create_regular (function_regular pyrefly_api "test.foo")];
               };
             ])
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_higher_order_call_graph_fixpoint
           ~source:
             {|
     class Base:
       def __enter__(self):
         return self
       def __exit__(self, exc_type: object, exc_value: object, traceback: object) -> bool:
         return False
       def __call__(self, func):
         def inner(*args, **kwds):
           func(*args, **kwds)
         return inner
     base = Base()
     @base  # Test custom context manager
     def contextmanager(x):
       print(x)
     class Subclass(Base):
       def __init__(self, name):
         self.name = name
     subclass = Subclass("123")
     @subclass  # Test subclass and decorator factory
     def contextmanager_subclass(x):
       print(x)
  |}
           ~expected:(fun pyrefly_api ->
             [
               {
                 Expected.callable =
                   function_regular_decorated pyrefly_api "test.contextmanager"
                   |> Target.from_regular;
                 call_graph =
                   [
                     ( "12:1-12:5|artificial-call|for-decorated-target",
                       ExpressionCallees.from_call
                         (CallCallees.create
                            ~call_targets:
                              [
                                CallTarget.create
                                  ~implicit_receiver:true
                                  ~implicit_dunder_call:true
                                  ~receiver_class:(class_id_exn pyrefly_api "test.Base")
                                  (create_parameterized_target
                                     ~regular:
                                       (method_regular
                                          pyrefly_api
                                          ~class_name:"test.Base"
                                          ~method_name:"__call__")
                                     ~parameters:
                                       [
                                         ( create_positional_parameter 1 "func",
                                           function_regular pyrefly_api "test.contextmanager"
                                           |> Target.from_regular );
                                       ]);
                              ]
                            ()) );
                     ( "13:0-14:10|identifier|contextmanager",
                       ExpressionCallees.from_identifier
                         (IdentifierCallees.create
                            ~if_called:
                              (CallCallees.create
                                 ~call_targets:
                                   [
                                     CallTarget.create_regular
                                       (function_regular pyrefly_api "test.contextmanager");
                                   ]
                                 ())
                            ()) );
                   ];
                 returned_callables =
                   [
                     CallTarget.create
                       (create_parameterized_target
                          ~regular:(function_regular pyrefly_api "test.Base.__call__.inner")
                          ~parameters:
                            [
                              ( AccessPath.Root.CapturedVariable
                                  (AccessPath.CapturedVariable.FromFunction
                                     {
                                       name = "func";
                                       defining_function = Ast.Reference.create "test.Base.__call__";
                                     }),
                                function_regular pyrefly_api "test.contextmanager"
                                |> Target.from_regular );
                            ]);
                   ];
               };
               {
                 Expected.callable =
                   function_regular_decorated pyrefly_api "test.contextmanager_subclass"
                   |> Target.from_regular;
                 call_graph =
                   [
                     ( "19:1-19:9|artificial-call|for-decorated-target",
                       ExpressionCallees.from_call
                         (CallCallees.create
                            ~call_targets:
                              [
                                CallTarget.create
                                  ~implicit_receiver:true
                                  ~implicit_dunder_call:true
                                  ~receiver_class:(class_id_exn pyrefly_api "test.Subclass")
                                  (create_parameterized_target
                                     ~regular:
                                       (method_regular
                                          pyrefly_api
                                          ~class_name:"test.Base"
                                          ~method_name:"__call__")
                                     ~parameters:
                                       [
                                         ( create_positional_parameter 1 "func",
                                           function_regular
                                             pyrefly_api
                                             "test.contextmanager_subclass"
                                           |> Target.from_regular );
                                       ]);
                              ]
                            ()) );
                     ( "20:0-21:10|identifier|contextmanager_subclass",
                       ExpressionCallees.from_identifier
                         (IdentifierCallees.create
                            ~if_called:
                              (CallCallees.create
                                 ~call_targets:
                                   [
                                     CallTarget.create_regular
                                       (function_regular pyrefly_api "test.contextmanager_subclass");
                                   ]
                                 ())
                            ()) );
                   ];
                 returned_callables =
                   [
                     CallTarget.create
                       (create_parameterized_target
                          ~regular:(function_regular pyrefly_api "test.Base.__call__.inner")
                          ~parameters:
                            [
                              ( AccessPath.Root.CapturedVariable
                                  (AccessPath.CapturedVariable.FromFunction
                                     {
                                       name = "func";
                                       defining_function = Ast.Reference.create "test.Base.__call__";
                                     }),
                                function_regular pyrefly_api "test.contextmanager_subclass"
                                |> Target.from_regular );
                            ]);
                   ];
               };
             ])
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_higher_order_call_graph_fixpoint
           ~source:
             {|
     class Base:
       def __call__(self, func):
         ...  # Test when `__call__` is a stub
     class Subclass(Base):
       def __init__(self, name):
         self.name = name
     subclass = Subclass("123")
     @subclass
     def stub_contextmanager(x):
       print(x)
  |}
           ~expected:(fun pyrefly_api ->
             [
               {
                 Expected.callable =
                   function_regular_decorated pyrefly_api "test.stub_contextmanager"
                   |> Target.from_regular;
                 call_graph =
                   [
                     ( "9:1-9:9|artificial-call|for-decorated-target",
                       ExpressionCallees.from_call
                         (CallCallees.create
                            ~call_targets:
                              [
                                CallTarget.create_regular
                                  ~implicit_receiver:true
                                  ~implicit_dunder_call:true
                                  ~receiver_class:(class_id_exn pyrefly_api "test.Subclass")
                                  (method_regular
                                     pyrefly_api
                                     ~class_name:"test.Base"
                                     ~method_name:"__call__");
                              ]
                            ()) );
                     ( "10:0-11:10|identifier|stub_contextmanager",
                       ExpressionCallees.from_identifier
                         (IdentifierCallees.create
                            ~if_called:
                              (CallCallees.create
                                 ~call_targets:
                                   [
                                     CallTarget.create_regular
                                       (function_regular pyrefly_api "test.stub_contextmanager");
                                   ]
                                 ())
                            ()) );
                   ];
                 returned_callables =
                   [
                     CallTarget.create_regular
                       (function_regular pyrefly_api "test.stub_contextmanager");
                   ];
               };
             ])
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_higher_order_call_graph_fixpoint
           ~source:
             {|
     def decorator():
       ...
     def foo():
       return
     @decorator  # Test stub call targets in decorated targets
     def bar():
       return foo
  |}
           ~expected:(fun pyrefly_api ->
             [
               {
                 Expected.callable =
                   function_regular_decorated pyrefly_api "test.bar" |> Target.from_regular;
                 call_graph =
                   [
                     ( "6:1-6:10|artificial-call|for-decorated-target",
                       ExpressionCallees.from_call
                         (CallCallees.create
                            ~call_targets:
                              [
                                CallTarget.create_regular
                                  (function_regular pyrefly_api "test.decorator");
                              ]
                            ()) );
                     ( "7:0-8:12|identifier|bar",
                       ExpressionCallees.from_identifier
                         (IdentifierCallees.create
                            ~if_called:
                              (CallCallees.create
                                 ~call_targets:
                                   [
                                     CallTarget.create_regular
                                       (function_regular pyrefly_api "test.bar");
                                   ]
                                 ())
                            ()) );
                   ];
                 returned_callables =
                   [CallTarget.create_regular (function_regular pyrefly_api "test.bar")];
               };
             ])
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_higher_order_call_graph_fixpoint
           ~source:
             {|
     class A():
       @property
       def foo(self) -> int:
         return 0
     def bar(f):
       return
     def main(a: A):
       x = a.foo  # Test not propagating property targets
       return bar(x)
  |}
           ~expected:(fun pyrefly_api ->
             [
               {
                 Expected.callable = function_regular pyrefly_api "test.main" |> Target.from_regular;
                 call_graph =
                   [
                     ( "10:9-10:15",
                       ExpressionCallees.from_call
                         (CallCallees.create
                            ~call_targets:
                              [CallTarget.create_regular (function_regular pyrefly_api "test.bar")]
                            ()) );
                     ( "9:6-9:11",
                       ExpressionCallees.from_attribute_access
                         (AttributeAccessCallees.create
                            ~property_targets:
                              [
                                CallTarget.create_regular
                                  ~implicit_receiver:true
                                  ~receiver_class:(class_id_exn pyrefly_api "test.A")
                                  ~return_type:(Some ReturnType.integer)
                                  (method_regular
                                     pyrefly_api
                                     ~class_name:"test.A"
                                     ~method_name:"foo");
                              ]
                            ~is_attribute:false
                            ()) );
                   ];
                 returned_callables = [];
               };
             ])
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_higher_order_call_graph_fixpoint
           ~source:
             {|
     def foo():
       return
     def bar(f):
       return
     def main():
       return bar(foo)  # Test treating parameters as being called
  |}
           ~called_when_parameter:(fun pyrefly_api ->
             [function_regular pyrefly_api "test.foo" |> Target.from_regular]
             |> Target.HashSet.of_list)
           ~expected:(fun pyrefly_api ->
             [
               {
                 Expected.callable = function_regular pyrefly_api "test.main" |> Target.from_regular;
                 call_graph =
                   [
                     ( "7:9-7:17",
                       ExpressionCallees.from_call
                         (CallCallees.create
                            ~call_targets:
                              [CallTarget.create_regular (function_regular pyrefly_api "test.bar")]
                            ~higher_order_parameters:
                              (HigherOrderParameterMap.from_list
                                 [
                                   {
                                     index = 0;
                                     call_targets =
                                       [
                                         CallTarget.create_regular
                                           (function_regular pyrefly_api "test.foo");
                                       ];
                                     unresolved = CallGraph.Unresolved.False;
                                   };
                                 ])
                            ()) );
                     ( "7:13-7:16|identifier|foo",
                       ExpressionCallees.from_identifier
                         (IdentifierCallees.create
                            ~if_called:
                              (CallCallees.create
                                 ~call_targets:
                                   [
                                     CallTarget.create_regular
                                       (function_regular pyrefly_api "test.foo");
                                   ]
                                 ())
                            ()) );
                   ];
                 returned_callables = [];
               };
             ])
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_higher_order_call_graph_fixpoint
           ~source:
             {|
     def decorator(f):
       def inner(x):
         return f(x)
       return inner
     def main():
       @decorator
       def inner(x):
         return
       inner(0)  # Test creating decorated targets for inner functions
  |}
           ~expected:(fun pyrefly_api ->
             [
               {
                 Expected.callable = function_regular pyrefly_api "test.main" |> Target.from_regular;
                 call_graph =
                   [
                     ( "10:2-10:10",
                       ExpressionCallees.from_call
                         (CallCallees.create
                            ~call_targets:
                              [
                                CallTarget.create
                                  (create_parameterized_target
                                     ~regular:(function_regular pyrefly_api "test.decorator.inner")
                                     ~parameters:
                                       [
                                         ( AccessPath.Root.CapturedVariable
                                             (AccessPath.CapturedVariable.FromFunction
                                                {
                                                  name = "f";
                                                  defining_function =
                                                    Ast.Reference.create "test.decorator";
                                                }),
                                           function_regular pyrefly_api "test.main.inner"
                                           |> Target.from_regular );
                                       ]);
                              ]
                            ()) );
                     ( "7:2-9:10",
                       ExpressionCallees.from_define
                         (DefineCallees.create
                            ~define_targets:
                              [
                                CallTarget.create
                                  (create_parameterized_target
                                     ~regular:(function_regular pyrefly_api "test.decorator.inner")
                                     ~parameters:
                                       [
                                         ( AccessPath.Root.CapturedVariable
                                             (AccessPath.CapturedVariable.FromFunction
                                                {
                                                  name = "f";
                                                  defining_function =
                                                    Ast.Reference.create "test.decorator";
                                                }),
                                           function_regular pyrefly_api "test.main.inner"
                                           |> Target.from_regular );
                                       ]);
                              ]
                            ()) );
                   ];
                 returned_callables = [];
               };
             ])
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_higher_order_call_graph_fixpoint
           ~source:
             {|
     def decorator_factory(x: int):
       def decorator(f):
         def inner(x):
           return f(x)
         return inner
       return decorator
     def main():
       @decorator_factory(1)
       def inner(x):
         return
       inner(0)  # Test creating decorated targets for inner functions
  |}
           ~expected:(fun pyrefly_api ->
             [
               {
                 Expected.callable = function_regular pyrefly_api "test.main" |> Target.from_regular;
                 call_graph =
                   [
                     ( "12:2-12:10",
                       ExpressionCallees.from_call
                         (CallCallees.create
                            ~call_targets:
                              [
                                CallTarget.create_regular
                                  (function_regular pyrefly_api "test.main.inner");
                                CallTarget.create
                                  (create_parameterized_target
                                     ~regular:
                                       (function_regular
                                          pyrefly_api
                                          "test.decorator_factory.decorator.inner")
                                     ~parameters:
                                       [
                                         ( AccessPath.Root.CapturedVariable
                                             (AccessPath.CapturedVariable.FromFunction
                                                {
                                                  name = "f";
                                                  defining_function =
                                                    Ast.Reference.create
                                                      "test.decorator_factory.decorator";
                                                }),
                                           function_regular pyrefly_api "test.main.inner"
                                           |> Target.from_regular );
                                       ]);
                              ]
                            ()) );
                     ( "9:2-11:10",
                       ExpressionCallees.from_define
                         (DefineCallees.create
                            ~define_targets:
                              [
                                CallTarget.create_regular
                                  (function_regular pyrefly_api "test.main.inner");
                                CallTarget.create
                                  (create_parameterized_target
                                     ~regular:
                                       (function_regular
                                          pyrefly_api
                                          "test.decorator_factory.decorator.inner")
                                     ~parameters:
                                       [
                                         ( AccessPath.Root.CapturedVariable
                                             (AccessPath.CapturedVariable.FromFunction
                                                {
                                                  name = "f";
                                                  defining_function =
                                                    Ast.Reference.create
                                                      "test.decorator_factory.decorator";
                                                }),
                                           function_regular pyrefly_api "test.main.inner"
                                           |> Target.from_regular );
                                       ]);
                              ]
                            ()) );
                   ];
                 returned_callables = [];
               };
             ])
           ();
    ]


let () = "callGraphFixpoint" >::: [test_higher_order_call_graph_fixpoint] |> Test.run
