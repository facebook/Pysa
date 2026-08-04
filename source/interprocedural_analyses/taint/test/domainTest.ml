(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2
open Ast
open Taint
open Domains
open Core
open Test

let test_partition_call_map context =
  let pyrefly_api =
    InterproceduralTest.ScratchPyrePysaProject.setup
      ~context
      ~requires_type_of_expressions:true
      ["test.py", {|
class Foo:
  def bar(self): ...
|}]
    |> InterproceduralTest.ScratchPyrePysaProject.read_only_api
  in
  let pyrefly_in_context =
    Interprocedural.PyreflyApi.InContext.create_at_function_scope
      pyrefly_api
      ~callable_id:
        (Interprocedural.PyreflyApi.ReadOnly.Target.callable_id_from_name_exn
           pyrefly_api
           !&"test.$toplevel")
      ~define_name:!&"test.$toplevel"
      ~call_graph:Interprocedural.CallGraph.DefineCallGraph.empty
  in
  let taint =
    ForwardTaint.singleton CallInfo.declaration (Sources.NamedSource "UserControlled") Frame.initial
  in
  let callee =
    let class_id =
      Interprocedural.PyreflyApi.ReadOnly.class_id_from_name pyrefly_api !&"test.Foo"
    in
    Interprocedural.PyreflyApi.ReadOnly.Target.resolve_method_target
      pyrefly_api
      ~class_id
      ~method_name:"bar"
      ~is_property_setter:false
    |> Option.value_exn ~message:"expected test.Foo.bar to resolve"
  in
  let call_taint1 =
    ForwardTaint.apply_call
      ~pyrefly_in_context
      ~call_site:CallSite.any
      ~location:Location.any
      ~callee
      ~arguments:[]
      ~port:AccessPath.Root.LocalResult
      ~path:[Abstract.TreeDomain.Label.create_name_index "a"]
      ~is_class_method:false
      ~is_static_method:false
      ~call_info_intervals:Domains.ClassIntervals.top
      taint
  in
  let call_taint2 =
    ForwardTaint.apply_call
      ~pyrefly_in_context
      ~call_site:CallSite.any
      ~location:Location.any
      ~callee
      ~arguments:[]
      ~port:AccessPath.Root.LocalResult
      ~path:[Abstract.TreeDomain.Label.create_name_index "a"]
      ~is_class_method:false
      ~is_static_method:false
      ~call_info_intervals:Domains.ClassIntervals.top
      taint
  in
  let joined = ForwardTaint.join call_taint1 call_taint2 in
  assert_equal
    ~cmp:(fun left right -> ForwardTaint.less_or_equal ~left ~right)
    ~printer:ForwardTaint.show
    ~msg:"call info not joined properly"
    joined
    call_taint2;
  assert_bool
    "joined should not be less or equal to non trivial access path in call trace"
    (ForwardTaint.less_or_equal ~left:joined ~right:call_taint1);
  let split partition =
    ( Map.Poly.find partition true |> Option.value ~default:ForwardTaint.bottom,
      Map.Poly.find partition false |> Option.value ~default:ForwardTaint.bottom )
  in
  let matches, does_not_match =
    ForwardTaint.partition
      ForwardTaint.kind
      By
      ~f:(fun kind -> Sources.equal kind (Sources.NamedSource "UserControlled"))
      joined
    |> split
  in
  assert_equal
    ~msg:"does_not_match must be equal to bottom"
    ~printer:ForwardTaint.show
    does_not_match
    ForwardTaint.bottom;
  assert_equal ~msg:"matches must be equal to original" ~printer:ForwardTaint.show matches joined


let () = "taint_domain" >::: ["partition_call_map" >:: test_partition_call_map] |> Test.run
