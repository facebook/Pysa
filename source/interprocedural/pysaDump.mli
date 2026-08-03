(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* Decides whether verbose "magic dump" logging is enabled for a given callable during a Pysa
   analysis phase. *)

val should_dump_call_graph
  :  pyrefly_api:PyreflyApi.ReadOnly.t ->
  define:Ast.Statement.Define.t ->
  callable:Target.t ->
  bool

val should_dump_higher_order_call_graph
  :  pyrefly_api:PyreflyApi.ReadOnly.t ->
  define:Ast.Statement.Define.t ->
  callable:Target.t ->
  bool

val should_dump_taint
  :  pyrefly_api:PyreflyApi.ReadOnly.t ->
  define:Ast.Statement.Define.t ->
  callable:Target.t ->
  bool

val should_dump_cfg
  :  pyrefly_api:PyreflyApi.ReadOnly.t ->
  define:Ast.Statement.Define.t ->
  callable:Target.t ->
  bool

val should_dump_perf
  :  pyrefly_api:PyreflyApi.ReadOnly.t ->
  define:Ast.Statement.Define.t ->
  callable:Target.t ->
  bool

val should_dump_perf_higher_order_call_graph
  :  pyrefly_api:PyreflyApi.ReadOnly.t ->
  define:Ast.Statement.Define.t ->
  callable:Target.t ->
  bool
