(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Ast

module CallTarget : sig
  type t =
    | Regular of {
        target: Target.t;
        receiver_class: PyreflyTypes.ClassId.t option;
      }
    | Init of {
        target: Target.t;
        receiver_class: PyreflyTypes.ClassId.t option;
      }
    | New of {
        target: Target.t;
        receiver_class: PyreflyTypes.ClassId.t option;
      }
    | Property of {
        target: Target.t;
        receiver_class: PyreflyTypes.ClassId.t option;
      }

  val target : t -> Target.t

  val receiver_class : t -> PyreflyTypes.ClassId.t option
end

module NestedCallees : sig
  type t =
    (* Given call `x(1)(...)`, this is the callees on `x(1)` *)
    | NestedCall of CallTarget.t list
    (* Given call `x.y.z(...)`, this is the callees on `x.y` *)
    | NestedAttributeAccess of CallTarget.t list
    | None
end

val shim_calls
  :  display_api:PyreflyTypes.DisplayApi.t ->
  class_mro:(PyreflyTypes.ClassId.t -> PyreflyTypes.ClassId.t list) ->
  class_id_from_name_opt:(Reference.t -> PyreflyTypes.ClassId.t option) ->
  callable_id_from_name_opt:(Reference.t -> PyreflyTypes.CallableId.t option) ->
  callees:CallTarget.t list ->
  nested_callees:NestedCallees.t ->
  arguments:Expression.Call.Argument.t list ->
  Shims.ShimArgumentMapping.t option
