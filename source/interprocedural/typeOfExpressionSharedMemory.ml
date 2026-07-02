(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type t = Pyrefly of PyreflyApi.ReadOnly.t

let create ~pyre_api ~callables_to_definitions_map:_ () =
  match pyre_api with
  | PyrePysaApi.ReadOnly.Pyrefly pyrefly_api -> Pyrefly pyrefly_api


let compute_or_retrieve_pysa_type type_of_expression_shared_memory ~pyre_in_context expression =
  match type_of_expression_shared_memory with
  | Pyrefly pyrefly_api -> (
      match Ast.Expression.origin expression with
      | Some _ ->
          (* This is an artificial expression that pyrefly doesn't know about. *)
          PyrePysaApi.PysaType.from_pyrefly_type Analysis.PysaTypes.PyreflyType.top
      | None ->
          let define_name = PyrePysaApi.InContext.define_name pyre_in_context in
          PyreflyApi.ReadOnly.get_type_of_expression
            pyrefly_api
            ~define_name
            ~location:(Ast.Node.location expression)
          |> Option.value
               ~default:(PyrePysaApi.PysaType.from_pyrefly_type Analysis.PysaTypes.PyreflyType.top))
