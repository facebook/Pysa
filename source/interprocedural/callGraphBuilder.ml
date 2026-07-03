(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* CallGraphBuilder: implements the logic to statically compute the call graph,
 * given a function definition.
 *
 * Note that the call graph is highly tuned for the taint analysis and might be
 * unsound for other analyses.
 *)

open Core
open Ast
open Statement
open Expression
open Pyre
open CallGraph
module PyrePysaLogic = Analysis.PyrePysaLogic
module AstResult = PyreflyApi.AstResult

let log ~debug format =
  if debug then
    Log.dump format
  else
    Log.log ~section:`CallGraph format


module ResolvedStringify = struct
  type t =
    | Str
    | Repr

  let from_method_name method_name =
    let base_name =
      match String.lsplit2 method_name ~on:'$' with
      | Some (prefix, _) when not (String.is_empty prefix) -> prefix
      | _ -> method_name
    in
    match base_name with
    | "__str__" -> Some Str
    | "__repr__" -> Some Repr
    | _ -> None


  let to_method_name = function
    | Str -> "__str__"
    | Repr -> "__repr__"
end

(* Resolve a call to `str(x)` into `x.__str__()` or `x.__repr__()` *)
let resolve_stringify_call ~pyrefly_in_context ~outer_expression_identifier _expression =
  (* When using pyrefly, use the callee of the artificial call resolved by pyrefly *)
  let call_graph = PyreflyApi.InContext.call_graph pyrefly_in_context in
  match
    DefineCallGraph.resolve_expression call_graph ~expression_identifier:outer_expression_identifier
  with
  | Some (ExpressionCallees.Call { CallCallees.call_targets = { CallTarget.target; _ } :: _; _ })
    -> (
      (* Note: the result is only used to create artificial AST nodes. It does not affect the
         analysis. *)
      let method_name =
        Target.collect_nested_regular_targets target
        |> List.find_map ~f:(fun regular ->
               match regular with
               | Target.Regular.Method { Target.Method.method_name; _ }
               | Target.Regular.Override { Target.Method.method_name; _ } ->
                   ResolvedStringify.from_method_name method_name
               | _ -> None)
      in
      match method_name with
      | Some resolved -> resolved
      | None -> ResolvedStringify.Repr)
  | Some (ExpressionCallees.Call { CallCallees.unresolved = Unresolved.True _; _ }) ->
      ResolvedStringify.Repr
  | _ ->
      Format.asprintf
        "Missing or unexpected call graph edge for expression identifier %a"
        ExpressionIdentifier.pp
        outer_expression_identifier
      |> failwith


(* Rewrite certain calls for the interprocedural analysis (e.g, pysa).
 * These rewrites are done symbolically during the analysis.
 * These should be preferred over AST transformations (see `preprocess_special_calls`). *)
let apply_identified_shim_call ~identified_callee ~arguments =
  let open Shims.ShimArgumentMapping in
  match identified_callee, arguments with
  | Some Shims.IdentifiedCallee.FunctoolsPartial, _actual_callable :: actual_arguments ->
      Some
        {
          identifier = "functools.partial";
          callee = Target.Argument { index = 0 };
          arguments =
            List.mapi actual_arguments ~f:(fun index_minus_one { Call.Argument.name; _ } ->
                {
                  Argument.name = name >>| Node.value;
                  value = Target.Argument { index = index_minus_one + 1 };
                });
          discard_higher_order_parameters = true;
        }
  | ( Some Shims.IdentifiedCallee.MultiprocessingProcess,
      [
        { Call.Argument.name = Some { Node.value = target; _ }; _ };
        {
          Call.Argument.value = { Node.value = Expression.Tuple process_arguments; _ };
          name = Some { Node.value = args; _ };
        };
      ] )
    when target |> AccessPath.Root.chop_parameter_prefix |> String.equal "target"
         && args |> AccessPath.Root.chop_parameter_prefix |> String.equal "args" ->
      Some
        {
          identifier = "multiprocessing.Process";
          callee = Target.Argument { index = 0 };
          arguments =
            List.mapi process_arguments ~f:(fun index _ ->
                {
                  Argument.name = None;
                  value = Target.GetTupleElement { index; inner = Target.Argument { index = 1 } };
                });
          discard_higher_order_parameters = true;
        }
  | _ -> None


let shim_special_calls ~callees ~arguments =
  let define_name_equals ~name call_target =
    SpecialCallResolution.CallTarget.target call_target
    |> Target.get_regular
    |> Target.Regular.define_name
    >>| Reference.show
    >>| String.equal name
    |> Option.value ~default:false
  in
  let identified_callee =
    if List.exists callees ~f:(define_name_equals ~name:"functools.partial.__new__") then
      Some Shims.IdentifiedCallee.FunctoolsPartial
    else if
      List.exists
        callees
        ~f:(define_name_equals ~name:"multiprocessing.process.BaseProcess.__init__")
    then
      Some Shims.IdentifiedCallee.MultiprocessingProcess
    else
      None
  in
  apply_identified_shim_call ~identified_callee ~arguments


(* Rewrite certain calls for the interprocedural analysis (e.g, pysa).
 * These rewrites are done as AST transformations. In general, this should be
 * avoided, hence this is only used for a few specific functions in the standard
 * library. One main difference between preprocessing and shimming is that shims
 * are considered additional calls, where preprocessing completely removes the
 * original call. This is preferred for things like `str`/`iter`/`next`. *)
let preprocess_special_calls
    ~pyrefly_in_context
    ~location:call_location
    {
      Call.callee = { Node.location = callee_location; _ } as callee;
      arguments;
      origin = call_origin;
    }
  =
  let attribute_access ~base ~method_name ~origin =
    {
      Node.value = Expression.Name (Name.Attribute { base; attribute = method_name; origin });
      location = callee_location;
    }
  in
  let is_starred expression =
    match Node.value expression with
    | Expression.Starred _ -> true
    | _ -> false
  in
  match Node.value callee, arguments with
  | Name (Name.Identifier "str"), [{ Call.Argument.value; name = None }] when not (is_starred value)
    ->
      (* str() takes an optional encoding and errors - if these are present, the call shouldn't be
         redirected: https://docs.python.org/3/library/stdtypes.html#str *)
      let origin =
        Origin.create ?base:call_origin ~location:call_location Origin.StrCallToDunderMethod
      in
      let method_name =
        resolve_stringify_call
          ~pyrefly_in_context
          ~outer_expression_identifier:(ExpressionIdentifier.ArtificialCall origin)
          value
        |> ResolvedStringify.to_method_name
      in
      let callee = attribute_access ~base:value ~method_name ~origin:(Some origin) in
      Some { Call.callee; arguments = []; origin = Some origin }
  | Name (Name.Identifier "abs"), [{ Call.Argument.value; name = None }] when not (is_starred value)
    ->
      let origin = Some (Origin.create ?base:call_origin ~location:call_location Origin.AbsCall) in
      Some
        {
          Call.callee = attribute_access ~base:value ~method_name:"__abs__" ~origin;
          arguments = [];
          origin;
        }
  | Name (Name.Identifier "repr"), [{ Call.Argument.value; name = None }]
    when not (is_starred value) ->
      let origin = Some (Origin.create ?base:call_origin ~location:call_location Origin.ReprCall) in
      Some
        {
          Call.callee = attribute_access ~base:value ~method_name:"__repr__" ~origin;
          arguments = [];
          origin;
        }
  | Name (Name.Identifier "iter"), [{ Call.Argument.value; name = None }]
    when not (is_starred value) ->
      (* Only handle `iter` with a single argument here. *)
      let origin = Some (Origin.create ?base:call_origin ~location:call_location Origin.IterCall) in
      Some
        {
          Call.callee = attribute_access ~base:value ~method_name:"__iter__" ~origin;
          arguments = [];
          origin;
        }
  | Name (Name.Identifier "next"), [{ Call.Argument.value; name = None }]
    when not (is_starred value) ->
      (* Only handle `next` with a single argument here. *)
      let origin = Some (Origin.create ?base:call_origin ~location:call_location Origin.NextCall) in
      Some
        {
          Call.callee = attribute_access ~base:value ~method_name:"__next__" ~origin;
          arguments = [];
          origin;
        }
  | Name (Name.Identifier "anext"), [{ Call.Argument.value; name = None }]
    when not (is_starred value) ->
      (* Only handle `anext` with a single argument here. *)
      let origin = Some (Origin.create ?base:call_origin ~location:call_location Origin.NextCall) in
      Some
        {
          Call.callee = attribute_access ~base:value ~method_name:"__anext__" ~origin;
          arguments = [];
          origin;
        }
  | _ -> None


let shim_for_call ~pyrefly_api ~callables_to_definitions_map ~callees ~nested_callees ~arguments =
  match shim_special_calls ~callees ~arguments with
  | Some identified_callee -> Some identified_callee
  | None ->
      let callable_exists name =
        CallablesSharedMemory.ReadOnly.callable_from_reference callables_to_definitions_map name
        |> Option.is_some
      in
      SpecialCallResolution.shim_calls
        ~class_mro:(PyreflyApi.ReadOnly.class_mro pyrefly_api)
        ~callable_exists
        ~callees
        ~nested_callees
        ~arguments


let create_shim_callee_expression ~debug ~callable ~location ~call shim =
  log ~debug "Found shim for call `%a`: `%a`" Call.pp call Shims.ShimArgumentMapping.pp shim;
  Shims.ShimArgumentMapping.create_artificial_call ~call_location:location call shim
  |> function
  | Ok { Call.callee = shim_callee_expression; _ } -> Some shim_callee_expression
  | Error error ->
      let () =
        Log.warning
          "Error applying shim argument mapping: %s for expression `%a` in `%a` at %a"
          error
          Expression.pp
          (Node.create_with_default_location (Expression.Call call))
          Target.pp
          callable
          Location.pp
          location
      in
      None


let preprocess_call ~pyrefly_in_context ~location original_call =
  preprocess_special_calls ~pyrefly_in_context ~location original_call
  |> Option.value ~default:original_call


let rec preprocess_expression ~pyrefly_in_context ~callable expression =
  (* This uses `Expression.Mapper` to recursively rewrite the given expression.
   *
   * Each `map_XXX` function is responsible for calling `Mapper.map` on sub-expressions to
   * properly recurse down the AST. This is why we sometimes call `default_map_XXX`.
   *
   * The mapper will use the same `pyrefly_in_context` for all sub-expressions. If
   * we need to update the context (for instance, for generators), we need to call
   * `preprocess_expression ~pyrefly_in_context` with the new context instead of
   * calling `Mapper.map`. This is why this function is recursive.
   *)
  let map_binary_operator ~mapper ~location ({ BinaryOperator.left; _ } as binary_operator) =
    BinaryOperator.lower_to_call ~location ~callee_location:left.Node.location binary_operator
    |> Mapper.default_map_call_node ~mapper ~location
  in
  let map_comparison_operator
      ~mapper
      ~location
      ({ ComparisonOperator.left; _ } as comparison_operator)
    =
    match
      ComparisonOperator.lower_to_expression
        ~location
        ~callee_location:left.location
        comparison_operator
    with
    | Some { Node.value = Expression.Call call; _ } ->
        Mapper.default_map_call_node ~mapper ~location call
    | _ -> Mapper.default_map_comparison_operator_node ~mapper ~location comparison_operator
  in
  let map_slice ~mapper ~location slice =
    Slice.lower_to_call ~location slice |> Mapper.default_map_call_node ~mapper ~location
  in
  let map_subscript ~mapper ~location { Subscript.base; index; origin } =
    Expression.Call
      (Subscript.lower_to_call
         ~location
         { Subscript.base = Mapper.map ~mapper base; index = Mapper.map ~mapper index; origin })
    |> Node.create ~location
  in
  let map_comprehension_generators generators =
    let fold_generator generators { Comprehension.Generator.target; iterator; conditions; async } =
      let conditions =
        List.map ~f:(preprocess_expression ~pyrefly_in_context ~callable) conditions
      in
      (* We explicitly do NOT preprocess the target and iterator since we need to call
         `generator_assignment` during the taint fixpoint, using the original expressions. Updating
         the `target` and `iterator` here would lead to inconsistencies of the type context. *)
      let generator = { Comprehension.Generator.target; iterator; conditions; async } in
      generator :: generators
    in
    List.rev (List.fold ~f:fold_generator ~init:[] generators)
  in
  let map_comprehension ~mapper:_ ~location ~make_node { Comprehension.element; generators } =
    let generators = map_comprehension_generators generators in
    {
      Comprehension.element = preprocess_expression ~pyrefly_in_context ~callable element;
      generators;
    }
    |> make_node
    |> Node.create ~location
  in
  let map_generator = map_comprehension ~make_node:(fun e -> Expression.Generator e) in
  let map_list_comprehension =
    map_comprehension ~make_node:(fun e -> Expression.ListComprehension e)
  in
  let map_set_comprehension =
    map_comprehension ~make_node:(fun e -> Expression.SetComprehension e)
  in
  let map_dictionary_comprehension
      ~mapper:_
      ~location
      { Comprehension.element = Dictionary.Entry.KeyValue.{ key; value }; generators }
    =
    let generators = map_comprehension_generators generators in
    Expression.DictionaryComprehension
      {
        Comprehension.element =
          {
            Dictionary.Entry.KeyValue.key = preprocess_expression ~pyrefly_in_context ~callable key;
            value = preprocess_expression ~pyrefly_in_context ~callable value;
          };
        generators;
      }
    |> Node.create ~location
  in
  let map_call ~mapper ~location call =
    preprocess_call ~pyrefly_in_context ~location call
    |> Mapper.default_map_call_node ~mapper ~location
  in
  Mapper.map
    ~mapper:
      (Mapper.create_default
         ~map_binary_operator
         ~map_comparison_operator
         ~map_slice
         ~map_subscript
         ~map_generator
         ~map_dictionary_comprehension
         ~map_list_comprehension
         ~map_set_comprehension
         ~map_call
         ())
    expression


let preprocess_assignments statement =
  let statement =
    (* Note that there are cases where we perform two consecutive redirects.
     * For instance, for `d[j] += x` *)
    match statement with
    | {
     Node.value = Statement.AugmentedAssign ({ AugmentedAssign.target; _ } as augmented_assignment);
     location;
    } ->
        (* TODO(T225700656): Augmented assignments can be lowered into `__iadd__`, `__add__` or
           `__radd__`. We only support the first case here. *)
        let call =
          AugmentedAssign.lower_to_expression
            ~location
            ~callee_location:target.Node.location
            augmented_assignment
        in
        let target =
          map_origin
            ~f:(fun origin ->
              Some
                (Origin.create
                   ?base:origin
                   ~location:(Node.location target)
                   Origin.AugmentedAssignLHS))
            target
        in
        {
          Node.location;
          value =
            Statement.Assign
              {
                Assign.target;
                annotation = None;
                value = Some call;
                origin = Some (Origin.create ~location Origin.AugmentedAssignStatement);
              };
        }
    | _ -> statement
  in
  match statement with
  | {
   Node.value =
     Statement.Assign
       {
         Assign.target =
           { Node.value = Expression.Subscript { base; index; origin = subscript_origin; _ }; _ };
         value = Some value_expression;
         origin = assign_origin;
         _;
       };
   location;
  } ->
      (* TODO(T187636576): For now, we translate assignments such as `d[a] = b` into
         `d.__setitem__(a, b)`. Unfortunately, this won't work for multi-target assignments such as
         `x, y[a], z = w`. In the future, we should implement proper logic to handle those. *)
      let index_argument = { Call.Argument.value = index; name = None } in
      let value_argument = { Call.Argument.value = value_expression; name = None } in
      {
        Node.location;
        value =
          Statement.Expression
            {
              Node.location;
              value =
                Expression.Call
                  {
                    callee =
                      {
                        value =
                          Name
                            (Name.Attribute
                               {
                                 base;
                                 attribute = "__setitem__";
                                 origin =
                                   Some
                                     (Origin.create
                                        ?base:subscript_origin
                                        ~location
                                        Origin.SubscriptSetItem);
                               });
                        location;
                      };
                    arguments = [index_argument; value_argument];
                    origin =
                      Some (Origin.create ?base:assign_origin ~location Origin.SubscriptSetItem);
                  };
            };
      }
  | statement -> statement


let preprocess_parameter_default_value = preprocess_expression

(* This must be called *once* before analyzing a statement in a control flow graph. *)
let preprocess_statement ~pyrefly_in_context ~callable statement =
  (* First, preprocess assignments *)
  let { Node.location; value } = preprocess_assignments statement in
  (* Then, preprocess expressions nested witin the statement *)
  let preprocess_expression = preprocess_expression ~pyrefly_in_context ~callable in
  let value =
    match value with
    | Statement.Assign { target; value; annotation; origin } ->
        Statement.Assign
          {
            target = preprocess_expression target;
            value = Option.map ~f:preprocess_expression value;
            annotation;
            origin;
          }
    | Assert { test; message; origin } ->
        Statement.Assert { test = preprocess_expression test; message; origin }
    | Delete expressions -> Statement.Delete (List.map ~f:preprocess_expression expressions)
    | Expression expression -> Statement.Expression (preprocess_expression expression)
    | Raise { expression; from } ->
        Statement.Raise
          {
            expression = Option.map ~f:preprocess_expression expression;
            from = Option.map ~f:preprocess_expression from;
          }
    | Return { expression; is_implicit } ->
        Statement.Return
          { expression = Option.map ~f:preprocess_expression expression; is_implicit }
    | TypeAlias { name; type_params; value } ->
        Statement.TypeAlias
          { name = preprocess_expression name; type_params; value = preprocess_expression value }
    | Define _
    | Break
    | Class _
    | Continue
    | Global _
    | Import _
    | Nonlocal _
    | Pass ->
        value
    | Try _ ->
        (* Try statements are lowered down in `Cfg.create`, but they are preserved in the final Cfg.
           They should be ignored. *)
        value
    | For _
    | If _
    | Match _
    | With _
    | While _ ->
        failwith "For/If/Match/With/While nodes should always be rewritten by `Cfg.create`"
    | AugmentedAssign _ ->
        failwith "AugmentedAssign nodes should always be rewritten by `preprocess_assignments`"
  in
  { Node.location; value }


(* This must be called *once* before analyzing a generator. *)
let preprocess_generator ~pyrefly_in_context ~callable generator =
  let { Assign.target; value; annotation; origin } = Statement.generator_assignment generator in
  {
    Assign.target = preprocess_expression ~pyrefly_in_context ~callable target;
    Assign.value = Option.map ~f:(preprocess_expression ~pyrefly_in_context ~callable) value;
    annotation;
    origin;
  }


(* The result of finding higher order function callees inside a callable. *)
module HigherOrderCallGraph = struct
  type t = {
    returned_callables: CallTarget.Set.t;
    call_graph: DefineCallGraph.t;
        (* Higher order function callees (i.e., parameterized targets) and potentially regular
           callees. *)
  }
  [@@deriving equal, show]

  let empty = { returned_callables = CallTarget.Set.bottom; call_graph = DefineCallGraph.empty }

  let merge
      { returned_callables = left_returned_callables; call_graph = left_call_graph }
      { returned_callables = right_returned_callables; call_graph = right_call_graph }
    =
    {
      returned_callables = CallTarget.Set.join left_returned_callables right_returned_callables;
      call_graph = DefineCallGraph.merge left_call_graph right_call_graph;
    }


  let is_empty { returned_callables; call_graph } =
    CallTarget.Set.is_bottom returned_callables && DefineCallGraph.is_empty call_graph


  let to_json_alist { returned_callables; call_graph } =
    let returned_callables =
      returned_callables |> CallTarget.Set.elements |> List.map ~f:CallTarget.to_json
    in
    ["returned_callables", `List returned_callables; "calls", DefineCallGraph.to_json call_graph]


  include MakeSaveCallGraph (struct
    type nonrec t = t

    let name = "higher order call graphs"

    let is_empty = is_empty

    let to_json_alist = to_json_alist
  end)

  module State = struct
    include
      Abstract.MapDomain.Make
        (struct
          include AccessPath.Root

          let name = "variables"

          let absence_implicitly_maps_to_bottom = true
        end)
        (CallTarget.Set)

    let empty = bottom

    let initialize_from_roots ~pyrefly_api ~callables_to_definitions_map alist =
      alist
      |> List.filter_map ~f:(fun (root, { Target.ParameterValue.target; implicit_receiver }) ->
             (* ASTs use `AccessPath.parameter_prefix` to distinguish local variables from
                parameters, but using parameters from the define does not result in creating
                parameterized targets whose parameter names contain `AccessPath.parameter_prefix`.
                To be consistent, we use the former. In addition, we treat formal arguments and
                local variables as the same variant under `AccessPath.Root`, so that we can look up
                the value bound to an `Identifier` easily. *)
             let root =
               match root with
               | AccessPath.Root.PositionalParameter { name; _ }
               | AccessPath.Root.NamedParameter { name } ->
                   Some (AccessPath.Root.Variable name)
               | AccessPath.Root.CapturedVariable captured_variable ->
                   Some
                     (PyreflyApi.ReadOnly.state_root_of_captured_variable
                        pyrefly_api
                        captured_variable)
               | AccessPath.Root.Variable _ ->
                   failwith "unexpected variable root in parameterized target"
               | _ -> None
             in
             match root with
             | None -> None
             | Some root ->
                 let is_class_method, is_static_method =
                   CallablesSharedMemory.ReadOnly.get_method_kind
                     callables_to_definitions_map
                     target
                 in
                 Some
                   ( root,
                     target
                     |> CallTarget.create ~implicit_receiver ~is_class_method ~is_static_method
                     |> CallTarget.Set.singleton ))
      |> of_list


    let initialize_from_callable ~pyrefly_api ~callables_to_definitions_map = function
      | Target.Regular _ -> bottom
      | Target.Parameterized { parameters; _ } ->
          parameters
          |> Target.ParameterMap.to_alist
          |> initialize_from_roots ~pyrefly_api ~callables_to_definitions_map
  end

  module MakeTransferFunction (Context : sig
    (* Inputs. *)
    val pyrefly_api : PyreflyApi.ReadOnly.t

    val get_callee_model : Target.t -> t option

    val debug : bool

    val module_qualifier : Reference.t

    val define : Ast.Statement.Define.t Node.t

    val define_name : Reference.t

    val callable : Target.t

    val callables_to_definitions_map : CallablesSharedMemory.ReadOnly.t

    val skip_analysis_targets : Target.HashSet.t

    val called_when_parameter : Target.HashSet.t

    val skip_inlining_higher_order_functions : Target.HashSet.t

    val profiler : CallGraphProfiler.t

    val maximum_target_depth : int

    val maximum_parameterized_targets_at_call_site : int option

    val maximum_parameterized_targets_when_analyzing_define : int option

    val input_define_call_graph : DefineCallGraph.t

    (* Outputs. *)
    val output_define_call_graph : DefineCallGraph.t ref
  end) =
  struct
    type t = State.t [@@deriving show]

    let bottom = State.bottom

    let less_or_equal = State.less_or_equal

    let join = State.join

    let widen ~previous ~next ~iteration = State.widen ~prev:previous ~next ~iteration

    let log format =
      if Context.debug then
        Log.dump format
      else
        Log.log ~section:`CallGraph format


    let store_callees ~weak ~root ~callees state =
      State.update state root ~f:(function
          | None -> callees
          | Some existing_callees ->
              if weak then CallTarget.Set.join existing_callees callees else callees)


    let returned_callables_of_target { CallTarget.target; _ } =
      log "Fetching returned callables for `%a`" Target.pp_pretty target;
      match Context.get_callee_model target with
      | Some { returned_callables; _ } -> returned_callables
      | None -> CallTarget.Set.bottom


    let returned_callables_of_targets call_targets =
      call_targets
      |> List.map ~f:returned_callables_of_target
      |> Algorithms.fold_balanced ~f:CallTarget.Set.join ~init:CallTarget.Set.bottom


    let self_variable =
      if Ast.Statement.Define.is_method (Node.value Context.define) then
        let { Ast.Statement.Define.signature = { parameters; _ }; _ } = Node.value Context.define in
        match AccessPath.normalize_parameters parameters with
        | { root = AccessPath.Root.PositionalParameter { position = 0; _ }; qualified_name; _ } :: _
          ->
            Some (AccessPath.Root.Variable qualified_name)
        | _ -> None
      else
        None


    let is_decorated_target = Target.is_decorated Context.callable

    let get_returned_callables state =
      self_variable
      >>| (fun self_variable ->
            (* For `__init__`, any functions stored in `self` would be returned, in order to
               propagate them. *)
            match Target.get_regular Context.callable with
            | Target.Regular.Method { method_name = "__init__"; _ } -> State.get self_variable state
            | _ -> CallTarget.Set.bottom)
      |> Option.value ~default:CallTarget.Set.bottom
      |> CallTarget.Set.join (State.get AccessPath.Root.LocalResult state)


    (* Join a set of call targets from the original call graph with targets inferred from the
       analysis. *)
    let join_original_targets_with_inferred ~original ~inferred =
      let fold_inferred { CallTarget.target = inferred_target; _ } original_targets =
        (* If we inferred a parameterized target, drop the non-parameterized version from the
           original set *)
        match inferred_target with
        | Target.Parameterized { regular = inferred_target; _ } ->
            CallTarget.Set.transform
              CallTarget.Set.Element
              Filter
              ~f:(fun { CallTarget.target; _ } ->
                match target with
                | Target.Regular target when Target.Regular.equal target inferred_target -> false
                | _ -> true)
              original_targets
        | _ -> original_targets
      in
      let original =
        CallTarget.Set.fold CallTarget.Set.Element ~init:original ~f:fold_inferred inferred
      in
      CallTarget.Set.join original inferred


    let cartesian_product_with_limit ~limit ~message_when_exceeding_limit list =
      match limit, list with
      | Some limit, head :: tail when limit > 0 ->
          let number_of_combinations =
            List.fold
              ~init:(List.length head)
              ~f:(fun so_far element -> element |> List.length |> Int.( * ) so_far)
              tail
          in
          if number_of_combinations > limit then (
            log
              "%s due to `%d` exceeding limit `%d`"
              message_when_exceeding_limit
              number_of_combinations
              limit;
            None)
          else
            Some (Algorithms.cartesian_product list)
      | _, _ -> Some (Algorithms.cartesian_product list)


    module AnalyzeDecoratedTargetsResult = struct
      type t = {
        (* A subset of the input call targets that are of `kind=Decorated`. *)
        decorated_targets: CallTarget.t list;
        (* A subset of the input call targets that are not of `kind=Decorated`. *)
        non_decorated_targets: CallTarget.t list;
        (* The result of evaluating the input call targets. *)
        result_targets: CallTarget.t list;
      }
    end

    (* Analyze any list of `CallTarget.t`s, which may contain decorated targets -- we would get
       their returned callable. The reason is that, the original call graph contains call edges to
       `foo@decorated` targets, which are symbolic target representing the decorated callables.
       Those need to be replaced by the actual function being called, which will usually be some
       kind of `decorator.inner[captured(f):foo]` target. *)
    let resolve_decorated_targets call_targets =
      let decorated_targets, non_decorated_targets =
        List.partition_tf
          ~f:(fun { CallTarget.target; _ } -> Target.is_decorated target)
          call_targets
      in
      log
        "Decorated targets: `%a`"
        Target.List.pp_pretty_with_kind
        (List.map ~f:CallTarget.target decorated_targets);
      (* When substituting a decorated call target (e.g. `foo@decorated`) with its returned
         callables, the decorated call target carries the type-aware receiver information resolved
         at the call site (`implicit_receiver`). The `@decorated` model's returned callables may not
         preserve it, so we OR the decorated call target's `implicit_receiver` into each returned
         callable: whenever the call site has `implicit_receiver=true`, the substituted callable
         gets it too. We never downgrade a returned callable that is already
         `implicit_receiver=true` (this is an OR, not an overwrite).

         The returned callable may be a completely different function/method than the decorated
         call-site target (e.g. `returns_function_decorator.replacement`, or a `*args` wrapper
         `inner`), so we keep its own `receiver_class`/`is_class_method`/`is_static_method` and only
         set `implicit_receiver`. *)
      let resolve_decorated_target ({ CallTarget.implicit_receiver; _ } as decorated_target) =
        returned_callables_of_target decorated_target
        |> CallTarget.Set.transform
             CallTarget.Set.Element
             Map
             ~f:(fun ({ CallTarget.implicit_receiver = returned_implicit_receiver; _ } as returned)
                ->
               {
                 returned with
                 CallTarget.implicit_receiver = returned_implicit_receiver || implicit_receiver;
               })
      in
      {
        AnalyzeDecoratedTargetsResult.decorated_targets;
        non_decorated_targets;
        result_targets =
          decorated_targets
          |> List.map ~f:resolve_decorated_target
          |> Algorithms.fold_balanced ~f:CallTarget.Set.join ~init:CallTarget.Set.bottom
          |> CallTarget.Set.join (CallTarget.Set.of_list non_decorated_targets)
          |> CallTarget.Set.elements;
      }


    let validate_target target =
      let exceed_depth = Target.depth target > Context.maximum_target_depth in
      let contain_recursive_target = Target.contain_recursive_target target in
      let skip_analysis =
        Target.should_skip_analysis ~skip_analysis_targets:Context.skip_analysis_targets target
      in
      if contain_recursive_target || exceed_depth || skip_analysis then
        let () =
          log
            "Invalid target: `%a` (contain_recursive_target: `%b`. exceed_depth: `%b`. \
             skip_analysis: `%b`)"
            Target.pp_pretty_with_kind
            target
            contain_recursive_target
            exceed_depth
            skip_analysis
        in
        None
      else
        Some target


    (* Results of analyzing a certain kind of call targets (e.g., `call_targets` or `init_targets`)
       on a callee expression. *)
    module AnalyzeCalleeResult = struct
      type t = {
        (* Transforming the input call targets by providing parameter targets. *)
        parameterized_targets: CallTarget.t list;
        (* The sublist of the input call target list that are of `kind=Decorated`. *)
        decorated_targets: CallTarget.t list;
        (* The sublist of the input call target list that are not transformed above. We create these
           regular targets when (1) no parameter targets exist or (2) we cannot find function bodies
           of the callee, so that the taint analysis can still use `higher_order_parameters`. *)
        non_parameterized_targets: CallTarget.t list;
        (* The sublist of the input call target list that are stubs. *)
        stub_targets: Target.t list;
      }
    end

    let analyze_callee_targets
        ~location
        ~call
        ~arguments
        ~argument_callees
        ~track_apply_call_step_name
        callee_targets
      =
      let track_apply_call_step step f =
        CallGraphProfiler.track_apply_call_step
          ~profiler:Context.profiler
          ~analysis:Forward
          ~step
          ~call_target:
            (* A hack to distinguish the profiling of different calls to
               `analyze_callee_targets`. *)
            (Some (Target.Regular.Object track_apply_call_step_name |> Target.from_regular))
          ~location
          ~argument:None
          ~f
      in
      let formal_arguments_if_non_stub target =
        if Target.is_override target || Target.is_object target then
          (* TODO(T204630385): It is possible for a target to be an `Override`, which we do not
             handle for now, or an `Object`, such as a function-typed variable that cannot be
             resolved by the original call graph building. *)
          None
        else
          match
            CallablesSharedMemory.ReadOnly.get_signature Context.callables_to_definitions_map target
          with
          | Some { CallablesSharedMemory.CallableSignature.is_stub_like; parameters; _ } ->
              if is_stub_like then
                let () = log "Callable `%a` is a stub" Target.pp_pretty_with_kind target in
                None
              else
                parameters
                |> AstResult.to_option
                >>| AccessPath.normalize_parameters
                >>| List.map ~f:(fun { AccessPath.NormalizedParameter.root; _ } -> root)
          | None ->
              log "Cannot find define for callable `%a`" Target.pp_pretty_with_kind target;
              None
      in
      let create_parameter_target_excluding_args_kwargs (parameter_target, (_, argument_matches)) =
        match argument_matches, parameter_target with
        | { AccessPath.root = AccessPath.Root.StarParameter _; _ } :: _, _
        | { AccessPath.root = AccessPath.Root.StarStarParameter _; _ } :: _, _ ->
            (* TODO(T215864108): Since we do not distinguish paths under the same `Root`, we may run
               into conflicts in `of_alist_exn` below, which is avoided by excluding those cases,
               such as kwargs and args. *)
            None
        | { AccessPath.root; _ } :: _, Some { CallTarget.target; implicit_receiver; _ } ->
            Some (root, { Target.ParameterValue.target; implicit_receiver })
        | _ -> (* TODO: Consider the remaining `argument_matches`. *) None
      in
      let non_parameterized_targets ~parameterized_targets call_targets =
        let regular_targets_from_parameterized =
          List.filter_map parameterized_targets ~f:(function
              | { CallTarget.target = Target.Parameterized { regular; _ }; _ } -> Some regular
              | { CallTarget.target = Target.Regular _; _ } -> None)
        in
        let is_parameterized { CallTarget.target; _ } =
          let regular = Target.get_regular target in
          List.exists regular_targets_from_parameterized ~f:(Target.Regular.equal regular)
        in
        List.filter call_targets ~f:(fun call_target -> call_target |> is_parameterized |> not)
      in
      let {
        AnalyzeDecoratedTargetsResult.decorated_targets;
        non_decorated_targets = _;
        result_targets = call_targets_from_callee;
      }
        =
        track_apply_call_step ComputeCalleeTargets (fun () ->
            resolve_decorated_targets callee_targets)
      in
      let create_call_target = function
        | Some callee_target :: parameter_targets ->
            let callee_regular, closure =
              match CallTarget.target callee_target with
              | Target.Regular regular -> regular, Target.ParameterMap.empty
              | Target.Parameterized { regular; parameters } -> regular, parameters
            in
            if
              Core.Hash_set.mem
                Context.skip_inlining_higher_order_functions
                (Target.from_regular callee_regular)
            then
              let () =
                log
                  "Callable `%a` is marked as @SkipInliningHigherOrderFunctions"
                  Target.pp_pretty_with_kind
                  (Target.from_regular callee_regular)
              in
              None
            else
              let formal_arguments =
                callee_regular |> Target.from_regular |> formal_arguments_if_non_stub
              in
              let parameter_targets, arguments =
                match ImplicitArgument.implicit_argument ~is_implicit_new:false callee_target with
                | ImplicitArgument.Callee ->
                    ( None :: parameter_targets,
                      { Call.Argument.name = None; value = call.Call.callee } :: arguments )
                | ImplicitArgument.CalleeBase ->
                    let { Node.value = call_expression; location } = call.Call.callee in
                    let self =
                      match call_expression with
                      | Expression.Name (Name.Attribute { base; _ }) -> base
                      | _ ->
                          (* Default to a placeholder self if we don't understand/retain information
                             of what self is. *)
                          Expression.Constant Constant.NoneLiteral |> Node.create ~location
                    in
                    ( None :: parameter_targets,
                      { Call.Argument.name = None; value = self } :: arguments )
                | ImplicitArgument.None -> parameter_targets, arguments
              in
              log
                "Formal arguments of callee regular `%a`: `%a`"
                Target.Regular.pp
                callee_regular
                AccessPath.Root.List.pp
                (Option.value ~default:[] formal_arguments);
              let parameters =
                formal_arguments
                >>| AccessPath.match_actuals_to_formals arguments
                >>| List.zip_exn parameter_targets
                >>| List.filter_map ~f:create_parameter_target_excluding_args_kwargs
                >>| Target.ParameterMap.of_alist_exn
                |> Option.value ~default:Target.ParameterMap.empty
                |> Target.ParameterMap.union
                     (fun _ _ right ->
                       (* The formal argument should shadow variables from the closure that share
                          the same name. *)
                       Some right)
                     closure
              in
              log
                "Parameter targets: %a"
                (Target.ParameterMap.pp Target.ParameterValue.pp_pretty)
                parameters;
              if Target.ParameterMap.is_empty parameters then
                None
              else
                Target.Parameterized { regular = callee_regular; parameters }
                |> validate_target
                >>| fun target -> { callee_target with CallTarget.target }
        | _ -> None
      in
      (* Treat an empty list as a single element list so that in each result of the cartesian
         product, there is still one element for the empty list, which preserves the indices of
         arguments. *)
      let to_option_list = function
        | [] -> [None]
        | list -> List.map ~f:(fun target -> Some target) list
      in
      let parameterized_targets =
        track_apply_call_step CreateParameterizedTargets (fun () ->
            argument_callees
            |> List.map ~f:(fun call_targets ->
                   call_targets |> CallTarget.Set.elements |> to_option_list)
            |> List.cons (to_option_list call_targets_from_callee)
            |> cartesian_product_with_limit
                 ~limit:Context.maximum_parameterized_targets_at_call_site
                 ~message_when_exceeding_limit:
                   "Avoid generating parameterized targets when analyzing call"
            |> Option.value ~default:[]
            |> List.filter_map ~f:create_call_target
            |> List.dedup_and_sort ~compare:CallTarget.compare)
      in
      let non_parameterized_targets =
        track_apply_call_step FindNonParameterizedTargets (fun () ->
            non_parameterized_targets ~parameterized_targets call_targets_from_callee)
      in
      List.iter parameterized_targets ~f:(fun { CallTarget.target; _ } ->
          log "Created parameterized target: `%a`" Target.pp_pretty target);
      let stub_targets =
        List.filter_map call_targets_from_callee ~f:(fun { CallTarget.target; _ } ->
            let is_stub =
              CallablesSharedMemory.ReadOnly.is_stub_like
                Context.callables_to_definitions_map
                target
              |> Option.value ~default:false
            in
            if is_stub then Some target else None)
      in
      {
        AnalyzeCalleeResult.parameterized_targets;
        decorated_targets;
        non_parameterized_targets;
        stub_targets;
      }


    let rec analyze_call ~pyrefly_in_context ~location ~call ~arguments ~state =
      let track_apply_call_step step f =
        CallGraphProfiler.track_apply_call_step
          ~profiler:Context.profiler
          ~analysis:Forward
          ~step
          ~call_target:None
          ~location
          ~argument:None
          ~f
      in
      let analyze_argument
          ~higher_order_parameters
          index
          (state_so_far, additional_higher_order_parameters, decorated_targets)
          { Call.Argument.value = argument; _ }
        =
        let callees, new_state =
          analyze_expression ~pyrefly_in_context ~state:state_so_far ~expression:argument
        in
        let call_targets_from_higher_order_parameters =
          match HigherOrderParameterMap.find_opt higher_order_parameters index with
          | Some { HigherOrderParameter.call_targets; _ } -> call_targets
          | None -> []
        in
        let partition_called_when_parameter =
          CallTarget.Set.fold
            CallTarget.Set.Element
            ~init:(CallTarget.Set.bottom, CallTarget.Set.bottom)
            ~f:(fun call_target (called_when_parameter, not_called_when_parameter) ->
              let is_called_when_parameter =
                call_target
                |> CallTarget.target
                |> Target.strip_parameters
                |> Core.Hash_set.mem Context.called_when_parameter
              in
              if is_called_when_parameter then
                CallTarget.Set.add call_target called_when_parameter, not_called_when_parameter
              else
                called_when_parameter, CallTarget.Set.add call_target not_called_when_parameter)
        in
        let {
          AnalyzeDecoratedTargetsResult.decorated_targets = new_decorated_targets;
          non_decorated_targets = _;
          result_targets;
        }
          =
          call_targets_from_higher_order_parameters
          |> CallTarget.Set.of_list
          |> CallTarget.Set.join callees
          |> CallTarget.Set.elements
          |> resolve_decorated_targets
        in
        let called_when_parameter, not_called_when_parameter =
          (* Partition on the targets after resolving decorated targets, not before resolving.
             Resolving decorated targets may result in targets that needs to be treated as
             `called_when_parameter`. *)
          result_targets |> CallTarget.Set.of_list |> partition_called_when_parameter
        in
        log
          "Finished analyzing argument `%a` -- called_when_parameter: %a. \
           not_called_when_parameter: %a"
          Expression.pp
          argument
          CallTarget.Set.pp
          called_when_parameter
          CallTarget.Set.pp
          not_called_when_parameter;
        let additional_higher_order_parameters =
          if CallTarget.Set.is_bottom called_when_parameter then
            additional_higher_order_parameters
          else
            HigherOrderParameterMap.add
              additional_higher_order_parameters
              {
                HigherOrderParameter.call_targets = CallTarget.Set.elements called_when_parameter;
                index;
                unresolved = Unresolved.False;
              }
        in
        ( ( new_state,
            additional_higher_order_parameters,
            List.rev_append new_decorated_targets decorated_targets ),
          not_called_when_parameter )
      in
      let ({
             CallCallees.call_targets = original_call_targets;
             higher_order_parameters = original_higher_order_parameters;
             unresolved;
             init_targets = original_init_targets;
             (* TODO(T243083593): Resolve decorated targets for __new__ *)
             new_targets = _;
             shim_target = original_shim_target;
             _;
           } as original_call_callees)
        =
        track_apply_call_step ResolveCall (fun () ->
            match DefineCallGraph.resolve_call ~location ~call Context.input_define_call_graph with
            | Some callees -> callees
            | None ->
                let () =
                  Log.error
                    "Call graph of `%a`: `%a`"
                    Target.pp_pretty
                    Context.callable
                    DefineCallGraph.pp
                    Context.input_define_call_graph
                in
                failwith
                  (Format.asprintf
                     "Could not find callees for `%a` in `%a` at `%a` in the call graph (see logs \
                      above)."
                     Ast.Expression.Call.pp
                     call
                     Target.pp_pretty
                     Context.callable
                     Location.pp
                     location))
      in
      (* The analysis of the callee AST handles the redirection to artifically created decorator
         defines. *)
      let callee_return_values, state =
        analyze_expression ~pyrefly_in_context ~state ~expression:call.Call.callee
      in
      let ( (state, additional_higher_order_parameters, decorated_targets_from_arguments),
            argument_callees )
        =
        track_apply_call_step AnalyzeArguments (fun () ->
            List.fold_mapi
              arguments
              ~f:(analyze_argument ~higher_order_parameters:original_higher_order_parameters)
              ~init:(state, HigherOrderParameterMap.empty, []))
      in
      let argument_callees =
        (* When passing an instance of a class with a `__call__` method as an argument, we end up
           inferring an implicit `__call__` target for the argument. When the function is annotated
           as taking the callable class, we should NOT create a parameterized target for it. This is
           similar to the `filter_implicit_dunder_calls` logic in the pyrefly call graph building.
           We check both call_targets and init_targets since constructor calls use init_targets. *)
        let pyrefly_api = Context.pyrefly_api in
        let any_parameter_annotation_has_class callable_class signatures =
          let annotation_has_class annotation =
            let { PyreflyApi.ClassNamesFromType.classes; is_exhaustive = _ } =
              PyreflyApi.ReadOnly.Type.get_class_names pyrefly_api annotation
            in
            let allow_modifier = function
              | PyreflyApi.TypeModifier.Optional
              | ReadOnly
              | Awaitable
              | Coroutine ->
                  true
              | Type
              | TypeVariableBound
              | TypeVariableConstraint ->
                  false
            in
            List.exists classes ~f:(fun { PyreflyApi.ClassWithModifiers.class_name; modifiers } ->
                (not (PyreflyApi.ReadOnly.is_object_class pyrefly_api class_name))
                && List.for_all modifiers ~f:allow_modifier
                && PyreflyApi.ReadOnly.is_subclass
                     pyrefly_api
                     ~parent:class_name
                     ~child:callable_class)
          in
          let parameter_has_class parameter =
            PyreflyApi.ModelQueries.FunctionParameter.annotation parameter
            >>| annotation_has_class
            |> Option.value ~default:false
          in
          List.exists
            signatures
            ~f:(fun { PyreflyApi.ModelQueries.FunctionSignature.parameters; _ } ->
              match parameters with
              | PyreflyApi.ModelQueries.FunctionParameters.List params ->
                  List.exists params ~f:parameter_has_class
              | Ellipsis
              | ParamSpec ->
                  false)
        in
        let filter_argument_callee ~signatures { CallTarget.implicit_dunder_call; target; _ } =
          if not implicit_dunder_call then
            true
          else
            let target =
              target
              |> Target.get_regular
              |> Target.Regular.override_to_method
              |> Target.from_regular
            in
            match target with
            | Target.Regular
                (Target.Regular.Method { class_name = callable_class; method_name = "__call__"; _ })
              ->
                not (any_parameter_annotation_has_class callable_class signatures)
            | _ -> true
        in
        let filter_for_callee_targets callee_targets argument_callees =
          match callee_targets with
          | [{ CallTarget.target = callee; _ }] when not (Target.is_object callee) ->
              let define_name =
                callee
                |> Target.get_regular
                |> Target.Regular.override_to_method
                |> Target.Regular.define_name_exn
              in
              let signatures =
                PyreflyApi.ReadOnly.get_undecorated_signatures pyrefly_api define_name
              in
              List.map argument_callees ~f:(fun call_target_set ->
                  CallTarget.Set.transform
                    CallTarget.Set.Element
                    Filter
                    ~f:(filter_argument_callee ~signatures)
                    call_target_set)
          | _ -> argument_callees
        in
        argument_callees
        |> filter_for_callee_targets original_call_targets
        |> filter_for_callee_targets original_init_targets
      in
      let ( parameterized_call_targets,
            decorated_call_targets,
            non_parameterized_call_targets,
            stub_call_targets )
        =
        let {
          AnalyzeCalleeResult.parameterized_targets = parameterized_callee_return_targets;
          decorated_targets = decorated_callee_return_targets;
          non_parameterized_targets = non_parameterized_callee_return_targets;
          stub_targets = stub_callee_return_targets;
        }
          =
          callee_return_values
          |> CallTarget.Set.elements
          |> analyze_callee_targets
               ~location
               ~call
               ~arguments
               ~argument_callees
               ~track_apply_call_step_name:"callee_return_targets"
        in
        let {
          AnalyzeCalleeResult.parameterized_targets = parameterized_call_targets;
          decorated_targets = decorated_call_targets;
          non_parameterized_targets = non_parameterized_call_targets;
          stub_targets = stub_call_targets;
        }
          =
          analyze_callee_targets
            ~location
            ~call
            ~arguments
            ~argument_callees
            ~track_apply_call_step_name:"call_targets"
            original_call_targets
        in
        ( List.rev_append parameterized_callee_return_targets parameterized_call_targets,
          List.rev_append decorated_callee_return_targets decorated_call_targets,
          List.rev_append non_parameterized_callee_return_targets non_parameterized_call_targets,
          List.rev_append stub_callee_return_targets stub_call_targets )
      in
      let {
        AnalyzeCalleeResult.parameterized_targets = parameterized_init_targets;
        decorated_targets = decorated_init_targets;
        non_parameterized_targets = non_parameterized_init_targets;
        stub_targets = stub_init_targets;
      }
        =
        analyze_callee_targets
          ~location
          ~call
          ~arguments
          ~argument_callees
          ~track_apply_call_step_name:"init_targets"
          original_init_targets
      in
      (* Discard higher order parameters only if each original target is parameterized, except for
         the targets that must be treated as being called. *)
      let new_higher_order_parameters =
        if
          List.is_empty non_parameterized_call_targets
          && List.is_empty non_parameterized_init_targets
        then
          additional_higher_order_parameters
        else
          (* Resolve decorated targets in the original higher order parameters before joining, to
             avoid keeping unresolved `@decorated` targets. *)
          let resolved_original_higher_order_parameters =
            original_higher_order_parameters
            |> HigherOrderParameterMap.to_list
            |> List.map ~f:(fun ({ HigherOrderParameter.call_targets; _ } as hop) ->
                   let { AnalyzeDecoratedTargetsResult.result_targets; _ } =
                     resolve_decorated_targets call_targets
                   in
                   { hop with HigherOrderParameter.call_targets = result_targets })
            |> HigherOrderParameterMap.from_list
          in
          HigherOrderParameterMap.join
            resolved_original_higher_order_parameters
            additional_higher_order_parameters
      in
      let new_shim_target =
        match original_shim_target with
        | Some { ShimTarget.call_targets = shim_call_targets; argument_mapping; _ } ->
            let { AnalyzeDecoratedTargetsResult.result_targets; decorated_targets; _ } =
              resolve_decorated_targets shim_call_targets
            in
            Some { ShimTarget.call_targets = result_targets; decorated_targets; argument_mapping }
        | None -> None
      in
      let new_call_targets =
        parameterized_call_targets
        |> List.rev_append non_parameterized_call_targets
        |> List.dedup_and_sort ~compare:CallTarget.compare
      in
      let new_init_targets =
        parameterized_init_targets
        |> List.rev_append non_parameterized_init_targets
        |> List.dedup_and_sort ~compare:CallTarget.compare
      in
      (* Unset `unresolved` when the original call graph building cannot resolve callees under cases
         like `f()` or `f`. *)
      let new_unresolved =
        match unresolved with
        | Unresolved.True (BypassingDecorators UnknownIdentifierCallee)
        | Unresolved.True (BypassingDecorators UnknownCallCallee)
        | Unresolved.True UnexpectedPyreflyTarget
          when not (CallTarget.Set.is_bottom callee_return_values) ->
            Unresolved.False
        | _ -> unresolved
      in
      track_apply_call_step StoreCallCallees (fun () ->
          Context.output_define_call_graph :=
            DefineCallGraph.set_call_callees
              ~error_if_new:true
              ~location
              ~call
              ~callees:
                {
                  original_call_callees with
                  call_targets = new_call_targets;
                  decorated_targets =
                    decorated_call_targets
                    |> List.rev_append decorated_init_targets
                    |> List.rev_append decorated_targets_from_arguments
                    |> List.dedup_and_sort ~compare:CallTarget.compare;
                  init_targets = new_init_targets;
                  higher_order_parameters = new_higher_order_parameters;
                  shim_target = new_shim_target;
                  unresolved = new_unresolved;
                }
              !Context.output_define_call_graph);
      track_apply_call_step FetchReturnedCallables (fun () ->
          let returned_callables_from_call =
            new_call_targets |> List.rev_append new_init_targets |> returned_callables_of_targets
          in
          (* To avoid false negatives when analyzing targets with `kind=Decorated`, sometimes
           * we allow all function-typed arguments to be passed directly to the return values.
           * - Case 1 is when calls might be unresolved.
           * - Case 2 is when there exists a stub `__init__` target. The stub target's summary
           * is considered as passing through.
           * - Case 3 is when there exists a stub call target. The stub target's summary
           * is considered as passing through.*)
          let pass_through_arguments =
            let exist_stub_init_targets = not (List.is_empty stub_init_targets) in
            let exist_stub_call_targets = not (List.is_empty stub_call_targets) in
            let should_pass_through =
              is_decorated_target
              && (exist_stub_init_targets
                 || Unresolved.is_unresolved unresolved
                 || exist_stub_call_targets)
            in
            if should_pass_through then
              let () =
                log
                  "Passing through arguments due to `is_decorated_target`: %b, \
                   `exist_stub_init_targets`: %b, `exist_stub_call_targets`: %b, `unresolved`: %a"
                  is_decorated_target
                  exist_stub_init_targets
                  exist_stub_call_targets
                  Unresolved.pp
                  unresolved
              in
              Algorithms.fold_balanced
                ~f:CallTarget.Set.join
                ~init:CallTarget.Set.bottom
                argument_callees
            else
              CallTarget.Set.bottom
          in
          CallTarget.Set.join pass_through_arguments returned_callables_from_call, state)


    and analyze_comprehension_generators ~pyrefly_in_context ~state generators =
      let add_binding state ({ Comprehension.Generator.conditions; _ } as generator) =
        let { Assign.target; value; _ } =
          preprocess_generator ~pyrefly_in_context ~callable:Context.callable generator
        in
        let state =
          match value with
          | Some value -> analyze_expression ~pyrefly_in_context ~state ~expression:value |> snd
          | None -> state
        in
        (* TODO: assign value to target *)
        let _ = target in
        (* Analyzing the conditions might have side effects. *)
        let analyze_condition state condiiton =
          analyze_expression ~pyrefly_in_context ~state ~expression:condiiton |> snd
        in
        List.fold conditions ~init:state ~f:analyze_condition
      in
      List.fold ~f:add_binding generators ~init:state


    and analyze_dictionary_comprehension
        ~pyrefly_in_context
        ~state
        { Comprehension.element = Dictionary.Entry.KeyValue.{ key; value }; generators; _ }
      =
      let state = analyze_comprehension_generators ~pyrefly_in_context ~state generators in
      let _, state = analyze_expression ~pyrefly_in_context ~state ~expression:value in
      let _, state = analyze_expression ~pyrefly_in_context ~state ~expression:key in
      CallTarget.Set.bottom, state


    and analyze_comprehension ~pyrefly_in_context ~state { Comprehension.element; generators; _ } =
      let bound_state = analyze_comprehension_generators ~pyrefly_in_context ~state generators in
      let _, state =
        analyze_expression ~pyrefly_in_context ~state:bound_state ~expression:element
      in
      CallTarget.Set.bottom, state


    (* Return possible callees and the new state. *)
    and analyze_expression
        ~pyrefly_in_context
        ~state
        ~expression:({ Node.value; location } as expression)
      =
      log
        "Analyzing expression `%a` at `%a` with state `%a`"
        Expression.pp_expression
        expression.Node.value
        Location.pp
        location
        State.pp
        state;
      let analyze_expression_inner () =
        match value with
        | Expression.Await { Await.operand = expression; origin = _ } ->
            analyze_expression ~pyrefly_in_context ~state ~expression
        | BooleanOperator { left; right; _ } ->
            let _, state = analyze_expression ~pyrefly_in_context ~state ~expression:left in
            let _, state = analyze_expression ~pyrefly_in_context ~state ~expression:right in
            CallTarget.Set.bottom, state
        | ComparisonOperator { left; operator = _; right; origin = _ } ->
            let _, state = analyze_expression ~pyrefly_in_context ~state ~expression:left in
            let _, state = analyze_expression ~pyrefly_in_context ~state ~expression:right in
            CallTarget.Set.bottom, state
        | Call ({ callee = _; arguments; origin = _ } as call) ->
            analyze_call ~pyrefly_in_context ~location ~call ~arguments ~state
        | Constant _ -> CallTarget.Set.bottom, state
        | Dictionary entries ->
            let analyze_dictionary_entry state = function
              | Dictionary.Entry.KeyValue { key; value } ->
                  let _, state = analyze_expression ~pyrefly_in_context ~state ~expression:key in
                  let _, state = analyze_expression ~pyrefly_in_context ~state ~expression:value in
                  state
              | Splat s -> analyze_expression ~pyrefly_in_context ~state ~expression:s |> snd
            in
            let state = List.fold entries ~f:analyze_dictionary_entry ~init:state in
            CallTarget.Set.bottom, state
        | DictionaryComprehension comprehension ->
            analyze_dictionary_comprehension ~pyrefly_in_context ~state comprehension
        | Generator comprehension -> analyze_comprehension ~pyrefly_in_context ~state comprehension
        | Lambda { parameters; body } ->
            let state =
              List.fold
                parameters
                ~init:state
                ~f:(fun state { Node.value = { Parameter.value; _ }; _ } ->
                  match value with
                  | Some default_value ->
                      analyze_expression ~pyrefly_in_context ~state ~expression:default_value |> snd
                  | None -> state)
            in
            let _, state = analyze_expression ~pyrefly_in_context ~state ~expression:body in
            CallTarget.Set.bottom, state
        | List list ->
            let analyze_list_element state expression =
              analyze_expression ~pyrefly_in_context ~state ~expression |> snd
            in
            let state = List.fold list ~f:analyze_list_element ~init:state in
            CallTarget.Set.bottom, state
        | ListComprehension comprehension ->
            analyze_comprehension ~pyrefly_in_context ~state comprehension
        | Set set ->
            let analyze_set_element state expression =
              analyze_expression ~pyrefly_in_context ~state ~expression |> snd
            in
            let state = List.fold ~f:analyze_set_element set ~init:state in
            CallTarget.Set.bottom, state
        | SetComprehension comprehension ->
            analyze_comprehension ~pyrefly_in_context ~state comprehension
        | Name (Name.Identifier identifier) ->
            let global_callables =
              Context.input_define_call_graph
              |> DefineCallGraph.resolve_identifier ~location ~identifier
              >>| (fun ({
                          IdentifierCallees.if_called =
                            {
                              CallCallees.call_targets = original_call_targets;
                              init_targets = original_init_targets;
                              (* TODO(T243083593): Resolve decorated targets for __new__ *)
                              new_targets = _;
                              _;
                            } as if_called;
                          _;
                        } as identifier_callees) ->
                    let {
                      AnalyzeDecoratedTargetsResult.decorated_targets = decorated_call_targets;
                      non_decorated_targets = non_decorated_call_targets;
                      result_targets = result_call_targets;
                    }
                      =
                      resolve_decorated_targets original_call_targets
                    in
                    let {
                      AnalyzeDecoratedTargetsResult.decorated_targets = decorated_init_targets;
                      non_decorated_targets = non_decorated_init_targets;
                      result_targets = _;
                    }
                      =
                      resolve_decorated_targets original_init_targets
                    in
                    Context.output_define_call_graph :=
                      DefineCallGraph.set_identifier_callees
                        ~error_if_new:true
                        ~identifier
                        ~location
                        ~identifier_callees:
                          {
                            identifier_callees with
                            if_called =
                              {
                                if_called with
                                CallGraph.CallCallees.call_targets = non_decorated_call_targets;
                                init_targets = non_decorated_init_targets;
                                decorated_targets =
                                  decorated_call_targets
                                  |> List.rev_append decorated_init_targets
                                  |> List.dedup_and_sort ~compare:CallTarget.compare;
                              };
                          }
                        !Context.output_define_call_graph;
                    CallTarget.Set.of_list result_call_targets)
              |> Option.value ~default:CallTarget.Set.bottom
            in
            let callables_from_variable =
              State.get
                (PyreflyApi.InContext.root_of_identifier pyrefly_in_context ~location ~identifier)
                state
            in
            ( join_original_targets_with_inferred
                ~original:global_callables
                ~inferred:callables_from_variable,
              state )
        | Name (Name.Attribute ({ Name.Attribute.base; _ } as attribute_access)) ->
            let _, state = analyze_expression ~pyrefly_in_context ~state ~expression:base in
            let callables =
              Context.input_define_call_graph
              |> DefineCallGraph.resolve_attribute_access ~location ~attribute_access
              >>| (fun ({
                          AttributeAccessCallees.property_targets;
                          if_called =
                            {
                              CallCallees.call_targets = original_call_targets;
                              init_targets = original_init_targets;
                              (* TODO(T243083593): Resolve decorated targets for __new__ *)
                              new_targets = _;
                              _;
                            } as if_called;
                          is_attribute =
                            _
                            (* This is irrelevant. Regardless of whether this could potentially be
                               an attribute access, we still need to treat `property_targets` in the
                               same way as `callable_targets`. *);
                          _;
                        } as attribute_access_callees) ->
                    let {
                      AnalyzeDecoratedTargetsResult.decorated_targets = decorated_call_targets;
                      non_decorated_targets = non_decorated_call_targets;
                      result_targets = result_call_targets;
                    }
                      =
                      resolve_decorated_targets original_call_targets
                    in
                    let {
                      AnalyzeDecoratedTargetsResult.decorated_targets = decorated_init_targets;
                      non_decorated_targets = non_decorated_init_targets;
                      result_targets = _;
                    }
                      =
                      resolve_decorated_targets original_init_targets
                    in
                    let {
                      AnalyzeDecoratedTargetsResult.decorated_targets = decorated_property_targets;
                      non_decorated_targets = _;
                      result_targets = result_property_targets;
                    }
                      =
                      (* Since properties can be decorated, we need to get the "inlined"
                         properties. *)
                      resolve_decorated_targets property_targets
                    in
                    Context.output_define_call_graph :=
                      DefineCallGraph.set_attribute_access_callees
                        ~error_if_new:false (* empty attribute accesses are stripped *)
                        ~location
                        ~attribute_access
                        ~callees:
                          {
                            attribute_access_callees with
                            property_targets = result_property_targets;
                            if_called =
                              {
                                if_called with
                                CallGraph.CallCallees.call_targets = non_decorated_call_targets;
                                init_targets = non_decorated_init_targets;
                                decorated_targets =
                                  decorated_property_targets
                                  |> List.rev_append decorated_call_targets
                                  |> List.rev_append decorated_init_targets
                                  |> List.dedup_and_sort ~compare:CallTarget.compare;
                              };
                          }
                        !Context.output_define_call_graph;
                    (* TODO(T222400916): We need to simulate the call to the property targets (by
                       calling `analyze_callee_targets`), which can return callables. *)
                    (* We should NOT return the property targets here. If method `A.foo` is a
                       property, then accessing the property `A().foo` means calling the getter, but
                       the result of the access is not the getter itself. *)
                    CallTarget.Set.of_list result_call_targets)
              |> Option.value ~default:CallTarget.Set.bottom
            in
            callables, state
        | Starred (Starred.Once expression)
        | Starred (Starred.Twice expression) ->
            let _, state = analyze_expression ~pyrefly_in_context ~state ~expression in
            CallTarget.Set.bottom, state
        | FormatString substrings ->
            let analyze_substring state = function
              | Substring.Literal _ -> state
              | Substring.Format { value; format_spec } ->
                  (* TODO: redirect decorators in the stringify target *)
                  let _, state = analyze_expression ~pyrefly_in_context ~state ~expression:value in
                  let state =
                    match format_spec with
                    | Some format_spec ->
                        analyze_expression ~pyrefly_in_context ~state ~expression:format_spec |> snd
                    | None -> state
                  in
                  state
            in
            let state = List.fold substrings ~init:state ~f:analyze_substring in
            CallTarget.Set.bottom, state
        | Ternary { target; test; alternative } ->
            let _, state = analyze_expression ~pyrefly_in_context ~state ~expression:test in
            let value_then, state_then =
              analyze_expression ~pyrefly_in_context ~state ~expression:target
            in
            let value_else, state_else =
              analyze_expression ~pyrefly_in_context ~state ~expression:alternative
            in
            CallTarget.Set.join value_then value_else, join state_then state_else
        | Tuple expressions ->
            let analyze_tuple_element state expression =
              analyze_expression ~pyrefly_in_context ~state ~expression |> snd
            in
            let state = List.fold ~f:analyze_tuple_element ~init:state expressions in
            CallTarget.Set.bottom, state
        | UnaryOperator { operand; _ } ->
            let _, state = analyze_expression ~pyrefly_in_context ~state ~expression:operand in
            CallTarget.Set.bottom, state
        | WalrusOperator { target = _; value; origin = _ } ->
            analyze_expression ~pyrefly_in_context ~state ~expression:value
        | Yield None -> CallTarget.Set.bottom, state
        | Yield (Some expression)
        | YieldFrom expression ->
            let callees, state = analyze_expression ~pyrefly_in_context ~state ~expression in
            callees, store_callees ~weak:true ~root:AccessPath.Root.LocalResult ~callees state
        | Slice _ ->
            failwith "Slice nodes should always be rewritten by `CallGraph.redirect_expressions`"
        | Subscript _ ->
            failwith
              "Subscripts nodes should always be rewritten by `CallGraph.redirect_expressions`"
        | BinaryOperator _ ->
            failwith
              "BinaryOperator nodes should always be rewritten by `CallGraph.redirect_expressions`"
      in
      let call_targets, state =
        CallGraphProfiler.track_expression_analysis
          ~profiler:Context.profiler
          ~analysis:Forward
          ~expression
          ~f:analyze_expression_inner
      in
      log
        "Finished analyzing expression `%a`: `%a`"
        Expression.pp
        expression
        CallTarget.Set.pp
        call_targets;
      call_targets, state


    let analyze_parameter_default_value ~pyrefly_in_context ~parameter_name ~state = function
      | None -> state
      | Some default_value ->
          let default_value =
            preprocess_parameter_default_value
              ~pyrefly_in_context
              ~callable:Context.callable
              default_value
          in
          let callees, state =
            analyze_expression ~pyrefly_in_context ~state ~expression:default_value
          in
          let root = AccessPath.Root.Variable parameter_name in
          store_callees ~weak:true ~root ~callees state


    let analyze_statement ~pyrefly_in_context ~state ~statement =
      log "Analyzing statement `%a` with state `%a`" Statement.pp statement State.pp state;
      let state =
        let statement =
          preprocess_statement ~pyrefly_in_context ~callable:Context.callable statement
        in
        match Node.value statement with
        | Statement.Assign { Assign.target; value = Some value; _ } -> (
            let callees, state = analyze_expression ~pyrefly_in_context ~state ~expression:value in
            let _target_callees, state =
              analyze_expression ~pyrefly_in_context ~state ~expression:target
            in
            match
              PyreflyApi.InContext.access_path_of_expression
                pyrefly_in_context
                ~self_variable
                target
            with
            | None -> state
            | Some { root; path } ->
                if AccessPath.Path.is_empty path then
                  store_callees ~weak:false ~root ~callees state
                else
                  (* Attribute assignments like `self.x = foo` should not propagate callees to the
                     root variable. The callable is stored on an attribute, not on the variable
                     itself. *)
                  (* TODO(T270919728): Handle attribute assignments using an abstract tree domain,
                     similar to the taint tree. *)
                  state)
        | Assign { Assign.target; value = None; _ } -> (
            let _target_callees, state =
              analyze_expression ~pyrefly_in_context ~state ~expression:target
            in
            match
              PyreflyApi.InContext.access_path_of_expression
                pyrefly_in_context
                ~self_variable
                target
            with
            | None -> state
            | Some { root; path } ->
                let strong_update = AccessPath.Path.is_empty path in
                store_callees ~weak:(not strong_update) ~root ~callees:CallTarget.Set.bottom state)
        | Assert { test; _ } ->
            analyze_expression ~pyrefly_in_context ~state ~expression:test |> snd
        | Define ({ Define.signature = { name; _ }; _ } as define) ->
            let define_location =
              Define.location_with_decorators
                { Node.location = Node.location statement; value = define }
            in
            let callees_without_captures, captures =
              Context.input_define_call_graph
              |> DefineCallGraph.resolve_define ~define_location
              >>| (fun { DefineCallees.define_targets; _ } ->
                    let {
                      AnalyzeDecoratedTargetsResult.decorated_targets;
                      non_decorated_targets = _ (* Not useful to taint analysis. *);
                      result_targets;
                    }
                      =
                      resolve_decorated_targets define_targets
                    in
                    let () =
                      Context.output_define_call_graph :=
                        DefineCallGraph.set_define_callees
                          ~error_if_new:true
                          ~define_location
                          ~callees:
                            { DefineCallees.define_targets = result_targets; decorated_targets }
                          !Context.output_define_call_graph
                    in
                    let captures =
                      match define_targets with
                      | [define_target] ->
                          define_target
                          |> CallTarget.target
                          |> (* Since `Define` statements inside another `Define` are stripped out
                                (to avoid bloat), use this API to query the definition. *)
                          CallablesSharedMemory.ReadOnly.get_captures
                            Context.callables_to_definitions_map
                      | _ ->
                          Format.asprintf
                            "Expect a single `define_target` but got `[%s]`"
                            (define_targets |> List.map ~f:CallTarget.show |> String.concat ~sep:";")
                          |> failwith
                    in
                    result_targets, captures)
              |> Option.value ~default:([], None)
            in
            let callees =
              match captures with
              | Some captures ->
                  let parameters_roots, parameters_targets =
                    captures
                    |> List.filter_map ~f:(fun captured_variable ->
                           let variable =
                             PyreflyApi.InContext.propagate_captured_variable
                               pyrefly_in_context
                               captured_variable
                           in
                           log "Inner function captures `%a`" AccessPath.Root.pp variable;
                           let parameter_targets =
                             state
                             |> State.get variable
                             |> CallTarget.Set.elements
                             |> List.map ~f:(fun { CallTarget.target; implicit_receiver; _ } ->
                                    { Target.ParameterValue.target; implicit_receiver })
                           in
                           (* Sometimes a captured variable does not have a record in `state`, but
                              we still want to create a callee with the captured variables that have
                              records in `state`. *)
                           if List.is_empty parameter_targets then
                             None
                           else
                             let root = AccessPath.Root.CapturedVariable captured_variable in
                             Some (root, parameter_targets))
                    |> List.unzip
                  in
                  if List.is_empty parameters_targets then
                    callees_without_captures
                  else
                    parameters_targets
                    |> cartesian_product_with_limit
                         ~limit:Context.maximum_parameterized_targets_when_analyzing_define
                         ~message_when_exceeding_limit:
                           "Avoid generating parameterized targets when analyzing `Define` \
                            statement"
                    |> Option.value ~default:[]
                    |> List.concat_map ~f:(fun parameters_targets ->
                           List.map callees_without_captures ~f:(fun call_target ->
                               match
                                 validate_target
                                   (Target.Parameterized
                                      {
                                        regular =
                                          call_target |> CallTarget.target |> Target.get_regular;
                                        parameters =
                                          parameters_targets
                                          |> List.zip_exn parameters_roots
                                          |> Target.ParameterMap.of_alist_exn;
                                      })
                               with
                               | Some parameterized -> { call_target with target = parameterized }
                               | None -> call_target))
              | None -> callees_without_captures
            in
            store_callees
              ~weak:false
              ~root:(AccessPath.Root.Variable (Reference.show name))
              ~callees:(CallTarget.Set.of_list callees)
              state
        | Delete expressions ->
            let analyze_delete state expression =
              analyze_expression ~pyrefly_in_context ~state ~expression |> snd
            in
            List.fold ~f:analyze_delete ~init:state expressions
        | Expression expression ->
            analyze_expression ~pyrefly_in_context ~state ~expression |> Core.snd
        | Global _
        | Import _
        | Nonlocal _
        | Pass
        | Raise { expression = None; _ } ->
            state
        | Raise { expression = Some expression; _ } ->
            analyze_expression ~pyrefly_in_context ~state ~expression |> snd
        | Return { expression = Some expression; _ } ->
            (* No need to propagate `ReturnShimCallees`, since the taint analysis only need to
               analyze them once. TODO(T231956685): Resolve decorated targets. *)
            let callees, state = analyze_expression ~pyrefly_in_context ~state ~expression in
            let () =
              Context.input_define_call_graph
              |> DefineCallGraph.resolve_return ~statement_location:statement.Node.location
              >>| (fun callees ->
                    Context.output_define_call_graph :=
                      DefineCallGraph.add_return_callees
                        ~debug:Context.debug
                        ~caller:Context.callable
                        ~on_existing_callees:DefineCallGraph.OnExistingCallees.WarnThenJoin
                        ~return_expression:expression
                        ~statement_location:statement.Node.location
                        ~callees
                        !Context.output_define_call_graph)
              |> Option.value ~default:()
            in
            store_callees ~weak:true ~root:AccessPath.Root.LocalResult ~callees state
        | Return { expression = None; _ }
        | Try _ ->
            (* Try statements are lowered down in `Cfg.create`, but they are preserved in the final
               Cfg. They should be ignored. *)
            state
        | Break
        | Class _
        | Continue
        | TypeAlias _ ->
            state
        | For _
        | If _
        | Match _
        | With _
        | While _ ->
            failwith "For/If/Match/With/While nodes should always be rewritten by `Cfg.create`"
        | AugmentedAssign _ ->
            failwith
              "AugmentedAssign nodes should always be rewritten by `CallGraph.preprocess_statement`"
      in
      log "Finished analyzing statement `%a`: `%a`" Statement.pp statement State.pp state;
      state


    let forward ~statement_key state ~statement =
      CallGraphProfiler.track_statement_analysis
        ~profiler:Context.profiler
        ~analysis:Forward
        ~statement
        ~f:(fun () ->
          let pyrefly_in_context =
            PyreflyApi.InContext.create_at_statement_scope
              Context.pyrefly_api
              ~module_qualifier:Context.module_qualifier
              ~define_name:Context.define_name
              ~call_graph:Context.input_define_call_graph
              ~statement_key
          in
          analyze_statement ~pyrefly_in_context ~state ~statement)


    let backward ~statement_key:_ _ ~statement:_ = failwith "unused"
  end
end

let higher_order_call_graph_of_define
    ~define_call_graph
    ~pyrefly_api
    ~callables_to_definitions_map
    ~skip_analysis_targets
    ~called_when_parameter
    ~skip_inlining_higher_order_functions
    ~qualifier
    ~callable
    ~define
    ~initial_state
    ~get_callee_model
    ~profiler
    ~maximum_target_depth
    ~maximum_parameterized_targets_at_call_site
  =
  let module Context = struct
    let input_define_call_graph = define_call_graph

    let output_define_call_graph = ref (DefineCallGraph.copy define_call_graph)

    let pyrefly_api = pyrefly_api

    let get_callee_model = get_callee_model

    let debug = PysaDump.should_dump_higher_order_call_graph ~define:(Node.value define) ~callable

    let module_qualifier = qualifier

    let define = define

    let define_name = Target.define_name_exn callable

    let callable = callable

    let callables_to_definitions_map = callables_to_definitions_map

    let skip_analysis_targets = skip_analysis_targets

    let called_when_parameter = called_when_parameter

    let skip_inlining_higher_order_functions = skip_inlining_higher_order_functions

    let profiler = profiler

    let maximum_target_depth = maximum_target_depth

    let maximum_parameterized_targets_at_call_site = maximum_parameterized_targets_at_call_site

    let maximum_parameterized_targets_when_analyzing_define =
      maximum_parameterized_targets_at_call_site
  end
  in
  log
    ~debug:Context.debug
    "Building higher order call graph of `%a` with initial state `%a`. Define call graph: `%a`"
    Target.pp_external
    callable
    HigherOrderCallGraph.State.pp
    initial_state
    DefineCallGraph.pp
    define_call_graph;
  let module TransferFunction = HigherOrderCallGraph.MakeTransferFunction (Context) in
  let module Fixpoint = PyrePysaLogic.Fixpoint.Make (TransferFunction) in
  (* Handle parameters. *)
  let initial_state =
    let pyrefly_in_context =
      PyreflyApi.InContext.create_at_function_scope
        pyrefly_api
        ~module_qualifier:qualifier
        ~define_name:(Target.define_name_exn callable)
        ~call_graph:Context.input_define_call_graph
    in
    List.fold
      define.Ast.Node.value.Ast.Statement.Define.signature.parameters
      ~init:initial_state
      ~f:(fun state { Node.value = { Parameter.name; value = default_value; _ }; _ } ->
        TransferFunction.analyze_parameter_default_value
          ~pyrefly_in_context
          ~state
          ~parameter_name:name
          default_value)
  in
  let returned_callables =
    let cfg = PyrePysaLogic.Cfg.create ~normalize_asserts:false (Node.value define) in
    Fixpoint.forward ~cfg ~initial:initial_state
    |> Fixpoint.exit
    >>| TransferFunction.get_returned_callables
    |> Option.value ~default:CallTarget.Set.bottom
  in
  let call_indexer = CallGraph.Indexer.create () in
  let higher_order_call_graph =
    {
      HigherOrderCallGraph.returned_callables;
      call_graph =
        !Context.output_define_call_graph
        |> DefineCallGraph.filter_empty_attribute_access
        |> DefineCallGraph.dedup_and_sort
        |> DefineCallGraph.regenerate_call_indices ~indexer:call_indexer;
    }
  in
  log
    ~debug:Context.debug
    "Built higher order call graph of `%a`: `%a`"
    Target.pp_external
    callable
    HigherOrderCallGraph.pp
    higher_order_call_graph;
  higher_order_call_graph


let default_scheduler_policy =
  Scheduler.Policy.fixed_chunk_size
    ~minimum_chunks_per_worker:1
    ~minimum_chunk_size:2
    ~preferred_chunk_size:5000
    ()


let build_whole_program_call_graph
    ~scheduler
    ~static_analysis_configuration:
      ({ Configuration.StaticAnalysis.scheduler_policies; _ } as static_analysis_configuration)
    ~pyrefly_api
    ~resolve_module_path
    ~callables_to_definitions_map
    ~callables_to_decorators_map
    ~global_constants
    ~override_graph
    ~store_shared_memory
    ~attribute_targets
    ~skip_analysis_targets
    ~skip_call_higher_order_functions
    ~definitions
    ~create_dependency_for
  =
  let find_missing_flows =
    static_analysis_configuration.Configuration.StaticAnalysis.find_missing_flows
  in
  let transform_redirected_call_graph decorated_target call_graph =
    (* For call graph of decorated targets, add a call graph edge for the decorated function itself,
       in the return expression `decorator1(decorator2(original_function))` *)
    let original_callable = Target.set_kind Target.Normal decorated_target in
    let {
      CallableToDecoratorsMap.DecoratedDefineBody.original_function_name;
      original_function_name_location;
      _;
    }
      =
      CallableToDecoratorsMap.SharedMemory.decorated_callable_body
        callables_to_decorators_map
        original_callable
      |> Option.value_exn ~message:"Unexpected decorated target without a decorated body"
    in
    let original_function_name =
      match original_function_name with
      | Name.Identifier name -> name
      | original_function_name ->
          Format.asprintf
            "Expect the decorated callable to be an identifier, but got `%a`"
            Name.pp
            original_function_name
          |> failwith
    in
    let {
      PyreflyApi.CallableMetadata.is_staticmethod = is_static_method;
      is_classmethod = is_class_method;
      _;
    }
      =
      PyreflyApi.ReadOnly.get_callable_metadata
        pyrefly_api
        (Target.define_name_exn original_callable)
    in
    DefineCallGraph.set_identifier_callees
      ~error_if_new:false
      ~location:original_function_name_location
      ~identifier:original_function_name
      ~identifier_callees:
        (IdentifierCallees.create
           ~if_called:
             (CallCallees.create
                ~call_targets:
                  [
                    CallTarget.create
                      ~implicit_receiver:false
                      ~is_class_method
                      ~is_static_method
                      original_callable;
                  ]
                ())
           ())
      call_graph
  in
  let add_targets ~debug ~define_and_qualifier callable call_graph =
    match define_and_qualifier with
    | Some
        {
          CallablesSharedMemory.DefineAndQualifier.define =
            { Node.location = define_location; value = define };
          qualifier = _;
        } ->
        let allow_modifier = function
          | PyreflyApi.TypeModifier.Optional
          | PyreflyApi.TypeModifier.Coroutine
          | PyreflyApi.TypeModifier.Awaitable
          | PyreflyApi.TypeModifier.ReadOnly
          | PyreflyApi.TypeModifier.TypeVariableBound
          | PyreflyApi.TypeModifier.TypeVariableConstraint ->
              true
          | PyreflyApi.TypeModifier.Type -> false
        in
        let is_class_instance modifiers = List.for_all ~f:allow_modifier modifiers in
        let is_class_type modifiers =
          match List.rev modifiers with
          | PyreflyApi.TypeModifier.Type :: rest -> List.for_all ~f:allow_modifier rest
          | _ -> false
        in
        let add_attribute_accesses
            ~location:attribute_access_location
            ~attribute_access:
              ({ Ast.Expression.Name.Attribute.base; attribute; _ } as attribute_access)
            call_graph
          =
          let targets =
            PyreflyApi.ReadOnly.get_type_of_expression
              pyrefly_api
              ~define_name:(Target.define_name_exn callable)
              ~location:(Ast.Node.location base)
            >>| PyreflyApi.ReadOnly.Type.get_class_names pyrefly_api
            >>| (fun { PyreflyApi.ClassNamesFromType.classes; _ } -> classes)
            >>| List.map ~f:(fun { PyreflyApi.ClassWithModifiers.modifiers; class_name } ->
                    let parents =
                      class_name :: PyreflyApi.ReadOnly.class_mro pyrefly_api class_name
                    in
                    if is_class_instance modifiers then
                      List.map
                        ~f:(fun class_name -> Format.sprintf "%s.%s" class_name attribute)
                        parents
                    else if is_class_type modifiers then
                      List.map
                        ~f:(fun class_name -> Format.sprintf "%s.__class__.%s" class_name attribute)
                        parents
                    else
                      [])
            >>| List.concat
            >>| List.map ~f:Reference.create
            >>| List.map ~f:Target.create_object
            >>| List.filter ~f:(fun target -> Target.Set.mem target attribute_targets)
            >>| List.map ~f:CallGraph.CallTarget.create
            |> Option.value ~default:[]
          in
          if not (List.is_empty targets) then
            DefineCallGraph.add_attribute_access_callees
              ~debug:false
              ~caller:callable
              ~on_existing_callees:DefineCallGraph.OnExistingCallees.Join
              ~location:attribute_access_location
              ~attribute_access
              ~callees:(AttributeAccessCallees.create ~global_targets:targets ~is_attribute:true ())
              call_graph
          else
            call_graph
        in
        let add_shim_target ~debug ~expression_location ~call ~arguments call_graph =
          let to_special_call_target ~kind call_target =
            let target = CallTarget.target call_target in
            let receiver_class = call_target.CallTarget.receiver_class in
            match kind with
            | `Regular -> SpecialCallResolution.CallTarget.Regular { target; receiver_class }
            | `Init -> SpecialCallResolution.CallTarget.Init { target; receiver_class }
            | `New -> SpecialCallResolution.CallTarget.New { target; receiver_class }
            | `Property -> SpecialCallResolution.CallTarget.Property { target; receiver_class }
          in
          let fetch_special_call_targets call_callees =
            List.map
              ~f:(to_special_call_target ~kind:`Regular)
              call_callees.CallCallees.call_targets
            |> List.rev_append
                 (List.map
                    ~f:(to_special_call_target ~kind:`Init)
                    call_callees.CallCallees.init_targets)
            |> List.rev_append
                 (List.map
                    ~f:(to_special_call_target ~kind:`New)
                    call_callees.CallCallees.new_targets)
          in
          DefineCallGraph.resolve_call ~location:expression_location ~call call_graph
          >>= fun original_call_callees ->
          let { Node.value = callee_expression; location = callee_location } = call.Call.callee in
          let nested_callees =
            match callee_expression with
            | Expression.Call nested_call ->
                let callees =
                  DefineCallGraph.resolve_call
                    ~location:callee_location
                    ~call:nested_call
                    call_graph
                  >>| fetch_special_call_targets
                in
                SpecialCallResolution.NestedCallees.NestedCall (Option.value ~default:[] callees)
            | Expression.Name
                (Name.Attribute
                  {
                    Name.Attribute.base =
                      { Node.value = Expression.Name (Name.Attribute attribute_access); location };
                    _;
                  }) ->
                let callees =
                  DefineCallGraph.resolve_attribute_access ~attribute_access ~location call_graph
                  >>| fun { AttributeAccessCallees.if_called; property_targets; _ } ->
                  fetch_special_call_targets if_called
                  |> List.rev_append
                       (List.map ~f:(to_special_call_target ~kind:`Property) property_targets)
                in
                SpecialCallResolution.NestedCallees.NestedAttributeAccess
                  (Option.value ~default:[] callees)
            | _ -> SpecialCallResolution.NestedCallees.None
          in
          let () =
            log
              ~debug
              "Shimming call: `%a`. Original callees: `%a`"
              Call.pp
              call
              CallCallees.pp
              original_call_callees
          in
          shim_for_call
            ~pyrefly_api
            ~callables_to_definitions_map
            ~callees:(fetch_special_call_targets original_call_callees)
            ~nested_callees
            ~arguments
          >>= fun ({
                     Shims.ShimArgumentMapping.callee = shim_target_callee;
                     discard_higher_order_parameters;
                     _;
                   } as shim) ->
          create_shim_callee_expression ~debug ~callable ~location:expression_location ~call shim
          >>= fun ({ Node.value = shim_callee; location = shim_callee_location } as
                  shim_callee_expression) ->
          let () =
            log
              ~debug
              "Shimmed callee: `%a` at `%a`"
              Expression.pp_expression
              shim_callee
              Location.pp
              shim_callee_location
          in
          let set_shim_target ~call_targets call_graph =
            if List.is_empty call_targets then
              let () =
                log
                  ~debug
                  "Failed to resolve callees for shimmed callee %a"
                  Expression.pp
                  shim_callee_expression
              in
              call_graph
            else
              let shim_target =
                Some { ShimTarget.call_targets; decorated_targets = []; argument_mapping = shim }
              in
              DefineCallGraph.set_call_callees
                ~error_if_new:false
                ~location:expression_location
                ~call
                ~callees:
                  {
                    original_call_callees with
                    CallCallees.shim_target;
                    higher_order_parameters =
                      (if discard_higher_order_parameters then
                         (* Otherwise we may end up with false positives due to analyzing
                            `higher_order_parameters`. *)
                         HigherOrderParameterMap.empty
                      else
                        original_call_callees.CallCallees.higher_order_parameters);
                  }
                call_graph
          in
          match shim_callee, shim_target_callee, nested_callees with
          | _, Shims.ShimArgumentMapping.Target.StaticMethod { class_name; method_name }, _ ->
              (* We just want to call this method, regardless of whether it exists. *)
              Some
                (set_shim_target
                   ~call_targets:[CallTarget.create (Target.create_method class_name method_name)]
                   call_graph)
          | ( Expression.Name (Name.Attribute { Name.Attribute.attribute; _ }),
              Shims.ShimArgumentMapping.Target.AppendAttribute
                {
                  inner =
                    Shims.ShimArgumentMapping.Target.GetAttributeBase
                      {
                        inner =
                          Shims.ShimArgumentMapping.Target.GetAttributeBase
                            { inner = Shims.ShimArgumentMapping.Target.Callee; _ };
                        _;
                      };
                  attribute = append_attribute;
                },
              SpecialCallResolution.NestedCallees.NestedAttributeAccess nested_callees )
            when String.equal attribute append_attribute ->
              (* This case is for `PromoteQueue`. *)
              let call_targets =
                nested_callees
                |> List.filter_map ~f:(fun call_target ->
                       match SpecialCallResolution.CallTarget.target call_target with
                       | Target.Regular (Method { class_name; _ }) ->
                           (* Given call `x.y.original_attribute(...)`, we want to resolve callees
                              on the made-up call `x.new_attribute(...)`. Here we fetch callees on
                              `x.y` and then replace `y` with `new_attribute`. *)
                           Some (Target.create_method (Reference.create class_name) attribute)
                       | _ -> None)
                |> List.map ~f:(CallTarget.create ~implicit_receiver:true)
              in
              Some (set_shim_target ~call_targets call_graph)
          | Expression.Name (Name.Identifier identifier), _, _ ->
              DefineCallGraph.resolve_identifier
                ~location:shim_callee_location
                ~identifier
                call_graph
              >>| fun { IdentifierCallees.if_called = { CallCallees.call_targets; _ }; _ } ->
              set_shim_target ~call_targets call_graph
          | Expression.Name (Name.Attribute attribute_access), _, _ ->
              DefineCallGraph.resolve_attribute_access
                ~location:shim_callee_location
                ~attribute_access
                call_graph
              >>| fun { AttributeAccessCallees.if_called = { CallCallees.call_targets; _ }; _ } ->
              set_shim_target ~call_targets call_graph
          | shim_callee, _, _ ->
              (* If `shim_callee` does not refer to an existing AST node (i.e., it is a made-up
                 one), then we give up adding a shim target. Otherwise we fetch callees on them and
                 use those as shim targets -- see the above cases. *)
              let () =
                log
                  ~debug
                  "Unknown shim callee (probably a made-up AST node): `%s`"
                  (Expression.show_expression shim_callee)
              in
              Some call_graph
        in
        let add_try_handler_targets ~try_:{ Try.handlers; _ } call_graph =
          let add_try_handler_targets call_graph { Try.Handler.kind; _ } =
            match kind with
            | Some { Node.location; _ } ->
                DefineCallGraph.add_callees
                  ~debug:false
                  ~caller:callable
                  ~on_existing_callees:DefineCallGraph.OnExistingCallees.Fail
                  ~expression_for_logging:None
                  ~expression_identifier:
                    (ExpressionIdentifier.ArtificialCall
                       (Origin.create ~location Origin.TryHandlerIsInstance))
                  ~callees:
                    (ExpressionCallees.Call
                       (CallCallees.create
                          ~call_targets:
                            [
                              CallTarget.create
                                (Target.create_function (Reference.create "builtins.isinstance"));
                            ]
                          ()))
                  call_graph
            | _ -> call_graph
          in
          List.fold ~f:add_try_handler_targets ~init:call_graph handlers
        in
        let add_match_targets ~match_:{ Match.subject; cases; _ } call_graph =
          (* The control flow graph building logic translates the `match` statement into a series of
             if/then/else. Each condition introduced might have artificial calls, which needs to
             have a call graph edge. For now, we just mark those as unresolved (hence obscure).
             TODO(T251614103): Properly resolve artificial calls in match conditions. *)
          let add_unresolved_callee ~location ~call call_graph =
            DefineCallGraph.set_call_callees
              ~error_if_new:false
              ~location
              ~call
              ~callees:
                (CallCallees.create
                   ~unresolved:(Unresolved.True Unresolved.SkippedMatchCondition)
                   ())
              call_graph
          in
          let add_case_targets call_graph case =
            let test = Analysis.Cfg.MatchTranslate.to_condition ~subject ~case in
            let fold_binary_operator
                ~folder
                ~state:call_graph
                ~location
                ({ BinaryOperator.left; _ } as binary_operator)
              =
              let ({ Call.origin; _ } as call) =
                BinaryOperator.lower_to_call
                  ~location
                  ~callee_location:left.Node.location
                  binary_operator
              in
              let call_graph =
                match origin with
                | Some origin when Origin.is_from_match origin ->
                    add_unresolved_callee ~location ~call call_graph
                | _ -> call_graph
              in
              Folder.default_fold_binary_operator ~folder ~state:call_graph binary_operator
            in
            let fold_comparison_operator
                ~folder
                ~state:call_graph
                ~location
                ({ ComparisonOperator.left; _ } as comparison_operator)
              =
              let call_graph =
                match
                  ComparisonOperator.lower_to_expression
                    ~location
                    ~callee_location:left.location
                    comparison_operator
                with
                | Some
                    { Node.value = Expression.Call ({ Call.origin = Some origin; _ } as call); _ }
                  when Origin.is_from_match origin ->
                    add_unresolved_callee ~location ~call call_graph
                | _ -> call_graph
              in
              Folder.default_fold_comparison_operator ~folder ~state:call_graph comparison_operator
            in
            let fold_subscript
                ~folder
                ~state:call_graph
                ~location
                ({ Subscript.origin; _ } as subscript)
              =
              let call_graph =
                match origin with
                | Some origin when Origin.is_from_match origin ->
                    let call = Subscript.lower_to_call ~location subscript in
                    add_unresolved_callee ~location ~call call_graph
                | _ -> call_graph
              in
              Folder.default_fold_subscript ~folder ~state:call_graph subscript
            in
            let fold_call ~folder ~state:call_graph ~location ({ Call.origin; _ } as call) =
              let call_graph =
                match origin with
                | Some origin when Origin.is_from_match origin ->
                    add_unresolved_callee ~location ~call call_graph
                | _ -> call_graph
              in
              Folder.default_fold_call ~folder ~state:call_graph call
            in
            let fold_slice ~folder ~state:call_graph ~location ({ Slice.origin; _ } as slice) =
              let call_graph =
                match origin with
                | Some origin when Origin.is_from_match origin ->
                    let call = Slice.lower_to_call ~location slice in
                    add_unresolved_callee ~location ~call call_graph
                | _ -> call_graph
              in
              Folder.default_fold_slice ~folder ~state:call_graph slice
            in
            Folder.fold
              ~folder:
                (Folder.create
                   ~fold_binary_operator
                   ~fold_comparison_operator
                   ~fold_subscript
                   ~fold_slice
                   ~fold_call
                   ())
              ~state:call_graph
              test
          in
          List.fold ~f:add_case_targets ~init:call_graph cases
        in
        let module NodeVisitor = Ast.Visit.MakeNodeVisitor (struct
          type t = DefineCallGraph.t

          let visit_expression
              call_graph
              { Node.value = expression; location = expression_location }
            =
            match expression with
            | Expression.Name (Ast.Expression.Name.Attribute attribute_access) ->
                (* For each attribute access, check the base and determine whether the attribute has
                   a user-provided model. *)
                add_attribute_accesses ~location:expression_location ~attribute_access call_graph
            | Expression.Call
                {
                  Call.callee = { Node.value = Name (Name.Identifier "getattr"); _ };
                  arguments =
                    { Call.Argument.value = base; name = None }
                    :: {
                         Call.Argument.value =
                           {
                             Node.value =
                               Expression.Constant
                                 (Constant.String { StringLiteral.value = attribute; _ });
                             _;
                           };
                         name = None;
                       }
                    :: ([] | [_]);
                  origin = call_origin;
                } ->
                let origin =
                  Some
                    (Origin.create
                       ?base:call_origin
                       ~location:expression_location
                       Origin.GetAttrConstantLiteral)
                in
                add_attribute_accesses
                  ~location:expression_location
                  ~attribute_access:{ Name.Attribute.base; attribute; origin }
                  call_graph
            | Expression.Call
                {
                  Call.callee =
                    {
                      Node.value =
                        Name
                          (Name.Attribute
                            {
                              base = { Node.value = Name (Name.Identifier "object"); _ };
                              attribute = "__setattr__";
                              _;
                            });
                      _;
                    };
                  arguments =
                    [
                      { Call.Argument.value = self; name = None };
                      {
                        Call.Argument.value =
                          {
                            Node.value =
                              Expression.Constant
                                (Constant.String { value = attribute; kind = String; _ });
                            _;
                          };
                        name = None;
                      };
                      { Call.Argument.value = _; name = None };
                    ];
                  origin = call_origin;
                } ->
                let origin =
                  Some
                    (Origin.create
                       ?base:call_origin
                       ~location:expression_location
                       Origin.SetAttrConstantLiteral)
                in
                add_attribute_accesses
                  ~location:expression_location
                  ~attribute_access:{ Name.Attribute.base = self; attribute; origin }
                  call_graph
            | Expression.Call ({ Call.arguments; _ } as call) ->
                add_shim_target ~debug ~expression_location ~call ~arguments call_graph
                |> Option.value ~default:call_graph
            | _ -> call_graph


          let visit_statement call_graph { Node.value = statement; location = _ } =
            match statement with
            | Statement.Try try_ -> add_try_handler_targets ~try_ call_graph
            | Statement.Match match_ -> add_match_targets ~match_ call_graph
            | _ -> call_graph


          let node state = function
            | Ast.Visit.Expression expression -> visit_expression state expression
            | Ast.Visit.Statement statement -> visit_statement state statement
            | _ -> state


          let visit_statement_children _ _ = true

          let visit_expression_children _ _ = true

          let visit_format_string_children _ _ = true

          let visit_expression_based_on_parent ~parent_expression:_ _ = true
        end)
        in
        NodeVisitor.visit
          call_graph
          (Source.create [Node.create ~location:define_location (Statement.Define define)])
    | _ -> call_graph
  in
  let transform_call_graph _ callable call_graph =
    let debug, define_and_qualifier =
      match CallablesSharedMemory.ReadOnly.get_define callables_to_definitions_map callable with
      | AstResult.Some
          ({ CallablesSharedMemory.DefineAndQualifier.define = { Node.value = define; _ }; _ } as
          define_and_qualifier) ->
          let debug = PysaDump.should_dump_call_graph ~define ~callable in
          debug, Some define_and_qualifier
      | _ -> false, None
    in
    let () =
      log
        ~debug
        "Transforming Pyrefly call graph for `%a`: %a"
        Target.pp_external
        callable
        DefineCallGraph.pp
        call_graph
    in
    let call_indexer = CallGraph.Indexer.create () in
    let call_graph =
      DefineCallGraph.map_target
        ~f:(CallableToDecoratorsMap.SharedMemory.redirect_to_decorated callables_to_decorators_map)
        ~map_call_if:CallCallees.should_redirect_to_decorated
        ~map_return_if:(fun _ -> false)
        call_graph
    in
    let call_graph =
      if Target.is_decorated callable then
        transform_redirected_call_graph callable call_graph
      else
        add_targets ~debug ~define_and_qualifier callable call_graph
    in
    call_graph
    |> DefineCallGraph.dedup_and_sort
    |> DefineCallGraph.strip_higher_order_parameters
         ~should_strip:(Core.Hash_set.mem skip_call_higher_order_functions)
    |> DefineCallGraph.filter_empty_attribute_access
    |> DefineCallGraph.filter_empty_identifier
    |> DefineCallGraph.filter_empty_format_string_stringify
    |> DefineCallGraph.regenerate_call_indices ~indexer:call_indexer
  in
  let overrides_exist target =
    match override_graph with
    | Some override_graph ->
        OverrideGraph.SharedMemory.ReadOnly.overrides_exist override_graph target
    | None -> false
  in
  let get_overriding_types target =
    match override_graph with
    | Some override_graph ->
        OverrideGraph.SharedMemory.ReadOnly.get_overriding_types override_graph ~member:target
    | None -> None
  in
  let global_is_string_literal global =
    GlobalConstants.SharedMemory.ReadOnly.mem global_constants global
  in
  let { CallGraph.SharedMemory.whole_program_call_graph; define_call_graphs } =
    PyreflyApi.ReadOnly.parse_call_graphs
      pyrefly_api
      ~scheduler
      ~scheduler_policies
      ~overrides_exist
      ~get_overriding_types
      ~global_is_string_literal
      ~store_shared_memory
      ~attribute_targets
      ~skip_analysis_targets
      ~find_missing_flows
      ~definitions
      ~create_dependency_for
      ~redirect_to_decorated:
        (CallableToDecoratorsMap.SharedMemory.redirect_to_decorated_opt callables_to_decorators_map)
      ~transform_call_graph
  in
  let () =
    let define_call_graphs_read_only = CallGraph.SharedMemory.read_only define_call_graphs in
    DefineCallGraph.save_to_directory
      ~scheduler
      ~static_analysis_configuration
      ~resolve_qualifier:(CallablesSharedMemory.ReadOnly.get_qualifier callables_to_definitions_map)
      ~resolve_module_path
      ~get_call_graph:(fun callable ->
        CallGraph.SharedMemory.ReadOnly.get define_call_graphs_read_only ~cache:false ~callable)
      ~json_kind:NewlineDelimitedJson.Kind.CallGraph
      ~filename_prefix:"call-graph"
      ~callables:definitions
  in
  { CallGraph.SharedMemory.whole_program_call_graph; define_call_graphs }
