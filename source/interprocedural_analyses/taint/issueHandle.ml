(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open Interprocedural

(* After stripping the path prefix, the callable is not necessarily a valid callable, hence we
   represent it as an arbitrary string. *)
module CanonicalCallee = struct
  type t =
    | Target of Target.t
    | Name of string
  [@@deriving compare, equal, hash, sexp, show]

  let external_name ~display_api = function
    | Target target -> Target.external_name ~display_api target
    | Name name -> name


  (* Strip target parameters. Must be used on a `Target` variant. *)
  let strip_parameters = function
    | Target target -> Target (Target.strip_parameters target)
    | Name _ -> failwith "CanonicalCallee.strip_parameters expects a `Target` callee"


  (* Strip the path prefix (if present) from the target. Must be used on a `Target` variant. Returns
     a `Name` variant, since the new callable is not necessarily valid. *)
  let strip_path_prefix ~display_api = function
    | Target target ->
        Name (Target.external_name ~display_api ~transform:PyreflyApi.strip_path_prefix target)
    | Name _ -> failwith "CanonicalCallee.strip_path_prefix expects a `Target` callee"
end

module Sink = struct
  type t =
    | Call of {
        callee: CanonicalCallee.t;
        (* If the callee is a name or attribute access AST node, this is the attribute or name. *)
        callee_suffix: string option;
        index: int;
        parameter: AccessPath.Root.t;
      }
    | Global of {
        callee: CanonicalCallee.t;
        index: int;
      }
    | Return
    | LiteralStringSink of Sinks.t
    | ConditionalTestSink of Sinks.t
    | StringFormat of {
        callee: CanonicalCallee.t;
        index: int;
        parameter_index: int;
      }
  [@@deriving compare, equal, hash, sexp, show]

  (* Strip target parameters from the callee. Must be used with the `CanonicalCallee.Target`
     variant. *)
  let strip_callee_parameters = function
    | Call ({ callee; callee_suffix = Some callee_suffix; _ } as call) ->
        (* This runs before path-prefix stripping, so the callee is always a `Target`. *)
        let callee =
          match callee with
          | CanonicalCallee.Target callee -> callee
          | CanonicalCallee.Name _ ->
              failwith "IssueHandle.Sink.strip_callee_parameters expects a `Target` callee"
        in
        (* The callee might be a decorated function that was expanded, such as
           `decorator.inner[f=foo]`. In those cases, we want to use `foo` as the callee. *)
        let regular_targets_matching_suffix =
          callee
          |> Target.collect_nested_regular_targets
          |> List.filter ~f:(function
                 | Target.Regular.Function { name; _ }
                 | Target.Regular.Object name
                   when String.equal
                          callee_suffix
                          (name |> Ast.Reference.create |> Ast.Reference.last) ->
                     true
                 | Target.Regular.Method { method_name; _ }
                 | Target.Regular.Override { method_name; _ }
                   when String.equal callee_suffix method_name ->
                     true
                 | _ -> false)
        in
        let callee =
          match regular_targets_matching_suffix with
          | [regular_target] -> Target.Regular regular_target
          | _ -> Target.strip_parameters callee
        in
        Call
          {
            call with
            callee = CanonicalCallee.Target callee;
            callee_suffix =
              None (* Remove the callee suffix since it's not part of the handle (historical) *);
          }
    | Call ({ callee; callee_suffix = None; _ } as call) ->
        Call { call with callee = CanonicalCallee.strip_parameters callee }
    | Global ({ callee; _ } as global) ->
        Global { global with callee = CanonicalCallee.strip_parameters callee }
    | StringFormat ({ callee; _ } as string_format) ->
        StringFormat { string_format with callee = CanonicalCallee.strip_parameters callee }
    | (Return | LiteralStringSink _ | ConditionalTestSink _) as sink -> sink


  (* Strip the path prefix (if present) from the callee. Must be used on the
     `CanonicalCallee.Target` variant. *)
  let strip_callee_path_prefix ~display_api = function
    | Call ({ callee; _ } as call) ->
        Call { call with callee = CanonicalCallee.strip_path_prefix ~display_api callee }
    | Global ({ callee; _ } as global) ->
        Global { global with callee = CanonicalCallee.strip_path_prefix ~display_api callee }
    | StringFormat ({ callee; _ } as string_format) ->
        StringFormat
          { string_format with callee = CanonicalCallee.strip_path_prefix ~display_api callee }
    | (Return | LiteralStringSink _ | ConditionalTestSink _) as sink -> sink


  let make_call ~call_target:{ CallGraph.CallTarget.target; index; _ } ~root ~callee_as_name =
    let root =
      (* Ignore extra information in the parameter in order to group issues together. *)
      let open AccessPath.Root in
      match root with
      | LocalResult -> LocalResult
      | PositionalParameter { name; _ } -> NamedParameter { name }
      | NamedParameter { name } -> NamedParameter { name }
      | StarParameter _ -> StarParameter { position = 0 }
      | StarStarParameter _ -> StarStarParameter { excluded = Ast.Identifier.SerializableSet.empty }
      | Variable name -> Variable name
      | CapturedVariable name -> CapturedVariable name
    in
    let target = Target.for_issue_handle target in
    let callee_suffix =
      match callee_as_name with
      | Some (Ast.Expression.Name.Identifier name) -> Some name
      | Some (Ast.Expression.Name.Attribute { attribute; _ }) -> Some attribute
      | None -> None
    in
    Call { callee = CanonicalCallee.Target target; callee_suffix; index; parameter = root }


  let make_global ~call_target:{ CallGraph.CallTarget.target; index; _ } =
    Global { callee = CanonicalCallee.Target target; index }


  let to_json ~display_api = function
    | Call { callee; callee_suffix; index; parameter } ->
        let json =
          [
            "index", `Int index;
            "parameter", `String (AccessPath.Root.show_for_issue_handle parameter);
          ]
        in
        let json =
          match callee_suffix with
          | Some callee_suffix -> ("callee_suffix", `String callee_suffix) :: json
          | None -> json
        in
        let json =
          ("kind", `String "Call")
          :: ("callee", `String (CanonicalCallee.external_name ~display_api callee))
          :: json
        in
        `Assoc json
    | Global { callee; index } ->
        `Assoc
          [
            "kind", `String "Global";
            "callee", `String (CanonicalCallee.external_name ~display_api callee);
            "index", `Int index;
          ]
    | Return -> `Assoc ["kind", `String "Return"]
    | LiteralStringSink sink ->
        `Assoc ["kind", `String "LiteralStringSink"; "sink", `String (Sinks.show sink)]
    | ConditionalTestSink sink ->
        `Assoc ["kind", `String "ConditionalTestSink"; "sink", `String (Sinks.show sink)]
    | StringFormat { callee; index; parameter_index } ->
        `Assoc
          [
            "kind", `String "StringFormat";
            "callee", `String (CanonicalCallee.external_name ~display_api callee);
            "index", `Int index;
            "parameter_index", `Int parameter_index;
          ]
end

module T = struct
  type t = {
    code: int;
    callable: CanonicalCallee.t;
    sink: Sink.t;
  }
  [@@deriving compare, equal, hash, sexp, show]

  (* Strip the target parameters from callables. Must be used with `CanonicalCallee.Target`
     variants. *)
  let strip_all_callable_parameters ({ callable; sink; _ } as handle) =
    {
      handle with
      callable = CanonicalCallee.strip_parameters callable;
      sink = Sink.strip_callee_parameters sink;
    }


  (* Remove the source path prefix - that we sometimes add to callable names to disambiguate
     conflicting module names - so that issue handles remain stable across runs. Must be used with
     the `CanonicalCallee.Target` variants and produces the `CanonicalCallee.Name` variants, since
     the produced callables names might not be valid callables. *)
  let strip_callable_path_prefix ~display_api ({ callable; sink; _ } as handle) =
    {
      handle with
      callable = CanonicalCallee.strip_path_prefix ~display_api callable;
      sink = Sink.strip_callee_path_prefix ~display_api sink;
    }


  let master_handle ~display_api { code; callable; sink = sink_handle; _ } =
    let version = 0 (* Increment the version on format change. *) in
    let sink_handle =
      match sink_handle with
      | Call { callee; callee_suffix = _; index; parameter } ->
          Format.asprintf
            "Call|%s|%d|%s"
            (CanonicalCallee.external_name ~display_api callee)
            index
            (AccessPath.Root.show_for_issue_handle parameter)
      | Global { callee; index } ->
          Format.asprintf "Global|%s|%d" (CanonicalCallee.external_name ~display_api callee) index
      | Return -> "Return"
      | LiteralStringSink sink -> Format.asprintf "LiteralStringSink|%a" Sinks.pp sink
      | ConditionalTestSink sink -> Format.asprintf "ConditionalTestSink|%a" Sinks.pp sink
      | StringFormat { callee; index; parameter_index } ->
          Format.asprintf
            "StringFormat|%s|%d|%d"
            (CanonicalCallee.external_name ~display_api callee)
            index
            parameter_index
    in
    let full_handle =
      Format.asprintf
        "%s:%d:%d:%s"
        (CanonicalCallee.external_name ~display_api callable)
        code
        version
        sink_handle
    in
    let hash = full_handle |> Md5.digest_string |> Md5.to_hex in
    let short_handle =
      String.sub
        full_handle
        ~pos:0
        ~len:(min (String.length full_handle) (255 - String.length hash - 1))
    in
    Format.asprintf "%s:%s" short_handle hash


  (* Since we use interning, the default comparison wouldn't be deterministic: i.e, two different
     runs might produce a different order. This function is meant to produce a consistent order
     between runs. *)
  let deterministic_compare ~display_api left right =
    let master_handle_compare =
      String.compare (master_handle ~display_api left) (master_handle ~display_api right)
    in
    if master_handle_compare <> 0 then
      master_handle_compare
    else
      (* Use the autogenerated compare to compare the rest of the fields that are not included in
         the master handle (such as callee_suffix). Those fields are not interned so the default
         comparison should be deterministic. *)
      compare left right


  let name = "IssueHandle"
end

include T
module SerializableMap = Data_structures.SerializableMap.Make (T)
module Set = Stdlib.Set.Make (T)
