(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open OUnit2
open Interprocedural
module ModuleId = PyreflyTypes.ModuleId
module LocalClassId = PyreflyTypes.LocalClassId
module LocalClassFieldId = PyreflyTypes.LocalClassFieldId
module FuncDefIndex = PyreflyTypes.FuncDefIndex
module LocalFunctionId = PyreflyTypes.LocalFunctionId
module ClassId = PyreflyTypes.ClassId
module CallableId = PyreflyTypes.CallableId

(* Field widths from the packed encodings. *)
let mask bits = (1 lsl bits) - 1

let max_module_id = mask ModuleId.bit_width

let max_func_def_index = (1 lsl 35) - 1

let max_class_top_level_local_class_id = mask LocalClassId.bit_width

let max_class_field_class_id = mask LocalClassId.bit_width

let max_class_field_field_id = (1 lsl 18) - 1

let assert_callable_round_trip ~module_id local_function_id =
  let module_id = ModuleId.from_int module_id in
  let encoded = CallableId.encode ~module_id local_function_id in
  let decoded_module_id, decoded_local_function_id = CallableId.decode encoded in
  (* decode (encode x) = x *)
  assert_equal ~cmp:ModuleId.equal ~printer:ModuleId.show module_id decoded_module_id;
  assert_equal
    ~cmp:LocalFunctionId.equal
    ~printer:LocalFunctionId.show
    local_function_id
    decoded_local_function_id;
  (* encode (decode y) = y *)
  let re_encoded = CallableId.encode ~module_id:decoded_module_id decoded_local_function_id in
  assert_equal ~cmp:CallableId.equal ~printer:CallableId.show encoded re_encoded


let test_callable_id_round_trip _ =
  (* Function variant, at zero and at the maximum func_def_index. *)
  assert_callable_round_trip ~module_id:0 (LocalFunctionId.Function (FuncDefIndex.from_int 0));
  assert_callable_round_trip
    ~module_id:max_module_id
    (LocalFunctionId.Function (FuncDefIndex.from_int max_func_def_index));
  (* FunctionDecoratedTarget variant. *)
  assert_callable_round_trip
    ~module_id:1
    (LocalFunctionId.FunctionDecoratedTarget (FuncDefIndex.from_int 7));
  assert_callable_round_trip
    ~module_id:max_module_id
    (LocalFunctionId.FunctionDecoratedTarget (FuncDefIndex.from_int max_func_def_index));
  (* ModuleTopLevel variant (zero payload). *)
  assert_callable_round_trip ~module_id:0 LocalFunctionId.ModuleTopLevel;
  assert_callable_round_trip ~module_id:max_module_id LocalFunctionId.ModuleTopLevel;
  (* ClassTopLevel variant. *)
  assert_callable_round_trip ~module_id:3 (LocalFunctionId.ClassTopLevel (LocalClassId.from_int 0));
  assert_callable_round_trip
    ~module_id:max_module_id
    (LocalFunctionId.ClassTopLevel (LocalClassId.from_int max_class_top_level_local_class_id));
  (* ClassField variant. *)
  assert_callable_round_trip
    ~module_id:5
    (LocalFunctionId.ClassField
       { class_id = LocalClassId.from_int 0; field_id = LocalClassFieldId.from_int 0 });
  assert_callable_round_trip
    ~module_id:max_module_id
    (LocalFunctionId.ClassField
       {
         class_id = LocalClassId.from_int max_class_field_class_id;
         field_id = LocalClassFieldId.from_int max_class_field_field_id;
       })


let assert_class_round_trip ~module_id ~local_class_id =
  let module_id = ModuleId.from_int module_id in
  let local_class_id = LocalClassId.from_int local_class_id in
  let encoded = ClassId.encode ~module_id local_class_id in
  let decoded_module_id, decoded_local_class_id = ClassId.decode encoded in
  assert_equal ~cmp:ModuleId.equal ~printer:ModuleId.show module_id decoded_module_id;
  assert_equal
    ~cmp:LocalClassId.equal
    ~printer:LocalClassId.show
    local_class_id
    decoded_local_class_id;
  let re_encoded = ClassId.encode ~module_id:decoded_module_id decoded_local_class_id in
  assert_equal ~cmp:ClassId.equal ~printer:ClassId.show encoded re_encoded


let test_class_id_round_trip _ =
  assert_class_round_trip ~module_id:0 ~local_class_id:0;
  assert_class_round_trip ~module_id:7 ~local_class_id:42;
  assert_class_round_trip
    ~module_id:max_module_id
    ~local_class_id:max_class_top_level_local_class_id


let test_callable_id_compare_orders_by_module_first _ =
  (* A callable in an earlier module sorts before one in a later module, even when the earlier one's
     tag and payload are larger. *)
  let earlier_module_large_payload =
    CallableId.encode
      ~module_id:(ModuleId.from_int 1)
      (LocalFunctionId.FunctionDecoratedTarget (FuncDefIndex.from_int max_func_def_index))
  in
  let later_module_small_payload =
    CallableId.encode
      ~module_id:(ModuleId.from_int 2)
      (LocalFunctionId.Function (FuncDefIndex.from_int 0))
  in
  assert_bool
    "callable in earlier module should sort first"
    (CallableId.compare earlier_module_large_payload later_module_small_payload < 0);
  (* Within the same module, the tag orders: Function (tag 0) before FunctionDecoratedTarget (tag
     1). *)
  let same_module_function =
    CallableId.encode
      ~module_id:(ModuleId.from_int 4)
      (LocalFunctionId.Function (FuncDefIndex.from_int 100))
  in
  let same_module_decorated =
    CallableId.encode
      ~module_id:(ModuleId.from_int 4)
      (LocalFunctionId.FunctionDecoratedTarget (FuncDefIndex.from_int 0))
  in
  assert_bool
    "Function should sort before FunctionDecoratedTarget in the same module"
    (CallableId.compare same_module_function same_module_decorated < 0);
  (* Within the same module and tag, the smaller payload sorts first. *)
  let small_payload =
    CallableId.encode
      ~module_id:(ModuleId.from_int 4)
      (LocalFunctionId.Function (FuncDefIndex.from_int 1))
  in
  let large_payload =
    CallableId.encode
      ~module_id:(ModuleId.from_int 4)
      (LocalFunctionId.Function (FuncDefIndex.from_int 2))
  in
  assert_bool
    "smaller payload should sort first within the same module and tag"
    (CallableId.compare small_payload large_payload < 0)


let test_class_id_compare_orders_by_module_first _ =
  let earlier_module_large_class =
    ClassId.encode
      ~module_id:(ModuleId.from_int 1)
      (LocalClassId.from_int max_class_top_level_local_class_id)
  in
  let later_module_small_class =
    ClassId.encode ~module_id:(ModuleId.from_int 2) (LocalClassId.from_int 0)
  in
  assert_bool
    "class in earlier module should sort first"
    (ClassId.compare earlier_module_large_class later_module_small_class < 0)


let assert_raises_something ~message f =
  let raised =
    try
      let _ = f () in
      false
    with
    | _ -> true
  in
  assert_bool message raised


let test_callable_id_encode_asserts _ =
  assert_raises_something ~message:"module_id above 24 bits should raise" (fun () ->
      CallableId.encode
        ~module_id:(ModuleId.from_int (max_module_id + 1))
        (LocalFunctionId.Function (FuncDefIndex.from_int 0)));
  assert_raises_something ~message:"func_def_index above 35 bits should raise" (fun () ->
      CallableId.encode
        ~module_id:(ModuleId.from_int 0)
        (LocalFunctionId.Function (FuncDefIndex.from_int (max_func_def_index + 1))));
  assert_raises_something ~message:"class field class_id above 17 bits should raise" (fun () ->
      CallableId.encode
        ~module_id:(ModuleId.from_int 0)
        (LocalFunctionId.ClassField
           {
             class_id = LocalClassId.from_int (max_class_field_class_id + 1);
             field_id = LocalClassFieldId.from_int 0;
           }));
  assert_raises_something ~message:"class field field_id above 18 bits should raise" (fun () ->
      CallableId.encode
        ~module_id:(ModuleId.from_int 0)
        (LocalFunctionId.ClassField
           {
             class_id = LocalClassId.from_int 0;
             field_id = LocalClassFieldId.from_int (max_class_field_field_id + 1);
           }))


let test_class_id_encode_asserts _ =
  assert_raises_something ~message:"module_id above 24 bits should raise" (fun () ->
      ClassId.encode ~module_id:(ModuleId.from_int (max_module_id + 1)) (LocalClassId.from_int 0));
  assert_raises_something ~message:"local_class_id above 17 bits should raise" (fun () ->
      ClassId.encode
        ~module_id:(ModuleId.from_int 0)
        (LocalClassId.from_int (max_class_top_level_local_class_id + 1)))


let test_callable_id_decorated_helpers _ =
  let module_id = ModuleId.from_int 5 in
  let func_def_index = FuncDefIndex.from_int 42 in
  let undecorated = CallableId.encode ~module_id (LocalFunctionId.Function func_def_index) in
  let decorated =
    CallableId.encode ~module_id (LocalFunctionId.FunctionDecoratedTarget func_def_index)
  in
  (* is_decorated reads the tag. *)
  assert_bool "undecorated function is not decorated" (not (CallableId.is_decorated undecorated));
  assert_bool "decorated function is decorated" (CallableId.is_decorated decorated);
  (* to_decorated / to_undecorated swap the tag, keeping module and func def index. *)
  assert_equal
    ~cmp:CallableId.equal
    ~printer:CallableId.show
    decorated
    (CallableId.to_decorated undecorated);
  assert_equal
    ~cmp:CallableId.equal
    ~printer:CallableId.show
    undecorated
    (CallableId.to_undecorated decorated);
  (* Round trip through the valid directions. *)
  assert_equal
    ~cmp:CallableId.equal
    ~printer:CallableId.show
    undecorated
    (undecorated |> CallableId.to_decorated |> CallableId.to_undecorated);
  assert_equal
    ~cmp:CallableId.equal
    ~printer:CallableId.show
    decorated
    (decorated |> CallableId.to_undecorated |> CallableId.to_decorated);
  (* Calling a helper on an id already in the target state raises. *)
  assert_raises_something ~message:"to_decorated on an already-decorated id should raise" (fun () ->
      CallableId.to_decorated decorated);
  assert_raises_something ~message:"to_undecorated on an undecorated id should raise" (fun () ->
      CallableId.to_undecorated undecorated);
  (* Non-function variants raise. *)
  let module_top_level = CallableId.encode ~module_id LocalFunctionId.ModuleTopLevel in
  assert_bool "module top level is not decorated" (not (CallableId.is_decorated module_top_level));
  assert_raises_something ~message:"to_decorated on a non-function id should raise" (fun () ->
      CallableId.to_decorated module_top_level);
  assert_raises_something ~message:"to_undecorated on a non-function id should raise" (fun () ->
      CallableId.to_undecorated module_top_level)


let () =
  "pyrefly_types"
  >::: [
         "callable_id_round_trip" >:: test_callable_id_round_trip;
         "class_id_round_trip" >:: test_class_id_round_trip;
         "callable_id_compare_orders_by_module_first"
         >:: test_callable_id_compare_orders_by_module_first;
         "class_id_compare_orders_by_module_first" >:: test_class_id_compare_orders_by_module_first;
         "callable_id_encode_asserts" >:: test_callable_id_encode_asserts;
         "class_id_encode_asserts" >:: test_class_id_encode_asserts;
         "callable_id_decorated_helpers" >:: test_callable_id_decorated_helpers;
       ]
  |> Test.run
