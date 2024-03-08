(** Copyright 2023-2024, Ilya Pankratov, Maxim Drumov *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open Jaml_lib
open Jaml_lib.Pprintanf
open Jaml_lib.Parser
open Result

let run_anf_tests test_case =
  let fmt = Format.std_formatter in
  match parse test_case with
  | Error err -> pp_error fmt err
  | Ok commands ->
    (match Inferencer.infer Inferencer.Enable commands with
     | Error err -> Inferencer.pp_error fmt err
     | Ok typed_commands ->
       Closure.closure typed_commands
       |> Lambdalift.lambda_lift
       |> Anfconv.anf
       |> Format.printf "%a" pp_anfstatements)
;;

let%expect_test _ =
  let _ =
    let e = "let x = (1 + 2) * 3" in
    run_anf_tests e
  in
  [%expect
    {|
    let x =
        let _binop1 = (1 + 2) in
        let _binop2 = (_binop1 * 3) in _binop2
 |}]
;;

let%expect_test _ =
  let _ =
    let e = "let x y = (6 + 9) * (4 + y) / y" in
    run_anf_tests e
  in
  [%expect
    {|
    let x y =
        let _binop1 = (6 + 9) in
        let _binop2 = (4 + y) in
        let _binop3 = (_binop1 * _binop2) in
        let _binop4 = (_binop3 / y) in _binop4
 |}]
;;

let%expect_test _ =
  let _ =
    let e = "let test f x = f x 1" in
    run_anf_tests e
  in
  [%expect
    {|
    let test f x =
        let _closure1 = add_args_to_closure(f x 1) in _closure1
 |}]
;;

let%expect_test _ =
  let _ =
    let e = "let test f x y = if f 1 2 then x + 1 + 3 else y + 4 + 5" in
    run_anf_tests e
  in
  [%expect
    {|
    let test f x y =
        let _closure1 = add_args_to_closure(f 1 2) in
        let _if6 = (if _closure1 then
        let _binop2 = (x + 1) in
        let _binop3 = (_binop2 + 3) in _binop3 else
        let _binop4 = (y + 4) in
        let _binop5 = (_binop4 + 5) in _binop5) in _if6
    |}]
;;

let%expect_test _ =
  let _ =
    let e =
      {|
    let rec fact n acc = if n < 1 then acc else fact (n - 1) (n * acc)
    let fac_tailrec n = fact n 1
      |}
    in
    run_anf_tests e
  in
  [%expect
    {|
    let fact n acc =
        let _binop1 = (n < 1) in
        let _if6 = (if _binop1 then acc else
        let _binop2 = (n * acc) in
        let _binop3 = (n - 1) in
        let _empty_closure4 = make_empty_closure(fact) in
        let _closure5 = add_args_to_closure(_empty_closure4 _binop3 _binop2) in _closure5) in _if6;
    let fac_tailrec n =
        let _empty_closure7 = make_empty_closure(fact) in
        let _closure8 = add_args_to_closure(_empty_closure7 n 1) in _closure8
  |}]
;;

let%expect_test _ =
  let _ =
    let e = "let x = (1 + 2, 3 + 1)" in
    run_anf_tests e
  in
  [%expect
    {|
    let x =
        let _binop2 = (1 + 2) in
        let _binop3 = (3 + 1) in
        let _tuple1 = (_binop2, _binop3) in _tuple1 |}]
;;

let%expect_test _ =
  let _ =
    let e = "let ((x, s), y) = ((1 ,(2 - 4)), 3 + 1)" in
    run_anf_tests e
  in
  [%expect
    {|
    let _tuple_out1 =
        let _binop3 = (2 - 4) in
        let _tuple2 = (1, _binop3) in
        let _binop4 = (3 + 1) in
        let _tuple1 = (_tuple2, _binop4) in _tuple1;
    let x =
        let _global_var5 = empty_app(_tuple_out1) in
        let _take6 = take(_global_var5, 0) in
        let _take7 = take(_take6, 0) in _take7;
    let s =
        let _global_var8 = empty_app(_tuple_out1) in
        let _take9 = take(_global_var8, 0) in
        let _take10 = take(_take9, 1) in _take10;
    let y =
        let _global_var11 = empty_app(_tuple_out1) in
        let _take12 = take(_global_var11, 1) in _take12 |}]
;;

let%expect_test _ =
  let _ =
    let e =
      {|

    let apply2 f fst snd = f fst snd
    let sum a b = a + b
    let x = (apply2 sum 3 4, apply2 sum 1 2)

    |}
    in
    run_anf_tests e
  in
  [%expect
    {|
    let apply2 f fst snd =
        let _closure1 = add_args_to_closure(f fst snd) in _closure1;
    let sum a b =
        let _binop2 = (a + b) in _binop2;
    let x =
        let _empty_closure4 = make_empty_closure(sum) in
        let _empty_closure5 = make_empty_closure(apply2) in
        let _closure6 = add_args_to_closure(_empty_closure5 _empty_closure4 3 4) in
        let _empty_closure7 = make_empty_closure(sum) in
        let _empty_closure8 = make_empty_closure(apply2) in
        let _closure9 = add_args_to_closure(_empty_closure8 _empty_closure7 1 2) in
        let _tuple3 = (_closure6, _closure9) in _tuple3
 |}]
;;

let%expect_test _ =
  let _ =
    let e = {|

    let apply2 f = f (f (f (1 + 0) (2 + 0)) 3) (4 + 5)

    |} in
    run_anf_tests e
  in
  [%expect
    {|
    let apply2 f =
        let _binop1 = (4 + 5) in
        let _binop2 = (2 + 0) in
        let _binop3 = (1 + 0) in
        let _closure4 = add_args_to_closure(f _binop3 _binop2) in
        let _closure5 = add_args_to_closure(f _closure4 3) in
        let _closure6 = add_args_to_closure(f _closure5 _binop1) in _closure6
 |}]
;;

let%expect_test _ =
  let _ =
    let e =
      {|
    let sum a b = (fun c d e f -> a + d + e + f)
    let partial = sum 1 2 3 4 5 6 
    |}
    in
    run_anf_tests e
  in
  [%expect
    {|
    let sum a b c d e f =
        let _binop1 = (a + d) in
        let _binop2 = (_binop1 + e) in
        let _binop3 = (_binop2 + f) in _binop3;
    let partial =
        let _empty_closure4 = make_empty_closure(sum) in
        let _closure5 = add_args_to_closure(_empty_closure4 1 2 3 4 5 6) in _closure5
 |}]
;;

let%expect_test _ =
  let _ =
    let e =
      {|
      let sum6 a b c d e f = a + b + c + d + e + f
      let sum4 a b c d = sum6 a b c d
      let sum2 a b = sum4 a b
      let rer = sum2 1 2 3 4 5 6
      |}
    in
    run_anf_tests e
  in
  [%expect
    {|
    let sum6 a b c d e f =
        let _binop1 = (a + b) in
        let _binop2 = (_binop1 + c) in
        let _binop3 = (_binop2 + d) in
        let _binop4 = (_binop3 + e) in
        let _binop5 = (_binop4 + f) in _binop5;
    let sum4 a b c d =
        let _empty_closure6 = make_empty_closure(sum6) in
        let _closure7 = add_args_to_closure(_empty_closure6 a b c d) in _closure7;
    let sum2 a b =
        let _empty_closure8 = make_empty_closure(sum4) in
        let _closure9 = add_args_to_closure(_empty_closure8 a b) in _closure9;
    let rer =
        let _empty_closure10 = make_empty_closure(sum2) in
        let _closure11 = add_args_to_closure(_empty_closure10 1 2 3 4 5 6) in _closure11
 |}]
;;

let%expect_test _ =
  let _ =
    let e = {|
    let sum_cortage ((a, b), (d, e)) = a + b + d + e
      |} in
    run_anf_tests e
  in
  [%expect
    {|
    let sum_cortage _tuple_arg1 =
        let _take1 = take(_tuple_arg1, 0) in
        let _take2 = take(_take1, 0) in
        let a = _take2 in
        let _take3 = take(_tuple_arg1, 0) in
        let _take4 = take(_take3, 1) in
        let b = _take4 in
        let _take5 = take(_tuple_arg1, 1) in
        let _take6 = take(_take5, 0) in
        let d = _take6 in
        let _take7 = take(_tuple_arg1, 1) in
        let _take8 = take(_take7, 1) in
        let e = _take8 in
        let _binop9 = (a + b) in
        let _binop10 = (_binop9 + d) in
        let _binop11 = (_binop10 + e) in _binop11
 |}]
;;

let%expect_test _ =
  let _ =
    let e =
      {|
    let fac n =
      let rec fack n k =
      if n <= 1 then k 1
      else fack (n-1) ((fun k n m -> k (m * n)) k n) 
      in
      fack n (fun x -> x)
      |}
    in
    run_anf_tests e
  in
  [%expect
    {|
    let _closure_fun1 k n m =
        let _binop1 = (m * n) in
        let _closure2 = add_args_to_closure(k _binop1) in _closure2;
    let _closure_fun2 x = x;
    let fack n k =
        let _binop3 = (n <= 1) in
        let _if10 = (if _binop3 then
        let _closure4 = add_args_to_closure(k 1) in _closure4 else
        let _empty_closure5 = make_empty_closure(_closure_fun1) in
        let _closure6 = add_args_to_closure(_empty_closure5 k n) in
        let _binop7 = (n - 1) in
        let _empty_closure8 = make_empty_closure(fack) in
        let _closure9 = add_args_to_closure(_empty_closure8 _binop7 _closure6) in _closure9) in _if10;
    let fac n =
        let _empty_closure11 = make_empty_closure(_closure_fun2) in
        let _empty_closure12 = make_empty_closure(fack) in
        let _closure13 = add_args_to_closure(_empty_closure12 n _empty_closure11) in _closure13
 |}]
;;

let%expect_test _ =
  let _ =
    let e =
      {|
    let fibo n =
      let rec fibo_cps n acc =
      if n < 3 then acc 1
      else fibo_cps (n - 1) (fun x -> fibo_cps (n - 2) (fun y -> acc (x + y)))
      in
      fibo_cps n (fun x -> x)
      |}
    in
    run_anf_tests e
  in
  [%expect
    {|
    let _closure_fun1 x acc y =
        let _binop1 = (x + y) in
        let _closure2 = add_args_to_closure(acc _binop1) in _closure2;
    let _closure_fun2 n fibo_cps acc x =
        let _empty_closure3 = make_empty_closure(_closure_fun1) in
        let _closure4 = add_args_to_closure(_empty_closure3 x acc) in
        let _binop5 = (n - 2) in
        let _closure6 = add_args_to_closure(fibo_cps _binop5 _closure4) in _closure6;
    let _closure_fun3 x = x;
    let fibo_cps n acc =
        let _binop7 = (n < 3) in
        let _if15 = (if _binop7 then
        let _closure8 = add_args_to_closure(acc 1) in _closure8 else
        let _empty_closure9 = make_empty_closure(fibo_cps) in
        let _empty_closure10 = make_empty_closure(_closure_fun2) in
        let _closure11 = add_args_to_closure(_empty_closure10 n _empty_closure9 acc) in
        let _binop12 = (n - 1) in
        let _empty_closure13 = make_empty_closure(fibo_cps) in
        let _closure14 = add_args_to_closure(_empty_closure13 _binop12 _closure11) in _closure14) in _if15;
    let fibo n =
        let _empty_closure16 = make_empty_closure(_closure_fun3) in
        let _empty_closure17 = make_empty_closure(fibo_cps) in
        let _closure18 = add_args_to_closure(_empty_closure17 n _empty_closure16) in _closure18
 |}]
;;

let%expect_test _ =
  let _ =
    let e =
      {|
      let test x y z = (if x > 1 then (if y > 0 then 1 else 2) else (if z > 0 then 3 else 4))
      |}
    in
    run_anf_tests e
  in
  [%expect
    {|
    let test x y z =
        let _binop1 = (x > 1) in
        let _if6 = (if _binop1 then
        let _binop2 = (y > 0) in
        let _if3 = (if _binop2 then 1 else 2) in _if3 else
        let _binop4 = (z > 0) in
        let _if5 = (if _binop4 then 3 else 4) in _if5) in _if6

 |}]
;;

let%expect_test _ =
  let _ =
    let e =
      {|
      let sum a b = a + b
      let minus a b = a - b
      let test1 = sum
      let test1_res = test1 1 2
      let test2 = let x = 1 in sum
      let test2_res = test2 1 2
      let test3 a = if a = 1 then sum else minus
      let test3_res = test3 0 3 4
      let a = 5
      let b = a
      let c = 5 + a + b
      let sum4 arg = arg + a + b + 1 
      |}
    in
    run_anf_tests e
  in
  [%expect
    {|
    let sum a b =
        let _binop1 = (a + b) in _binop1;
    let minus a b =
        let _binop2 = (a - b) in _binop2;
    let test1 =
        let _empty_closure3 = make_empty_closure(sum) in _empty_closure3;
    let test1_res =
        let _global_var4 = empty_app(test1) in
        let _closure5 = add_args_to_closure(_global_var4 1 2) in _closure5;
    let test2 =
        let x = 1 in
        let _empty_closure6 = make_empty_closure(sum) in _empty_closure6;
    let test2_res =
        let _global_var7 = empty_app(test2) in
        let _closure8 = add_args_to_closure(_global_var7 1 2) in _closure8;
    let test3 a =
        let _binop9 = (a = 1) in
        let _if12 = (if _binop9 then
        let _empty_closure10 = make_empty_closure(sum) in _empty_closure10 else
        let _empty_closure11 = make_empty_closure(minus) in _empty_closure11) in _if12;
    let test3_res =
        let _empty_closure13 = make_empty_closure(test3) in
        let _closure14 = add_args_to_closure(_empty_closure13 0 3 4) in _closure14;
    let a = 5;
    let b =
        let _global_var15 = empty_app(a) in _global_var15;
    let c =
        let _global_var16 = empty_app(a) in
        let _binop17 = (5 + _global_var16) in
        let _global_var18 = empty_app(b) in
        let _binop19 = (_binop17 + _global_var18) in _binop19;
    let sum4 arg =
        let _global_var20 = empty_app(a) in
        let _binop21 = (arg + _global_var20) in
        let _global_var22 = empty_app(b) in
        let _binop23 = (_binop21 + _global_var22) in
        let _binop24 = (_binop23 + 1) in _binop24



 |}]
;;
