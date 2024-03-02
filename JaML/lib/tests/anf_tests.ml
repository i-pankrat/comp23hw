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
        let #binop1 = (1 + 2) in
        let #binop2 = (#binop1 * 3) in #binop2
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
        let #binop1 = (6 + 9) in
        let #binop2 = (4 + y) in
        let #binop3 = (#binop1 * #binop2) in
        let #binop4 = (#binop3 / y) in #binop4
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
        let #closure1 = make_closure(f, x) in
        let #closure2 = make_closure(#closure1, 1) in
        let #closure3 = #closure2 in #closure3
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
        let #closure1 = make_closure(f, 1) in
        let #closure2 = make_closure(#closure1, 2) in
        let #closure3 = #closure2 in if #closure3 then
        let #binop4 = (x + 1) in
        let #binop5 = (#binop4 + 3) in #binop5 else
        let #binop6 = (y + 4) in
        let #binop7 = (#binop6 + 5) in #binop7
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
    let rec fact n acc =
        let #binop1 = (n < 1) in if #binop1 then acc else
        let #binop2 = (n * acc) in
        let #binop3 = (n - 1) in
        let #app4 = fact #binop3 #binop2 in
        let #closure5 = #app4 in #closure5;
    let fac_tailrec n =
        let #app6 = fact n 1 in
        let #closure7 = #app6 in #closure7
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
        let #binop2 = (1 + 2) in
        let #binop3 = (3 + 1) in
        let #tuple1 = (#binop2, #binop3) in #tuple1 |}]
;;

let%expect_test _ =
  let _ =
    let e = "let ((x, s), y) = ((1 ,(2 - 4)), 3 + 1)" in
    run_anf_tests e
  in
  [%expect
    {|
    let #tuple_out1 =
        let #binop3 = (2 - 4) in
        let #tuple2 = (1, #binop3) in
        let #binop4 = (3 + 1) in
        let #tuple1 = (#tuple2, #binop4) in #tuple1;
    let x =
        let #take5 = take(#tuple_out1, 0) in
        let #take6 = take(#take5, 0) in #take6;
    let s =
        let #take7 = take(#tuple_out1, 0) in
        let #take8 = take(#take7, 1) in #take8;
    let y =
        let #take9 = take(#tuple_out1, 1) in #take9 |}]
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
        let #closure1 = make_closure(f, fst) in
        let #closure2 = make_closure(#closure1, snd) in
        let #closure3 = #closure2 in #closure3;
    let sum a b =
        let #binop4 = (a + b) in #binop4;
    let x =
        let #app6 = apply2 sum 3 4 in
        let #closure7 = #app6 in
        let #app8 = apply2 sum 1 2 in
        let #closure9 = #app8 in
        let #tuple5 = (#closure7, #closure9) in #tuple5
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
        let #binop1 = (4 + 5) in
        let #binop2 = (2 + 0) in
        let #binop3 = (1 + 0) in
        let #closure4 = make_closure(f, #binop3) in
        let #closure5 = make_closure(#closure4, #binop2) in
        let #closure6 = #closure5 in
        let #closure7 = make_closure(f, #closure6) in
        let #closure8 = make_closure(#closure7, 3) in
        let #closure9 = #closure8 in
        let #closure10 = make_closure(f, #closure9) in
        let #closure11 = make_closure(#closure10, #binop1) in
        let #closure12 = #closure11 in #closure12
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
        let #binop1 = (a + d) in
        let #binop2 = (#binop1 + e) in
        let #binop3 = (#binop2 + f) in #binop3;
    let partial =
        let #app4 = sum 1 2 3 4 5 6 in
        let #closure5 = #app4 in #closure5
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
        let #binop1 = (a + b) in
        let #binop2 = (#binop1 + c) in
        let #binop3 = (#binop2 + d) in
        let #binop4 = (#binop3 + e) in
        let #binop5 = (#binop4 + f) in #binop5;
    let sum4 a b c d =
        let #closure6 = make_closure(sum6, a) in
        let #closure7 = make_closure(#closure6, b) in
        let #closure8 = make_closure(#closure7, c) in
        let #closure9 = make_closure(#closure8, d) in
        let #closure10 = #closure9 in #closure10;
    let sum2 a b =
        let #closure11 = make_closure(sum4, a) in
        let #closure12 = make_closure(#closure11, b) in
        let #closure13 = #closure12 in #closure13;
    let rer =
        let #app14 = sum2 1 2 in
        let #closure15 = make_closure(#app14, 3) in
        let #closure16 = make_closure(#closure15, 4) in
        let #closure17 = make_closure(#closure16, 5) in
        let #closure18 = make_closure(#closure17, 6) in
        let #closure19 = #closure18 in #closure19
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
    let sum_cortage #tuple_arg1 =
        let #take1 = take(#tuple_arg1, 0) in
        let #take2 = take(#take1, 0) in
        let a = #take2 in
        let #take3 = take(#tuple_arg1, 0) in
        let #take4 = take(#take3, 1) in
        let b = #take4 in
        let #take5 = take(#tuple_arg1, 1) in
        let #take6 = take(#take5, 0) in
        let d = #take6 in
        let #take7 = take(#tuple_arg1, 1) in
        let #take8 = take(#take7, 1) in
        let e = #take8 in
        let #binop9 = (a + b) in
        let #binop10 = (#binop9 + d) in
        let #binop11 = (#binop10 + e) in #binop11
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
    let #closure_fun1 k n m =
        let #binop1 = (m * n) in
        let #closure2 = make_closure(k, #binop1) in
        let #closure3 = #closure2 in #closure3;
    let #closure_fun2 x = x;
    let fack n k =
        let #binop4 = (n <= 1) in if #binop4 then
        let #closure5 = make_closure(k, 1) in
        let #closure6 = #closure5 in #closure6 else
        let #closure7 = make_closure(#closure_fun1, k) in
        let #closure8 = make_closure(#closure7, n) in
        let #closure9 = #closure8 in
        let #binop10 = (n - 1) in
        let #app11 = fack #binop10 #closure9 in
        let #closure12 = #app11 in #closure12;
    let fac n =
        let #app13 = fack n #closure_fun2 in
        let #closure14 = #app13 in #closure14
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
    let #closure_fun1 x acc y =
        let #binop1 = (x + y) in
        let #closure2 = make_closure(acc, #binop1) in
        let #closure3 = #closure2 in #closure3;
    let #closure_fun2 n fibo_cps acc x =
        let #closure4 = make_closure(#closure_fun1, acc) in
        let #closure5 = make_closure(#closure4, x) in
        let #closure6 = #closure5 in
        let #binop7 = (n - 2) in
        let #closure8 = make_closure(fibo_cps, #binop7) in
        let #closure9 = make_closure(#closure8, #closure6) in
        let #closure10 = #closure9 in #closure10;
    let #closure_fun3 x = x;
    let fibo_cps n acc =
        let #binop11 = (n < 3) in if #binop11 then
        let #closure12 = make_closure(acc, 1) in
        let #closure13 = #closure12 in #closure13 else
        let #closure14 = make_closure(#closure_fun2, acc) in
        let #closure15 = make_closure(#closure14, fibo_cps) in
        let #closure16 = make_closure(#closure15, n) in
        let #closure17 = #closure16 in
        let #binop18 = (n - 1) in
        let #app19 = fibo_cps #binop18 #closure17 in
        let #closure20 = #app19 in #closure20;
    let fibo n =
        let #app21 = fibo_cps n #closure_fun3 in
        let #closure22 = #app21 in #closure22
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
  [%expect {|

 |}]
;;
