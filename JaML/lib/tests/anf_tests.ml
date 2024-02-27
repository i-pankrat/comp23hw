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
       (match Closure.closure typed_commands |> Lambdalift.lambda_lift |> Anfconv.anf with
        | Error err -> Format.printf "Anf error: %s%!" err
        | Ok anfstatements -> Format.printf "%a" pp_anfstatements anfstatements))
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
  [%expect {|
    let test f x =
        let #app1 = (f x 1) in #app1
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
        let #app1 = (f 1 2) in if #app1 then
        let #binop2 = (x + 1) in
        let #binop3 = (#binop2 + 3) in #binop3 else
        let #binop4 = (y + 4) in
        let #binop5 = (#binop4 + 5) in #binop5
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
        let #app4 = (fact #binop3 #binop2) in #app4;
    let fac_tailrec n =
        let #app5 = (fact n 1) in #app5
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
        let #app1 = (f fst snd) in #app1;
    let sum a b =
        let #binop2 = (a + b) in #binop2;
    let x =
        let #app4 = (apply2 sum 3 4) in
        let #app5 = (apply2 sum 1 2) in
        let #tuple3 = (#app4, #app5) in #tuple3
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
        let #app4 = (f #binop3 #binop2) in
        let #app5 = (f #app4 3) in
        let #app6 = (f #app5 #binop1) in #app6
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
        let #app4 = (sum 1 2 3 4 5 6) in #app4
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
        let #make_closure6 = make_closure(sum6, d c b a) in #make_closure6;
    let sum2 a b =
        let #make_closure7 = make_closure(sum4, b a) in #make_closure7;
    let rer =
        let #app8 = (sum2 1 2 3 4 5 6) in #app8
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
        let #app2 = (k #binop1) in #app2;
    let #closure_fun2 x = x;
    let fack n k =
        let #binop3 = (n <= 1) in if #binop3 then
        let #app4 = (k 1) in #app4 else
        let #make_closure5 = make_closure(#closure_fun1, n k) in
        let #binop6 = (n - 1) in
        let #app7 = (fack #binop6 #make_closure5) in #app7;
    let fac n =
        let #app8 = (fack n #closure_fun2) in #app8
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
        let #app2 = (acc #binop1) in #app2;
    let #closure_fun2 n fibo_cps acc x =
        let #make_closure3 = make_closure(#closure_fun1, x acc) in
        let #binop4 = (n - 2) in
        let #app5 = (fibo_cps #binop4 #make_closure3) in #app5;
    let #closure_fun3 x = x;
    let fibo_cps n acc =
        let #binop6 = (n < 3) in if #binop6 then
        let #app7 = (acc 1) in #app7 else
        let #make_closure8 = make_closure(#closure_fun2, n fibo_cps acc) in
        let #binop9 = (n - 1) in
        let #app10 = (fibo_cps #binop9 #make_closure8) in #app10;
    let fibo n =
        let #app11 = (fibo_cps n #closure_fun3) in #app11
 |}]
;;
