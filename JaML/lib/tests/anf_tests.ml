(** Copyright 2023-2024, Ilya Pankratov, Maxim Drumov *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open Jaml_lib
open Jaml_lib.Pprintanf

let run_anf_tests test_case =
  Parser.parse test_case
  |> Result.get_ok
  |> Inferencer.infer Inferencer.Enable
  |> Result.get_ok
  |> Closure.closure
  |> Lambdalift.lambda_lift
  |> Anfconv.anf
  |> fun anfstatements -> Format.printf "%a" pp_anfstatements anfstatements
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
        let #binop4 = (x + 1) in
        let #binop5 = (#binop4 + 3) in #binop5 else
        let #binop2 = (y + 4) in
        let #binop3 = (#binop2 + 5) in #binop3
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
        let #app1 = (fact n 1) in #app1
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
        let #binop1 = (1 + 2) in
        let #binop2 = (3 + 1) in (#binop2, #binop1) |}]
;;

(* Тут в тесте порядок элементов кортежа нарущен, нужно понять проблема в анф'е или в принтере.
   Посмотрев тест можно понять что все же проблемы не с анф, а с принтером кортежей :) *)
let%expect_test _ =
  let _ =
    let e = "let ((x, s), y) = ((1 ,(2 - 4)), 3 + 1)" in
    run_anf_tests e
  in
  [%expect
    {|
    let #tuple_out1 =
        let #binop1 = (2 - 4) in
        let #binop2 = (3 + 1) in (#binop2, (#binop1, 1));
    let x =
        let #take1 = take(#tuple_out1, 0) in
        let #take2 = take(#take1, 0) in #take2;
    let s =
        let #take1 = take(#tuple_out1, 0) in
        let #take2 = take(#take1, 1) in #take2;
    let y =
        let #take1 = take(#tuple_out1, 1) in #take1 |}]
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
        let #binop1 = (a + b) in #binop1;
    let x =
        let #make_closure1 = make_closure(apply2, sum 3 4) in
        let #make_closure2 = make_closure(apply2, sum 1 2) in (#make_closure2, #make_closure1)
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
        let #app1 = (sum 1 2 3 4 5 6) in #app1
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
        let #make_closure1 = make_closure(sum6, a b c d) in #make_closure1;
    let sum2 a b =
        let #make_closure1 = make_closure(sum4, a b) in #make_closure1;
    let rer =
        let #app1 = (sum2 1 2 3 4 5 6) in #app1
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
        let #take5 = take(#tuple_arg1, 0) in
        let #take6 = take(#take5, 2) in
        let #take7 = take(#take6, 0) in
        let d = #take7 in
        let #take8 = take(#tuple_arg1, 0) in
        let #take9 = take(#take8, 2) in
        let #take10 = take(#take9, 1) in
        let e = #take10 in
        let #binop11 = (a + b) in
        let #binop12 = (#binop11 + d) in
        let #binop13 = (#binop12 + e) in #binop13
 |}]
;;
