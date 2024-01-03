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
    let x  =
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
        let #app1 = (f x) in
        let #app2 = (#app1 1) in #app2
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
        let #app1 = (f 1) in
        let #app2 = (#app1 2) in if #app2 then
        let #binop5 = (x + 1) in
        let #binop6 = (#binop5 + 3) in #binop6 else
        let #binop3 = (y + 4) in
        let #binop4 = (#binop3 + 5) in #binop4
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
        let #binop2 = (n - 1) in
        let #app3 = (fact #binop2) in
        let #binop4 = (n * acc) in
        let #app5 = (#app3 #binop4) in #app5;
    let fac_tailrec n =
        let #app1 = (fact n) in
        let #app2 = (#app1 1) in #app2
 |}]
;;
