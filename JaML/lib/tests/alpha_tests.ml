(** Copyright 2023-2024, Ilya Pankratov, Maxim Drumov *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open Jaml_lib

let run_alpha_test test_case =
  let open Pprinttypedtree in
  Parser.parse test_case
  |> Result.get_ok
  |> Inferencer.infer Inferencer.Enable
  |> Result.get_ok
  |> Alpha.alpha
  |> fun tstatements -> Format.printf "%a" pp_statements_without_types tstatements
;;

let%expect_test _ =
  let _ =
    let e = "let name x y = x + y" in
    run_alpha_test e
  in
  [%expect {| let name1 = fun x2 -> fun y3 -> (x2 + y3) |}]
;;

let%expect_test _ =
  let _ =
    let e =
      {|
     let test x y =
        let x = x * x in
        let y = y * y in
        x * y
    |}
    in
    run_alpha_test e
  in
  [%expect
    {|
    let test1 = fun x2 -> fun y3 ->
        let x4 = (x2 * x2) in
        let y5 = (y3 * y3) in (x4 * y5)
    |}]
;;
