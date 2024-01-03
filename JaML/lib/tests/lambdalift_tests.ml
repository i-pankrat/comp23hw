(** Copyright 2023-2024, Ilya Pankratov, Maxim Drumov *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open Jaml_lib
open Jaml_lib.Lambdalift

let run_lambda_test test_case =
  Parser.parse test_case
  |> Result.get_ok
  |> Inferencer.infer Inferencer.Enable
  |> Result.get_ok
  |> Closure.closure
  |> lambda_lift
  |> fun llstatements ->
  Format.printf
    "%a"
    Jaml_lib.Pprinttopleveltree.pp_llstatements_without_types
    llstatements
;;

let%expect_test _ =
  let _ =
    let e =
      {|
    let test x =
        let id1 y = y in
        let id2 z = id1 z in
        let id3 w = id2 w in
        let id4 u = id3 u in
        id4 x
     |}
    in
    run_lambda_test e
  in
  [%expect
    {|
    let id1 y = y;
    let id2 id1 z = id1 z;
    let id3 id2 w = id2 id1 w;
    let id4 id3 u = id3 id2 u;
    let test x = id4 id3 x
 |}]
;;

let%expect_test _ =
  let _ =
    let e = {|
     let sum x =
     let new_sum x = x + 1 in
     new_sum x
  |} in
    run_lambda_test e
  in
  [%expect {|
    let new_sum x = (x + 1);
    let sum x = new_sum x
 |}]
;;

let%expect_test _ =
  let _ =
    let e =
      {|
     let sum x =
     let var = 1 in
     let new_sum x var = var + x in
     new_sum x var
  |}
    in
    run_lambda_test e
  in
  [%expect
    {|
    let new_sum x var = (var + x);
    let sum x =
        let var = 1 in new_sum x var
 |}]
;;

let%expect_test _ =
  let _ =
    let e =
      {|
     let sum x =
     let new_x = x + 1 in
     let new_sum = new_x + 1 in
     new_sum
  |}
    in
    run_lambda_test e
  in
  [%expect
    {|
    let sum x =
        let new_x = (x + 1) in
        let new_sum = (new_x + 1) in new_sum
 |}]
;;
