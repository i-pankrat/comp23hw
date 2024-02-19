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

(** Tests for tuples *)

let%expect_test _ =
  let _ =
    let e = {|
     let (a, b) = (1, 2)
  |} in
    run_lambda_test e
  in
  [%expect
    {|
    let #tuple_out1  = (1, 2);
    let a  = take(#tuple_out1, 0);
    let b  = take(#tuple_out1, 1)
 |}]
;;

let%expect_test _ =
  let _ =
    let e = {|
     let (_, _) = (1, 2)
  |} in
    run_lambda_test e
  in
  [%expect {| let #tuple_out1  = (1, 2) |}]
;;

let%expect_test _ =
  let _ =
    let e = {|
     let make_tuple a b = (a, b)
     let (f, s) = make_tuple 1 2
  |} in
    run_lambda_test e
  in
  [%expect
    {|
    let make_tuple a b = (a, b);
    let #tuple_out1  = make_tuple 1 2;
    let f  = take(#tuple_out1, 0);
    let s  = take(#tuple_out1, 1)
 |}]
;;

let%expect_test _ =
  let _ =
    let e = {|
     let make_tuple a b = (a, b)
     let (f, s) = make_tuple 1 2
  |} in
    run_lambda_test e
  in
  [%expect
    {|
    let make_tuple a b = (a, b);
    let #tuple_out1  = make_tuple 1 2;
    let f  = take(#tuple_out1, 0);
    let s  = take(#tuple_out1, 1)
 |}]
;;

let%expect_test _ =
  let _ =
    let e =
      {|
     let make_tuple a b = (a, b)
     let (f, (s, _, x)) = make_tuple 1 (2, 4, 3) 
     |}
    in
    run_lambda_test e
  in
  [%expect
    {|
    let make_tuple a b = (a, b);
    let #tuple_out1  = make_tuple 1 (2, 4, 3);
    let f  = take(#tuple_out1, 0);
    let s  = take(take(#tuple_out1, 1), 0);
    let x  = take(take(#tuple_out1, 1), 2)
 |}]
;;

let%expect_test _ =
  let _ =
    let e =
      {|
    let make_tuple a b = (a, b)
    let x =
      let (f, (s, (k, l), x)) = make_tuple 1 (2, (4, 5), 3) in
      (f, s) 
      |}
    in
    run_lambda_test e
  in
  [%expect
    {|
    let make_tuple a b = (a, b);
    let x  =
        let #tuple_out1 = make_tuple 1 (2, (4, 5), 3) in
        let f = take(#tuple_out1, 0) in
        let s = take(take(#tuple_out1, 1), 0) in
        let k = take(take(take(#tuple_out1, 1), 1), 0) in
        let l = take(take(take(#tuple_out1, 1), 1), 1) in
        let x = take(take(take(#tuple_out1, 1), 1), 2) in (f, s)
 |}]
;;

let%expect_test _ =
  let _ =
    let e =
      {|
    let x (f, s, y) k (s, (m, d), u) l = ((f + s + y) / k) * ((s + m + d + u) / l)
      |}
    in
    run_lambda_test e
  in
  [%expect
    {| let x f s y k s m d u l = ((((f + s) + y) / k) * ((((s + m) + d) + u) / l)) |}]
;;
