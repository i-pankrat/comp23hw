(** Copyright 2023-2024, Ilya Pankratov, Maxim Drumov *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open Jaml_lib

let run_closure_test test_case =
  let open Pprinttypedtree in
  Parser.parse test_case
  |> Result.get_ok
  |> Inferencer.infer Inferencer.Enable
  |> Result.get_ok
  |> Closure.closure
  |> fun tstatements -> Format.printf "%a" pp_statements_without_types tstatements
;;

let%expect_test _ =
  let _ =
    let test = {|
     let sum x =
     let new_sum y = x + y in
     new_sum 5
    |} in
    run_closure_test test
  in
  [%expect
    {|
    let sum = fun x ->
        let new_sum = fun x -> fun y -> (x + y) in new_sum x 5
 |}]
;;

let%expect_test _ =
  let _ =
    let test = {|
     let x y =
     let z a = a (y + 1) in
     z (fun x -> x)
  |} in
    run_closure_test test
  in
  [%expect
    {|
    let x = fun y ->
        let #closure_fun1 = fun x -> x in
        let z = fun y -> fun a -> a (y + 1) in z y #closure_fun1
 |}]
;;

let%expect_test _ =
  let _ =
    let test =
      {|
     let fac n =
     let rec fack n k =
     if n <= 1 then k 1
     else fack (n-1) ((fun k n m -> k (m * n)) k n)
     in
     fack n (fun x -> x)
  |}
    in
    run_closure_test test
  in
  [%expect
    {|
    let fac = fun n ->
        let #closure_fun3 = fun x -> x in
        let #closure_fun2 = fun k -> fun n -> fun m -> k (m * n) in
        let rec fack = fun n -> fun k ->
        if (n <= 1) then k 1 else fack (n - 1) #closure_fun2 k n in fack n #closure_fun3
 |}]
;;

let%expect_test _ =
  let _ =
    let test =
      {|
     let sum x =
     let new_x = x + 1 in
     let new_sum = new_x + 1 in
     new_sum
  |}
    in
    run_closure_test test
  in
  [%expect
    {|
    let sum = fun x ->
        let new_x = (x + 1) in
        let new_sum = (new_x + 1) in new_sum
 |}]
;;
