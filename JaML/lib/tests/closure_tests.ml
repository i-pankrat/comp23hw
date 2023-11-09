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
  (* Input:
     let fac n =
     let rec fack n k =
     if n<=1 then k 1
     else fack (n−1) ((fun k n m −> k (m ∗ n)) k n)
     in
     fack n (fun x −> x)

     Output:
     let fac n =
     let #closure_fun2 k n m = k (m * n)
     let #closure_fun3 x = x in
     let rec fack n k =
     if n<=1 then k 1
     else fack (n−1) (#closure_fun2 k n)
     in
     fack n #closure_fun3
  *)
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
    (TLet(
        fac: (int -> int),
        (TFun: (int -> int) (
            (n: int),
            (TLetIn(
                #closure_fun2: ((int -> int) -> (int -> (int -> int))),
                (TFun: ((int -> int) -> (int -> (int -> int))) (
                    (k: (int -> int)),
                    (TFun: (int -> (int -> int)) (
                        (n: int),
                        (TFun: (int -> int) (
                            (m: int),
                            (TApp: (int -> int) (
                                (k: (int -> int)),
                                (Mul: (int -> (int -> int)) (
                                    (m: int),
                                    (n: int)
                                ))
                            ))
                        ))
                    ))
                )),
                (TLetIn(
                    #closure_fun3: (int -> int),
                    (TFun: (int -> int) (
                        (x: int),
                        (x: int)
                    )),
                    (TLetRecIn(
                        fack: (int -> ((int -> int) -> int)),
                        (TFun: (int -> ((int -> int) -> int)) (
                            (n: int),
                            (TFun: ((int -> int) -> int) (
                                (k: (int -> int)),
                                (TIfThenElse: int
                                    ((Lte: (int -> (int -> bool)) (
                                        (n: int),
                                        (TConst((CInt 1): int))
                                    )),
                                    (TApp: (int -> int) (
                                        (k: (int -> int)),
                                        (TConst((CInt 1): int))
                                    )),
                                    (TApp: int (
                                        (TApp: (int -> ((int -> int) -> int)) (
                                            (fack: (int -> ((int -> int) -> int))),
                                            (Sub: (int -> (int -> int)) (
                                                (n: int),
                                                (TConst((CInt 1): int))
                                            ))
                                        )),
                                        (TApp: (int -> int) (
                                            (TApp: ((int -> int) -> (int -> (int -> int))) (
                                                (#closure_fun2: ((int -> int) -> (int -> (int -> int)))),
                                                (k: (int -> int))
                                            )),
                                            (n: int)
                                        ))
                                    ))
                                ))
                            ))
                        )),
                        (TApp: int (
                            (TApp: (int -> ((int -> int) -> int)) (
                                (fack: (int -> ((int -> int) -> int))),
                                (n: int)
                            )),
                            (#closure_fun3: (int -> int))
                        ))
                    ))
                ))
            ))
        ))
    ))
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

let%expect_test _ =
  (* Input:
     let fibo n =
     let rec fibo_cps n acc =
     if n < 3
     then acc 1
     else fibo_cps (n - 1) (fun x -> fibo_cps (n - 2) (fun y -> acc (x + y)))
     in
     fibo_cps n (fun x -> x)

     Output:
     let fibo n =
     let #closure_fun4 x acc y = acc (x + y) in
     let #closure_fun5 n fibo_cps acc x = fibo_cps (n - 2) (#closure_fun4 x acc) in
     let #closure_fun6 x = x in
     let rec fibo_cps n acc =
     if n < 3
     then acc 1
     else fibo_cps (n - 1) #closure_fun5 n fibo_cps acc
     in
     fibo_cps n #closure_fun6
  *)
  let _ =
    let e =
      [ TLet
          ( "fibo"
          , TFun
              ( Arg ("n", Prim Int)
              , TLetRecIn
                  ( "fibo_cps"
                  , TFun
                      ( Arg ("n", Prim Int)
                      , TFun
                          ( Arg ("acc", Arrow (Prim Int, Prim Int))
                          , TIfThenElse
                              ( TBinop
                                  ( Lt
                                  , TVar ("n", Prim Int)
                                  , TConst (CInt 3, Prim Int)
                                  , Arrow (Prim Int, Arrow (Prim Int, Prim Bool)) )
                              , TApp
                                  ( TVar ("acc", Arrow (Prim Int, Prim Int))
                                  , TConst (CInt 1, Prim Int)
                                  , Prim Int )
                              , TApp
                                  ( TApp
                                      ( TVar
                                          ( "fibo_cps"
                                          , Arrow
                                              ( Prim Int
                                              , Arrow
                                                  (Arrow (Prim Int, Prim Int), Prim Int)
                                              ) )
                                      , TBinop
                                          ( Sub
                                          , TVar ("n", Prim Int)
                                          , TConst (CInt 1, Prim Int)
                                          , Arrow (Prim Int, Arrow (Prim Int, Prim Int))
                                          )
                                      , Arrow (Arrow (Prim Int, Prim Int), Prim Int) )
                                  , TFun
                                      ( Arg ("x", Prim Int)
                                      , TApp
                                          ( TApp
                                              ( TVar
                                                  ( "fibo_cps"
                                                  , Arrow
                                                      ( Prim Int
                                                      , Arrow
                                                          ( Arrow (Prim Int, Prim Int)
                                                          , Prim Int ) ) )
                                              , TBinop
                                                  ( Sub
                                                  , TVar ("n", Prim Int)
                                                  , TConst (CInt 2, Prim Int)
                                                  , Arrow
                                                      ( Prim Int
                                                      , Arrow (Prim Int, Prim Int) ) )
                                              , Arrow
                                                  (Arrow (Prim Int, Prim Int), Prim Int)
                                              )
                                          , TFun
                                              ( Arg ("y", Prim Int)
                                              , TApp
                                                  ( TVar
                                                      ("acc", Arrow (Prim Int, Prim Int))
                                                  , TBinop
                                                      ( Add
                                                      , TVar ("x", Prim Int)
                                                      , TVar ("y", Prim Int)
                                                      , Arrow
                                                          ( Prim Int
                                                          , Arrow (Prim Int, Prim Int) )
                                                      )
                                                  , Prim Int )
                                              , Arrow (Prim Int, Prim Int) )
                                          , Prim Int )
                                      , Arrow (Prim Int, Prim Int) )
                                  , Prim Int )
                              , Prim Int )
                          , Arrow (Arrow (Prim Int, Prim Int), Prim Int) )
                      , Arrow (Prim Int, Arrow (Arrow (Prim Int, Prim Int), Prim Int)) )
                  , TApp
                      ( TApp
                          ( TVar
                              ( "fibo_cps"
                              , Arrow
                                  (Prim Int, Arrow (Arrow (Prim Int, Prim Int), Prim Int))
                              )
                          , TVar ("n", Prim Int)
                          , Arrow (Arrow (Prim Int, Prim Int), Prim Int) )
                      , TFun
                          ( Arg ("x", Prim Int)
                          , TVar ("x", Prim Int)
                          , Arrow (Prim Int, Prim Int) )
                      , Prim Int )
                  , Arrow (Prim Int, Arrow (Arrow (Prim Int, Prim Int), Prim Int)) )
              , Arrow (Prim Int, Prim Int) )
          , Arrow (Prim Int, Prim Int) )
      ]
    in
    Closure.closure e |> pp_with_type_statements
  in
  [%expect
    {|
    (TLet(
        fibo: (int -> int),
        (TFun: (int -> int) (
            (n: int),
            (TLetIn(
                #closure_fun4: (int -> ((int -> int) -> (int -> int))),
                (TFun: (int -> ((int -> int) -> (int -> int))) (
                    (x: int),
                    (TFun: ((int -> int) -> (int -> int)) (
                        (acc: (int -> int)),
                        (TFun: (int -> int) (
                            (y: int),
                            (TApp: (int -> int) (
                                (acc: (int -> int)),
                                (Add: (int -> (int -> int)) (
                                    (x: int),
                                    (y: int)
                                ))
                            ))
                        ))
                    ))
                )),
                (TLetIn(
                    #closure_fun5: (int -> ((int -> ((int -> int) -> int)) -> ((int -> int) -> (int -> int)))),
                    (TFun: (int -> ((int -> ((int -> int) -> int)) -> ((int -> int) -> (int -> int)))) (
                        (n: int),
                        (TFun: ((int -> ((int -> int) -> int)) -> ((int -> int) -> (int -> int))) (
                            (fibo_cps: (int -> ((int -> int) -> int))),
                            (TFun: ((int -> int) -> (int -> int)) (
                                (acc: (int -> int)),
                                (TFun: (int -> int) (
                                    (x: int),
                                    (TApp: int (
                                        (TApp: (int -> ((int -> int) -> int)) (
                                            (fibo_cps: (int -> ((int -> int) -> int))),
                                            (Sub: (int -> (int -> int)) (
                                                (n: int),
                                                (TConst((CInt 2): int))
                                            ))
                                        )),
                                        (TApp: (int -> ((int -> int) -> (int -> int))) (
                                            (TApp: ((int -> int) -> (int -> int)) (
                                                (#closure_fun4: (int -> ((int -> int) -> (int -> int)))),
                                                (acc: (int -> int))
                                            )),
                                            (x: int)
                                        ))
                                    ))
                                ))
                            ))
                        ))
                    )),
                    (TLetIn(
                        #closure_fun6: (int -> int),
                        (TFun: (int -> int) (
                            (x: int),
                            (x: int)
                        )),
                        (TLetRecIn(
                            fibo_cps: (int -> ((int -> int) -> int)),
                            (TFun: (int -> ((int -> int) -> int)) (
                                (n: int),
                                (TFun: ((int -> int) -> int) (
                                    (acc: (int -> int)),
                                    (TIfThenElse: int
                                        ((Lt: (int -> (int -> bool)) (
                                            (n: int),
                                            (TConst((CInt 3): int))
                                        )),
                                        (TApp: (int -> int) (
                                            (acc: (int -> int)),
                                            (TConst((CInt 1): int))
                                        )),
                                        (TApp: int (
                                            (TApp: (int -> ((int -> int) -> int)) (
                                                (fibo_cps: (int -> ((int -> int) -> int))),
                                                (Sub: (int -> (int -> int)) (
                                                    (n: int),
                                                    (TConst((CInt 1): int))
                                                ))
                                            )),
                                            (TApp: (int -> ((int -> ((int -> int) -> int)) -> ((int -> int) -> (int -> int)))) (
                                                (TApp: ((int -> ((int -> int) -> int)) -> ((int -> int) -> (int -> int))) (
                                                    (TApp: ((int -> int) -> (int -> int)) (
                                                        (#closure_fun5: (int -> ((int -> ((int -> int) -> int)) -> ((int -> int) -> (int -> int))))),
                                                        (acc: (int -> int))
                                                    )),
                                                    (fibo_cps: (int -> ((int -> int) -> int)))
                                                )),
                                                (n: int)
                                            ))
                                        ))
                                    ))
                                ))
                            )),
                            (TApp: int (
                                (TApp: (int -> ((int -> int) -> int)) (
                                    (fibo_cps: (int -> ((int -> int) -> int))),
                                    (n: int)
                                )),
                                (#closure_fun6: (int -> int))
                            ))
                        ))
                    ))
                ))
            ))
        ))
    ))
 |}]
;;
