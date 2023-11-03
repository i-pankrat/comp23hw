(** Copyright 2023-2024, Ilya Pankratov, Maxim Drumov *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open Jaml_lib
open Jaml_lib.Typedtree

let pp_with_type_statements te =
  let open Pprinttypedtree in
  let pp_statements = pp_statements ";\n" Complete in
  Format.printf "%a%!" pp_statements te
;;

let%expect_test _ =
  let _ =
    let e =
      [ TLet
          ( "sum"
          , TFun
              ( Arg ("x", Prim Int)
              , TLetIn
                  ( "new_sum"
                  , TFun
                      ( Arg ("y", Prim Int)
                      , TBinop
                          ( Add
                          , TVar ("x", Prim Int)
                          , TVar ("y", Prim Int)
                          , Arrow (Prim Int, Arrow (Prim Int, Prim Int)) )
                      , Arrow (Prim Int, Prim Int) )
                  , TApp
                      ( TVar ("new_sum", Arrow (Prim Int, Prim Int))
                      , TConst (CInt 5, Prim Int)
                      , Prim Int )
                  , Arrow (Prim Int, Prim Int) )
              , Arrow (Prim Int, Prim Int) )
          , Arrow (Prim Int, Prim Int) )
      ]
    in
    Closure.closure e |> pp_with_type_statements
  in
  [%expect
    {|
    (TLet(
        sum: (int -> int),
        (TFun: (int -> int) (
            (x: int),
            (TLetIn(
                new_sum: (int -> (int -> int)),
                (TFun: (int -> (int -> int)) (
                    (x: int),
                    (TFun: (int -> int) (
                        (y: int),
                        (Add: (int -> (int -> int)) (
                            (x: int),
                            (y: int)
                        ))
                    ))
                )),
                (TApp: int (
                    (TApp: (int -> (int -> (int -> int))) (
                        (new_sum: (int -> int)),
                        (x: int)
                    )),
                    (TConst((CInt 5): int))
                ))
            ))
        ))
    ))
 |}]
;;

let%expect_test _ =
  let _ =
    let e =
      [ TLet
          ( "sum"
          , TFun
              ( Arg ("x", Prim Int)
              , TLetIn
                  ( "new_sum"
                  , TFun
                      ( Arg ("y", Prim Int)
                      , TFun
                          ( Arg ("z", Tyvar 2)
                          , TFun
                              ( Arg ("c", Tyvar 3)
                              , TBinop
                                  ( Add
                                  , TVar ("x", Prim Int)
                                  , TVar ("y", Prim Int)
                                  , Arrow (Prim Int, Arrow (Prim Int, Prim Int)) )
                              , Arrow (Tyvar 3, Prim Int) )
                          , Arrow (Tyvar 2, Arrow (Tyvar 3, Prim Int)) )
                      , Arrow (Prim Int, Arrow (Tyvar 2, Arrow (Tyvar 3, Prim Int))) )
                  , TApp
                      ( TApp
                          ( TApp
                              ( TVar
                                  ( "new_sum"
                                  , Arrow
                                      ( Prim Int
                                      , Arrow (Prim Int, Arrow (Prim Int, Prim Int)) ) )
                              , TConst (CInt 5, Prim Int)
                              , Arrow (Prim Int, Arrow (Prim Int, Prim Int)) )
                          , TConst (CInt 4, Prim Int)
                          , Arrow (Prim Int, Prim Int) )
                      , TConst (CInt 3, Prim Int)
                      , Prim Int )
                  , Arrow (Prim Int, Arrow (Tyvar 2, Arrow (Tyvar 3, Prim Int))) )
              , Arrow (Prim Int, Prim Int) )
          , Arrow (Prim Int, Prim Int) )
      ]
    in
    Closure.closure e |> pp_with_type_statements
  in
  [%expect
    {|
    (TLet(
        sum: (int -> int),
        (TFun: (int -> int) (
            (x: int),
            (TLetIn(
                new_sum: (int -> (int -> ('a -> ('b -> int)))),
                (TFun: (int -> (int -> ('a -> ('b -> int)))) (
                    (x: int),
                    (TFun: (int -> ('a -> ('b -> int))) (
                        (y: int),
                        (TFun: ('a -> ('b -> int)) (
                            (z: 'a),
                            (TFun: ('b -> int) (
                                (c: 'b),
                                (Add: (int -> (int -> int)) (
                                    (x: int),
                                    (y: int)
                                ))
                            ))
                        ))
                    ))
                )),
                (TApp: int (
                    (TApp: (int -> int) (
                        (TApp: (int -> (int -> int)) (
                            (TApp: (int -> (int -> (int -> ('a -> ('b -> int))))) (
                                (new_sum: (int -> (int -> (int -> int)))),
                                (x: int)
                            )),
                            (TConst((CInt 5): int))
                        )),
                        (TConst((CInt 4): int))
                    )),
                    (TConst((CInt 3): int))
                ))
            ))
        ))
    ))
 |}]
;;

let%expect_test _ =
  let _ =
    let e =
      [ TLet
          ( "fac"
          , TFun
              ( Arg ("n", Prim Int)
              , TLetRecIn
                  ( "fack"
                  , TFun
                      ( Arg ("n", Prim Int)
                      , TFun
                          ( Arg ("k", Arrow (Prim Int, Prim Int))
                          , TIfThenElse
                              ( TBinop
                                  ( Lte
                                  , TVar ("n", Prim Int)
                                  , TConst (CInt 1, Prim Int)
                                  , Arrow (Prim Int, Arrow (Prim Int, Prim Bool)) )
                              , TApp
                                  ( TVar ("k", Arrow (Prim Int, Prim Int))
                                  , TConst (CInt 1, Prim Int)
                                  , Prim Int )
                              , TApp
                                  ( TApp
                                      ( TVar
                                          ( "fack"
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
                                  , TApp
                                      ( TApp
                                          ( TFun
                                              ( Arg ("k", Arrow (Prim Int, Prim Int))
                                              , TFun
                                                  ( Arg ("n", Prim Int)
                                                  , TFun
                                                      ( Arg ("m", Prim Int)
                                                      , TApp
                                                          ( TVar
                                                              ( "k"
                                                              , Arrow (Prim Int, Prim Int)
                                                              )
                                                          , TBinop
                                                              ( Mul
                                                              , TVar ("m", Prim Int)
                                                              , TVar ("n", Prim Int)
                                                              , Arrow
                                                                  ( Prim Int
                                                                  , Arrow
                                                                      (Prim Int, Prim Int)
                                                                  ) )
                                                          , Prim Int )
                                                      , Arrow (Prim Int, Prim Int) )
                                                  , Arrow
                                                      ( Prim Int
                                                      , Arrow (Prim Int, Prim Int) ) )
                                              , Arrow
                                                  ( Arrow (Prim Int, Prim Int)
                                                  , Arrow
                                                      ( Prim Int
                                                      , Arrow (Prim Int, Prim Int) ) ) )
                                          , TVar ("k", Arrow (Prim Int, Prim Int))
                                          , Arrow (Prim Int, Arrow (Prim Int, Prim Int))
                                          )
                                      , TVar ("n", Prim Int)
                                      , Arrow (Prim Int, Prim Int) )
                                  , Prim Int )
                              , Prim Int )
                          , Arrow (Arrow (Prim Int, Prim Int), Prim Int) )
                      , Arrow (Prim Int, Arrow (Arrow (Prim Int, Prim Int), Prim Int)) )
                  , TApp
                      ( TApp
                          ( TVar
                              ( "fack"
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
        fac: (int -> int),
        (TFun: (int -> int) (
            (n: int),
            (TLetIn(
                closure_fun2: (int -> int),
                (TFun: (int -> int) (
                    (x: int),
                    (x: int)
                )),
                (TLetIn(
                    closure_fun1: ((int -> int) -> (int -> (int -> int))),
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
                                                (closure_fun1: ((int -> int) -> (int -> (int -> int)))),
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
                            (closure_fun2: (int -> int))
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
    let e =
      [ TLet
          ( "sum"
          , TFun
              ( Arg ("x", Prim Int)
              , TLetIn
                  ( "new_x"
                  , TBinop
                      ( Add
                      , TVar ("x", Prim Int)
                      , TConst (CInt 1, Prim Int)
                      , Arrow (Prim Int, Arrow (Prim Int, Prim Int)) )
                  , TLetIn
                      ( "new_sum"
                      , TBinop
                          ( Add
                          , TVar ("new_x", Prim Int)
                          , TConst (CInt 1, Prim Int)
                          , Arrow (Prim Int, Arrow (Prim Int, Prim Int)) )
                      , TVar ("new_sum", Prim Int)
                      , Prim Int )
                  , Prim Int )
              , Arrow (Prim Int, Prim Int) )
          , Arrow (Prim Int, Prim Int) )
      ]
    in
    Closure.closure e |> pp_with_type_statements
  in
  [%expect
    {|
    (TLet(
        sum: (int -> int),
        (TFun: (int -> int) (
            (x: int),
            (TLetIn(
                new_x: int,
                (Add: (int -> (int -> int)) (
                    (x: int),
                    (TConst((CInt 1): int))
                )),
                (TLetIn(
                    new_sum: int,
                    (Add: (int -> (int -> int)) (
                        (new_x: int),
                        (TConst((CInt 1): int))
                    )),
                    (new_sum: int)
                ))
            ))
        ))
    ))
 |}]
;;
