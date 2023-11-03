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
  (* Input:
     let sum x =
     let new_sum x = x + 1 in
     new_sum x

     Output:
     let new_sum x = x + 1

     let sum x = new_sum x
  *)
  let _ =
    let e =
      [ TLet
          ( "sum"
          , TFun
              ( Arg ("x", Prim Int)
              , TLetIn
                  ( "new_sum"
                  , TFun
                      ( Arg ("x", Prim Int)
                      , TBinop
                          ( Add
                          , TVar ("x", Prim Int)
                          , TConst (CInt 1, Prim Int)
                          , Arrow (Prim Int, Arrow (Prim Int, Prim Int)) )
                      , Arrow (Prim Int, Prim Int) )
                  , TApp
                      ( TVar ("new_sum", Arrow (Prim Int, Prim Int))
                      , TVar ("x", Prim Int)
                      , Prim Int )
                  , Arrow (Prim Int, Prim Int) )
              , Arrow (Prim Int, Prim Int) )
          , Arrow (Prim Int, Prim Int) )
      ]
    in
    Lambdalift.lambda_lift e |> pp_with_type_statements
  in
  [%expect
    {|
    (TLet(
        new_sum: (int -> int),
        (TFun: (int -> int) (
            (x: int),
            (Add: (int -> (int -> int)) (
                (x: int),
                (TConst((CInt 1): int))
            ))
        ))
    ));
    (TLet(
        sum: (int -> int),
        (TFun: (int -> int) (
            (x: int),
            (TApp: int (
                (new_sum: (int -> int)),
                (x: int)
            ))
        ))
    ))
 |}]
;;

let%expect_test _ =
  (* Input:
     let sum x =
     let var = 1 in
     let new_sum x var = var + x in
     new_sum x var

     Output:
     let new_sum x var = var + x

     let sum x =
     let var = 1 in
     new_sum x var
  *)
  let _ =
    let e =
      [ TLet
          ( "sum"
          , TFun
              ( Arg ("x", Prim Int)
              , TLetIn
                  ( "var"
                  , TConst (CInt 1, Prim Int)
                  , TLetIn
                      ( "new_sum"
                      , TFun
                          ( Arg ("x", Prim Int)
                          , TFun
                              ( Arg ("var", Prim Int)
                              , TBinop
                                  ( Add
                                  , TVar ("var", Prim Int)
                                  , TVar ("x", Prim Int)
                                  , Arrow (Prim Int, Arrow (Prim Int, Prim Int)) )
                              , Arrow (Prim Int, Prim Int) )
                          , Arrow (Prim Int, Arrow (Prim Int, Prim Int)) )
                      , TApp
                          ( TApp
                              ( TVar
                                  ("new_sum", Arrow (Prim Int, Arrow (Prim Int, Prim Int)))
                              , TVar ("x", Prim Int)
                              , Arrow (Prim Int, Prim Int) )
                          , TVar ("var", Prim Int)
                          , Prim Int )
                      , Arrow (Prim Int, Arrow (Prim Int, Prim Int)) )
                  , Prim Int )
              , Arrow (Prim Int, Prim Int) )
          , Arrow (Prim Int, Prim Int) )
      ]
    in
    Lambdalift.lambda_lift e |> pp_with_type_statements
  in
  [%expect
    {|
    (TLet(
        new_sum: (int -> (int -> int)),
        (TFun: (int -> (int -> int)) (
            (x: int),
            (TFun: (int -> int) (
                (var: int),
                (Add: (int -> (int -> int)) (
                    (var: int),
                    (x: int)
                ))
            ))
        ))
    ));
    (TLet(
        sum: (int -> int),
        (TFun: (int -> int) (
            (x: int),
            (TLetIn(
                var: int,
                (TConst((CInt 1): int)),
                (TApp: int (
                    (TApp: (int -> int) (
                        (new_sum: (int -> (int -> int))),
                        (x: int)
                    )),
                    (var: int)
                ))
            ))
        ))
    ))
 |}]
;;

let%expect_test _ =
  (* Input:
     let sum x =
     let new_x = x + 1 in
     let new_sum = new_x + 1 in
     new_sum

     Output:
     let sum x =
     let new_x = x + 1 in
     let new_sum = new_x + 1 in
     new_sum
  *)
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
    Lambdalift.lambda_lift e |> pp_with_type_statements
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
