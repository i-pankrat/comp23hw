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
  (* Input: let name x y = x + y *)
  let _ =
    let e =
      [ TLet
          ( "name"
          , TFun
              ( Arg ("x", Prim Int)
              , TFun
                  ( Arg ("y", Prim Int)
                  , TBinop
                      ( Add
                      , TVar ("x", Prim Int)
                      , TVar ("y", Prim Int)
                      , Arrow (Prim Int, Arrow (Prim Int, Prim Int)) )
                  , Arrow (Prim Int, Prim Int) )
              , Arrow (Prim Int, Arrow (Prim Int, Prim Int)) )
          , Arrow (Prim Int, Arrow (Prim Int, Prim Int)) )
      ]
    in
    Alpha.alpha e |> pp_with_type_statements
  in
  [%expect
    {|
    (TLet(
        name1: (int -> (int -> int)),
        (TFun: (int -> (int -> int)) (
            (x2: int),
            (TFun: (int -> int) (
                (y3: int),
                (Add: (int -> (int -> int)) (
                    (x2: int),
                    (y3: int)
                ))
            ))
        ))
    ))
 |}]
;;

let%expect_test _ =
  (* Input:
     let test x y =
     let x = x * x in
     let y = y * y in
     x * y

     Output:
     let test7 x8 y9 =
     let x10 = x8 * x8 in
     let y11 = y9 * y9 in
     x10 * y11*)
  let _ =
    let e =
      [ TLet
          ( "test"
          , TFun
              ( Arg ("x", Prim Int)
              , TFun
                  ( Arg ("y", Prim Int)
                  , TLetIn
                      ( "x"
                      , TBinop
                          ( Mul
                          , TVar ("x", Prim Int)
                          , TVar ("x", Prim Int)
                          , Arrow (Prim Int, Arrow (Prim Int, Prim Int)) )
                      , TLetIn
                          ( "y"
                          , TBinop
                              ( Mul
                              , TVar ("y", Prim Int)
                              , TVar ("y", Prim Int)
                              , Arrow (Prim Int, Arrow (Prim Int, Prim Int)) )
                          , TBinop
                              ( Mul
                              , TVar ("x", Prim Int)
                              , TVar ("y", Prim Int)
                              , Arrow (Prim Int, Arrow (Prim Int, Prim Int)) )
                          , Prim Int )
                      , Prim Int )
                  , Arrow (Prim Int, Prim Int) )
              , Arrow (Prim Int, Arrow (Prim Int, Prim Int)) )
          , Arrow (Prim Int, Arrow (Prim Int, Prim Int)) )
      ]
    in
    Alpha.alpha e |> pp_with_type_statements
  in
  [%expect
    {|
    (TLet(
        x1: (int -> int),
        (TFun: (int -> int) (
            (x62: int),
            (Mul: (int -> (int -> int)) (
                (x62: int),
                (x62: int)
            ))
        ))
    ))
 |}]
;;
