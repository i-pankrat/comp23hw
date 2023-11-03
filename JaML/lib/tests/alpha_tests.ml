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
        name3: (int -> (int -> int)),
        (TFun: (int -> (int -> int)) (
            (x4: int),
            (TFun: (int -> int) (
                (y5: int),
                (Add: (int -> (int -> int)) (
                    (x4: int),
                    (y5: int)
                ))
            ))
        ))
    ))
 |}]
;;

let%expect_test _ =
  (* Input: let x x6 = x6 * x6 *)
  let _ =
    let e =
      [ TLet
          ( "x"
          , TFun
              ( Arg ("x6", Prim Int)
              , TBinop
                  ( Mul
                  , TVar ("x6", Prim Int)
                  , TVar ("x6", Prim Int)
                  , Arrow (Prim Int, Arrow (Prim Int, Prim Int)) )
              , Arrow (Prim Int, Prim Int) )
          , Arrow (Prim Int, Prim Int) )
      ]
    in
    Alpha.alpha e |> pp_with_type_statements
  in
  [%expect
    {|
    (TLet(
        x6: (int -> int),
        (TFun: (int -> int) (
            (x67: int),
            (Mul: (int -> (int -> int)) (
                (x67: int),
                (x67: int)
            ))
        ))
    ))
 |}]
;;
