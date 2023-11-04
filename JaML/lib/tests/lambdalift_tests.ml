(** Copyright 2023-2024, Ilya Pankratov, Maxim Drumov *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open Jaml_lib.Lambdalift
open Jaml_lib.Typedtree
open Jaml_lib.Toplevel

let print_llstatements program = Format.printf "%s" (show_llstatements program)

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
    lambda_lift e |> print_llstatements
  in
  [%expect
    {|
    [(LLet ("new_sum", [(Arg ("x", (Prim int)))],
        (LBinop (Add, (LVar ("x", (Prim int))), (LConst ((CInt 1), (Prim int))),
           (Arrow ((Prim int), (Arrow ((Prim int), (Prim int))))))),
        (Arrow ((Prim int), (Prim int)))));
      (LLet ("sum", [(Arg ("x", (Prim int)))],
         (LApp ((LVar ("new_sum", (Arrow ((Prim int), (Prim int))))),
            (LVar ("x", (Prim int))), (Prim int))),
         (Arrow ((Prim int), (Prim int)))))
      ]
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
    lambda_lift e |> print_llstatements
  in
  [%expect
    {|
    [(LLet ("new_sum", [(Arg ("x", (Prim int))); (Arg ("var", (Prim int)))],
        (LBinop (Add, (LVar ("var", (Prim int))), (LVar ("x", (Prim int))),
           (Arrow ((Prim int), (Arrow ((Prim int), (Prim int))))))),
        (Arrow ((Prim int), (Arrow ((Prim int), (Prim int)))))));
      (LLet ("sum", [(Arg ("x", (Prim int)))],
         (LLetIn ("var", (LConst ((CInt 1), (Prim int))),
            (LApp (
               (LApp (
                  (LVar ("new_sum",
                     (Arrow ((Prim int), (Arrow ((Prim int), (Prim int))))))),
                  (LVar ("x", (Prim int))), (Arrow ((Prim int), (Prim int))))),
               (LVar ("var", (Prim int))), (Prim int))),
            (Prim int))),
         (Arrow ((Prim int), (Prim int)))))
      ]
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
    lambda_lift e |> print_llstatements
  in
  [%expect
    {|
    [(LLet ("sum", [(Arg ("x", (Prim int)))],
        (LLetIn ("new_x",
           (LBinop (Add, (LVar ("x", (Prim int))),
              (LConst ((CInt 1), (Prim int))),
              (Arrow ((Prim int), (Arrow ((Prim int), (Prim int))))))),
           (LLetIn ("new_sum",
              (LBinop (Add, (LVar ("new_x", (Prim int))),
                 (LConst ((CInt 1), (Prim int))),
                 (Arrow ((Prim int), (Arrow ((Prim int), (Prim int))))))),
              (LVar ("new_sum", (Prim int))), (Prim int))),
           (Prim int))),
        (Arrow ((Prim int), (Prim int)))))
      ]
 |}]
;;
