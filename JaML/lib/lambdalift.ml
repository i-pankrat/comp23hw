(** Copyright 2023-2024, Ilya Pankratov, Maxim Drumov *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open Base
open Typedtree

module EnvM = struct
  include Base.Map.Poly
end

let extend_env env key data = EnvM.set env ~key ~data

let rec get_args_let known = function
  | TFun (Arg (id, ty1), expr, ty2) ->
    let known = id :: known in
    let known, expr = get_args_let known expr in
    known, TFun (Arg (id, ty1), expr, ty2)
  | other -> known, other
;;

let rec lambda_lift_expr env = function
  | TVar (x, ty) -> TVar (x, ty), env
  | TBinop (op, e1, e2, ty) ->
    let e1, env = lambda_lift_expr env e1 in
    let e2, env = lambda_lift_expr env e2 in
    TBinop (op, e1, e2, ty), env
  | TFun (Arg (id, ty1), expr, ty2) ->
    let expr, env = lambda_lift_expr env expr in
    TFun (Arg (id, ty1), expr, ty2), env
  | TApp (fst, scd, ty) ->
    let fst, env = lambda_lift_expr env fst in
    let scd, env = lambda_lift_expr env scd in
    TApp (fst, scd, ty), env
  | TIfThenElse (cond, e1, e2, ty) ->
    let cond, env = lambda_lift_expr env cond in
    let e1, env = lambda_lift_expr env e1 in
    let e2, env = lambda_lift_expr env e2 in
    TIfThenElse (cond, e1, e2, ty), env
  | TLetRecIn (id, e1, e2, ty) ->
    let args, e1 = get_args_let [] e1 in
    let expr, env =
      if List.length args = 0
      then TLetRecIn (id, e1, e2, ty), env
      else (
        let env = extend_env env id (TLetRec (id, e1, ty)) in
        e2, env)
    in
    expr, env
  | TLetIn (id, e1, e2, ty) ->
    let args, e1 = get_args_let [] e1 in
    let expr, env =
      if List.length args = 0
      then (
        let e2, env = lambda_lift_expr env e2 in
        TLetIn (id, e1, e2, ty), env)
      else (
        let env = extend_env env id (TLet (id, e1, ty)) in
        let e2, env = lambda_lift_expr env e2 in
        e2, env)
    in
    expr, env
  | other -> other, env
;;

let lambda_lift_bindings env = function
  | TLet (id, expr, ty) ->
    let expr, env = lambda_lift_expr env expr in
    TLet (id, expr, ty), env
  | TLetRec (id, expr, ty) ->
    let expr, env = lambda_lift_expr env expr in
    TLetRec (id, expr, ty), env
;;

let lambda_lift expr =
  let empty = EnvM.empty in
  let _, stms =
    List.fold expr ~init:(empty, []) ~f:(fun (env, stms) el ->
      let stmt, env = lambda_lift_bindings env el in
      let env, stms =
        List.fold
          (List.rev @@ EnvM.to_alist env)
          ~init:(env, stms)
          ~f:(fun (env, stms) (key, stmt) -> EnvM.remove env key, stmt :: stms)
      in
      env, stmt :: stms)
  in
  List.rev stms
;;

let run_lambda_lift_statements =
  let open Pprinttypedtree in
  function
  | te ->
    let pp_statements = pp_statements ";\n" Complete in
    Stdlib.Format.printf "%a%!" pp_statements te
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
    lambda_lift e |> run_lambda_lift_statements
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
  let _ =
    let e =
      [ TLet
          ( "sum"
          , TFun
              ( Arg ("x", Prim Int)
              , TLetIn
                  ( "new_x"
                  , TFun
                      ( Arg ("x", Prim Int)
                      , TBinop
                          ( Add
                          , TVar ("x", Prim Int)
                          , TConst (CInt 1, Prim Int)
                          , Arrow (Prim Int, Arrow (Prim Int, Prim Int)) )
                      , Arrow (Prim Int, Prim Int) )
                  , TLetIn
                      ( "new_sum"
                      , TFun
                          ( Arg ("x", Prim Int)
                          , TFun
                              ( Arg ("y", Prim Int)
                              , TBinop
                                  ( Add
                                  , TApp
                                      ( TVar ("new_x", Arrow (Prim Int, Prim Int))
                                      , TVar ("x", Prim Int)
                                      , Prim Int )
                                  , TVar ("y", Prim Int)
                                  , Arrow (Prim Int, Arrow (Prim Int, Prim Int)) )
                              , Arrow (Prim Int, Prim Int) )
                          , Arrow (Prim Int, Arrow (Prim Int, Prim Int)) )
                      , TApp
                          ( TVar ("new_sum", Arrow (Prim Int, Arrow (Prim Int, Prim Int)))
                          , TVar ("x", Prim Int)
                          , Arrow (Prim Int, Prim Int) )
                      , Arrow (Prim Int, Arrow (Prim Int, Prim Int)) )
                  , Arrow (Prim Int, Prim Int) )
              , Arrow (Prim Int, Arrow (Prim Int, Prim Int)) )
          , Arrow (Prim Int, Arrow (Prim Int, Prim Int)) )
      ]
    in
    lambda_lift e |> run_lambda_lift_statements
  in
  [%expect
    {|
    (TLet(
        new_x: (int -> int),
        (TFun: (int -> int) (
            (x: int),
            (Add: (int -> (int -> int)) (
                (x: int),
                (TConst((CInt 1): int))
            ))
        ))
    ));
    (TLet(
        new_sum: (int -> (int -> int)),
        (TFun: (int -> (int -> int)) (
            (x: int),
            (TFun: (int -> int) (
                (y: int),
                (Add: (int -> (int -> int)) (
                    (TApp: int (
                        (new_x: (int -> int)),
                        (x: int)
                    )),
                    (y: int)
                ))
            ))
        ))
    ));
    (TLet(
        sum: (int -> (int -> int)),
        (TFun: (int -> (int -> int)) (
            (x: int),
            (TApp: (int -> int) (
                (new_sum: (int -> (int -> int))),
                (x: int)
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
    lambda_lift e |> run_lambda_lift_statements
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
