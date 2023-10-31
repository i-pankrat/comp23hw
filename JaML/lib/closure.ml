(** Copyright 2023-2024, Ilya Pankratov, Maxim Drumov *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open Base
open Typedtree
open Typetree

module TS = struct
  type t = string * Typetree.ty

  let compare ((n1, _) : t) ((n2, _) : t) = Base.Poly.compare n1 n2
end

module NameS = Stdlib.Set.Make (TS)

module EnvM = struct
  include Base.Map.Poly
end

let find id env = EnvM.find_exn env id
let extend_env key data env = EnvM.set env ~key ~data

let rec free_variables texpr =
  let union = NameS.union in
  match texpr with
  | TVar (x, ty) -> NameS.singleton (x, ty)
  | TApp (_, expr, _) | TFun (_, expr, _) ->
    let fv_expr = free_variables expr in
    fv_expr
  | TIfThenElse (cond, e1, e2, _) ->
    let fv_cond = free_variables cond in
    let fv_e1 = free_variables e1 in
    let fv_e2 = free_variables e2 in
    union (union fv_e1 fv_e2) fv_cond
  | TBinop (_, e1, e2, _) | TLetIn (_, e1, e2, _) | TLetRecIn (_, e1, e2, _) ->
    let fv_e1 = free_variables e1 in
    let fv_e2 = free_variables e2 in
    union fv_e1 fv_e2
  | _ -> NameS.empty
;;

let put_diff_arg diff acc =
  List.fold diff ~init:acc ~f:(fun (expr, ty) var ->
    let var, ty1 = var in
    let ty2 = Arrow (ty1, ty) in
    TFun (Arg (var, ty1), expr, ty2), ty2)
;;

let put_diff_app diff acc =
  List.fold_left diff ~init:acc ~f:(fun (expr, expr_ty) var ->
    let var, ty1 = var in
    let ty2 = Arrow (ty1, expr_ty) in
    TApp (expr, TVar (var, ty1), ty2), ty2)
;;

let create_closure_let (elm, env) diff =
  List.fold
    (EnvM.to_alist diff)
    ~init:(elm, env)
    ~f:(fun (elm, env) (key, (_, _, constr)) ->
      match constr with
      | Some constr ->
        let expr = constr elm in
        expr, EnvM.remove env key
      | None -> elm, env)
;;

let rec get_args_let env known expr =
  match expr with
  | TFun (Arg (id, ty1), e2, ty2) ->
    let known' = NameS.add (id, ty1) known in
    let env, known, e2 = get_args_let env known' e2 in
    env, known, TFun (Arg (id, ty1), e2, ty2)
  | other ->
    let expr, known, env = closure_expr env known other in
    env, known, expr

and closure_expr env known expr =
  match expr with
  | TVar (x, ty) ->
    let expr, _ =
      match find x env with
      | diff, ty1, _ -> put_diff_app diff (TVar (x, ty), ty1)
      | (exception Stdlib.Not_found) | (exception Not_found_s _) -> TVar (x, ty), ty
    in
    expr, known, env
  | TBinop (op, e1, e2, ty) ->
    let e1, known, env = closure_expr env known e1 in
    let e2, known, env = closure_expr env known e2 in
    TBinop (op, e1, e2, ty), known, env
  | TFun (arg, expr, ty2) ->
    let env, known, expr = get_args_let env known (TFun (arg, expr, ty2)) in
    let fv_known = free_variables expr in
    let diff = NameS.diff fv_known known |> NameS.elements in
    let e1, ty = put_diff_arg diff (expr, ty2) in
    let open Counter in
    let new_id = Counter.genid "closure_fun" in
    let env =
      let constr_new_let e2 = TLetIn (new_id, e1, e2, ty) in
      extend_env new_id (diff, ty, Some constr_new_let) env
    in
    let expr, _ = put_diff_app diff (TVar (new_id, ty), ty2) in
    expr, known, env
  | TApp (fst, scd, ty) ->
    let fst, known, env = closure_expr env known fst in
    let fst, ty =
      match fst with
      | TVar (x, ty) ->
        (match Map.find env x with
         | Some (free_var, new_ty, _) -> put_diff_app free_var (TVar (x, new_ty), ty)
         | None -> fst, ty)
      | _ -> fst, ty
    in
    let scd, known, env = closure_expr env known scd in
    TApp (fst, scd, ty), known, env
  | TIfThenElse (cond, e1, e2, ty) ->
    let cond, known, env = closure_expr env known cond in
    let e1, known, env = closure_expr env known e1 in
    let e2, known, env = closure_expr env known e2 in
    TIfThenElse (cond, e1, e2, ty), known, env
  | TLetRecIn (id, e1, e2, ty) ->
    let known = NameS.singleton (id, ty) in
    let env, known', e1 = get_args_let env known e1 in
    let fv_known = free_variables e1 in
    let diff =
      (if NameS.cardinal known' = 1 then known' else NameS.diff fv_known known')
      |> NameS.elements
    in
    let e1, ty = put_diff_arg diff (e1, ty) in
    let env = if List.length diff > 0 then extend_env id (diff, ty, None) env else env in
    let e2, known, env = closure_expr env known' e2 in
    let expr, env = create_closure_let (TLetRecIn (id, e1, e2, ty), env) env in
    expr, known, env
  | TLetIn (id, e1, e2, ty) ->
    let env, known', e1 = get_args_let env NameS.empty e1 in
    let fv_known = free_variables e1 in
    let diff =
      (if NameS.is_empty known' then known' else NameS.diff fv_known known')
      |> NameS.elements
    in
    let e1, ty = put_diff_arg diff (e1, ty) in
    let env = if List.length diff > 0 then extend_env id (diff, ty, None) env else env in
    let e2, known, env = closure_expr env known' e2 in
    let expr, env = create_closure_let (TLetIn (id, e1, e2, ty), env) env in
    expr, known, env
  | other -> other, known, env
;;

let closure_bindings env = function
  | TLet (id, expr, ty) ->
    let env, _, expr = get_args_let env NameS.empty expr in
    TLet (id, expr, ty), env
  | TLetRec (id, expr, ty) ->
    let env, _, expr = get_args_let env (NameS.singleton (id, ty)) expr in
    TLetRec (id, expr, ty), env
;;

let closure expr =
  let empty = EnvM.empty in
  let _, stms =
    List.fold expr ~init:(empty, []) ~f:(fun (env, stms) el ->
      let stmt, env = closure_bindings env el in
      env, stmt :: stms)
  in
  List.rev stms
;;

let run_closure_statements =
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
    closure e |> run_closure_statements
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
    closure e |> run_closure_statements
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
    closure e |> run_closure_statements
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
    closure e |> run_closure_statements
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
