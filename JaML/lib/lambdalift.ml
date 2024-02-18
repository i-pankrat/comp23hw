(** Copyright 2023-2024, Ilya Pankratov, Maxim Drumov *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open Base
open Typedtree
open Toplevel
open Counter.Counter

(* Environment Map
   key -- string name of function;
   value -- tbinding:
   Stores, by function name, the function declaration that is to be placed outside the function afterwards.
*)
module EnvM = Base.Map.Poly

let extend_env env key data = EnvM.set env ~key ~data

let rec get_args_let known = function
  | TFun (TPVar (id, ty), expr, _) -> get_args_let ((id, ty) :: known) expr
  | TFun (TPTuple (pat_lst, _), expr, _) ->
    let rec args_collector pat init =
      List.fold_right
        ~f:(fun pat acc ->
          match pat with
          | TPVar (id, ty) -> (id, ty) :: acc
          | TPTuple (pat_lst, _) -> args_collector pat_lst acc
          | _ -> acc)
        ~init
        pat
    in
    let known = args_collector pat_lst known in
    get_args_let known expr
  | _ -> known
;;

let rec lambda_lift_expr env = function
  | TConst (c, ty) -> LConst (c, ty), env
  | TVar (x, ty) -> LVar (x, ty), env
  | TTuple (expr, ty) ->
    let expr, env =
      List.fold_right
        ~f:(fun e (acc, env) ->
          let e, env = lambda_lift_expr env e in
          e :: acc, env)
        ~init:([], env)
        expr
    in
    LTuple (expr, ty), env
  | TFun (_, expr, _) -> lambda_lift_expr env expr
  | TBinop ((op, ty), e1, e2) ->
    let e1, env = lambda_lift_expr env e1 in
    let e2, env = lambda_lift_expr env e2 in
    LBinop ((op, ty), e1, e2), env
  | TApp (fst, scd, ty) ->
    let fst, env = lambda_lift_expr env fst in
    let scd, env = lambda_lift_expr env scd in
    LApp (fst, scd, ty), env
  | TIfThenElse (cond, e1, e2, ty) ->
    let cond, env = lambda_lift_expr env cond in
    let e1, env = lambda_lift_expr env e1 in
    let e2, env = lambda_lift_expr env e2 in
    LIfThenElse (cond, e1, e2, ty), env
  | TLetRecIn ((id, ty), e1, e2) ->
    let args = List.rev @@ get_args_let [] e1 in
    let e1, env = lambda_lift_expr env e1 in
    let e2, env = lambda_lift_expr env e2 in
    let expr, env =
      if List.is_empty args
      then LLetIn ((id, ty), e1, e2), env
      else e2, extend_env env id (LLet ((id, ty), args, e1))
    in
    expr, env
  | TLetIn (TPVar (id, ty), e1, e2) ->
    let args = List.rev @@ get_args_let [] e1 in
    let e1, env = lambda_lift_expr env e1 in
    let e2, env = lambda_lift_expr env e2 in
    let expr, env =
      if List.is_empty args
      then LLetIn ((id, ty), e1, e2), env
      else e2, extend_env env id (LLet ((id, ty), args, e1))
    in
    expr, env
  | TLetIn (TPTuple (pat_lst, ty), e1, e2) ->
    let e1, env = lambda_lift_expr env e1 in
    let e2, env = lambda_lift_expr env e2 in
    let new_id = genid "#tuple_out" in
    let expr_with_hole, _ =
      let expr_with_hole e2 = LLetIn ((new_id, ty), e1, e2) in
      let rec dispose_of_patten pat_lst expr_with_hole counter =
        List.fold_right
          ~f:(fun pat (expr_with_hole, counter) ->
            match pat with
            | TPVar (id, typ) ->
              let new_expr e2 =
                expr_with_hole
                  (LLetIn ((id, typ), LTake (LVar (new_id, ty), counter), e2))
              in
              new_expr, counter + 1
            | TPTuple (pat_lst, _) -> dispose_of_patten pat_lst expr_with_hole counter
            | TPConst _ | TPWildcard _ -> expr_with_hole, counter + 1)
          ~init:(expr_with_hole, counter)
          pat_lst
      in
      dispose_of_patten pat_lst expr_with_hole 0
    in
    expr_with_hole e2, env
  | TLetIn (TPConst (_, ty), e1, e2) | TLetIn (TPWildcard ty, e1, e2) ->
    let e1, env = lambda_lift_expr env e1 in
    let e2, env = lambda_lift_expr env e2 in
    let new_id = genid "#wildcard" in
    LLetIn ((new_id, ty), e1, e2), env
;;

let lambda_lift_bindings env = function
  | TLet (TPVar (id, ty), expr) ->
    let args = List.rev @@ get_args_let [] expr in
    let expr, env = lambda_lift_expr env expr in
    LLet ((id, ty), args, expr), env, []
  | TLet (TPTuple (pat_lst, ty), expr) ->
    let args = List.rev @@ get_args_let [] expr in
    let expr, env = lambda_lift_expr env expr in
    let new_id = genid "#tuple_out" in
    let lst, _ =
      let rec dispose_of_patten pat_lst lst counter =
        List.fold_right
          ~f:(fun pat (env, counter) ->
            match pat with
            | TPVar (id, typ) ->
              let env = LLet ((id, typ), [], LTake (LVar (new_id, ty), counter)) :: env in
              env, counter + 1
            | TPTuple (pat_lst, _) -> dispose_of_patten pat_lst env counter
            | TPConst _ | TPWildcard _ -> env, counter + 1)
          ~init:(lst, counter)
          pat_lst
      in
      dispose_of_patten pat_lst [] 0
    in
    LLet ((new_id, ty), args, expr), env, lst
  | TLet (TPConst (_, ty), expr) | TLet (TPWildcard ty, expr) ->
    let expr, env = lambda_lift_expr env expr in
    let new_id = genid "#wildcard" in
    LLet ((new_id, ty), [], expr), env, []
  | TLetRec ((id, ty), expr) ->
    let args = List.rev @@ get_args_let [] expr in
    let expr, env = lambda_lift_expr env expr in
    LLetRec ((id, ty), args, expr), env, []
;;

let lambda_lift expr =
  let empty = EnvM.empty in
  let stms =
    List.fold expr ~init:[] ~f:(fun stms el ->
      let stmt, env, lst = lambda_lift_bindings empty el in
      let mapped_env = List.map ~f:(fun (_, expr) -> expr) (EnvM.to_alist env) in
      (lst @ (stmt :: List.rev mapped_env)) @ stms)
  in
  List.rev stms
;;
