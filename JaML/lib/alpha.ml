(** Copyright 2023-2024, Ilya Pankratov, Maxim Drumov *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open Base
open Typedtree
open Counter.Counter

let find x env = Map.find_exn env x
let extend_env id env = Map.set ~key:id ~data:(genid id) env

let rec alpha_expr env = function
  | TVar (x, ty) -> TVar (find x env, ty)
  | TBinop (op, e1, e2, ty) -> TBinop (op, alpha_expr env e1, alpha_expr env e2, ty)
  | TFun (Arg (id, ty1), expr, ty2) ->
    let env = extend_env id env in
    TFun (Arg (find id env, ty1), alpha_expr env expr, ty2)
  | TApp (fst, scd, ty) -> TApp (alpha_expr env fst, alpha_expr env scd, ty)
  | TIfThenElse (cond, e1, e2, ty) ->
    TIfThenElse (alpha_expr env cond, alpha_expr env e1, alpha_expr env e2, ty)
  | TLetRecIn (id, e1, e2, ty) ->
    let new_env = extend_env id env in
    TLetRecIn (find id new_env, alpha_expr env e1, alpha_expr new_env e2, ty)
  | TLetIn (id, e1, e2, ty) ->
    let new_env = extend_env id env in
    TLetIn (find id new_env, alpha_expr env e1, alpha_expr new_env e2, ty)
  | other -> other
;;

let test x y =
  let x = x * x in
  let y = y * y in
  x * y
;;

let alpha_bindings env = function
  | TLet (id, expr, ty) ->
    let env = extend_env id env in
    TLet (find id env, alpha_expr env expr, ty), env
  | TLetRec (id, expr, ty) ->
    let env = extend_env id env in
    TLetRec (find id env, alpha_expr env expr, ty), env
;;

let alpha stms =
  reset 0;
  let empty = Map.empty (module Base.String) in
  let _, stms =
    List.fold stms ~init:(empty, []) ~f:(fun (env, stms) el ->
      let stmt, env = alpha_bindings env el in
      env, stmt :: stms)
  in
  List.rev stms
;;
