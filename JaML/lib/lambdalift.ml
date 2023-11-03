(** Copyright 2023-2024, Ilya Pankratov, Maxim Drumov *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open Base
open Typedtree

(* Environment Map
   key -- string name of function;
   value -- tbinding:
   Stores, by function name, the function declaration that is to be placed outside the function afterwards.
*)
module EnvM = struct
  include Base.Map.Poly
end

let extend_env env key data = EnvM.set env ~key ~data

let rec get_args_let known = function
  | TFun (Arg (id, _), expr, _) -> get_args_let (id :: known) expr
  | _ -> known
;;

let rec lambda_lift_expr env = function
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
    let args = get_args_let [] e1 in
    let e1, env = lambda_lift_expr env e1 in
    let e2, env = lambda_lift_expr env e2 in
    let expr, env =
      if List.is_empty args
      then TLetRecIn (id, e1, e2, ty), env
      else e2, extend_env env id (TLetRec (id, e1, ty))
    in
    expr, env
  | TLetIn (id, e1, e2, ty) ->
    let args = get_args_let [] e1 in
    let e1, env = lambda_lift_expr env e1 in
    let e2, env = lambda_lift_expr env e2 in
    let expr, env =
      if List.is_empty args
      then TLetIn (id, e1, e2, ty), env
      else e2, extend_env env id (TLet (id, e1, ty))
    in
    expr, env
  | other -> other, env (* TConst, TVar *)
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
