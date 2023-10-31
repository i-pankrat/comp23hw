(** Copyright 2023-2024, Ilya Pankratov, Maxim Drumov *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open Base
open Typedtree
open Counter

let find x env =
  match Map.find env x with
  | Some x -> x
  | None -> x
;;

let extend_env id env = Map.set ~key:id ~data:(Counter.genid id) env

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
    let env = extend_env id env in
    TLetRecIn (find id env, alpha_expr env e1, alpha_expr env e2, ty)
  | TLetIn (id, e1, e2, ty) ->
    let env = extend_env id env in
    TLetIn (find id env, alpha_expr env e1, alpha_expr env e2, ty)
  | other -> other
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
  let empty = Map.empty (module Base.String) in
  let _, stms =
    List.fold stms ~init:(empty, []) ~f:(fun (env, stms) el ->
      let stmt, env = alpha_bindings env el in
      env, stmt :: stms)
  in
  List.rev stms
;;

let run_alpha_statements =
  let open Pprinttypedtree in
  function
  | te ->
    let pp_statements = pp_statements ";\n" Complete in
    Stdlib.Format.printf "%a%!" pp_statements te
;;

let%expect_test _ =
  let _ =
    let e = [ TLet ("test", TConst (CInt 1, Prim Int), Prim Int) ] in
    alpha e |> run_alpha_statements
  in
  [%expect {|
    (TLet(
        test3: int,
        (TConst((CInt 1): int))
    ))
 |}]
;;
