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

(** The function is used to get rid of tuples in let in constructors and in arguments.
    It traverses the list of patterns from the tuple and produces expr_with_hole with
    LetIn constructors of variables, with index reference. *)
let rec dispose_of_tuple_in pat_lst expr_with_hole take_constr =
  List.fold_left
    ~f:(fun (expr_with_hole, take_constr, deep_counter) ->
      function
      | TPVar (id, typ) ->
        ( (fun e2 ->
            expr_with_hole (LLetIn ((id, typ), LTake (take_constr, deep_counter), e2)))
        , take_constr
        , deep_counter + 1 )
      | TPTuple (pat_lst, _) ->
        dispose_of_tuple_in pat_lst expr_with_hole (LTake (take_constr, deep_counter))
      | _ -> expr_with_hole, take_constr, deep_counter + 1)
    ~init:(expr_with_hole, take_constr, 0)
    (List.rev pat_lst)
;;

(** The function is used to get rid of tuples in let constructors.
    It does almost the same thing as the function above, but gives
    a list of let constructors for declaring variables at the top level. *)
let rec dispose_of_tuple pat_lst lst take_constr =
  List.fold_left
    ~f:(fun (env, take_constr, deep_counter) ->
      function
      | TPVar (id, typ) ->
        ( LLet ((id, typ), [], LTake (take_constr, deep_counter)) :: env
        , take_constr
        , deep_counter + 1 )
      | TPTuple (pat_lst, _) ->
        dispose_of_tuple pat_lst env (LTake (take_constr, deep_counter))
      | _ -> env, take_constr, deep_counter + 1)
    ~init:(lst, take_constr, 0)
    (List.rev pat_lst)
;;

let rec get_args_let (known, expr_with_hole) = function
  | TFun (TPVar (id, ty), expr, _) ->
    get_args_let ((id, ty) :: known, expr_with_hole) expr
  | TFun (TPTuple (pat_lst, ty), expr, _) ->
    let new_id = genid "#tuple_arg" in
    let expr_with_hole, _, _ =
      dispose_of_tuple_in pat_lst expr_with_hole (LVar (new_id, ty))
    in
    get_args_let ((new_id, ty) :: known, expr_with_hole) expr
  | _ -> known, expr_with_hole
;;

let rec lambda_lift_expr env = function
  | TConst (c, ty) -> LConst (c, ty), env
  | TVar (x, ty) -> LVar (x, ty), env
  | TTuple (expr, ty) ->
    let expr, env =
      List.fold_left
        ~f:(fun (acc, env) e ->
          let e, env = lambda_lift_expr env e in
          e :: acc, env)
        ~init:([], env)
        (List.rev expr)
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
    let args, expr_with_pat_hole = get_args_let ([], fun x -> x) e1 in
    let e1, env = lambda_lift_expr env e1 in
    let e2, env = lambda_lift_expr env e2 in
    let expr, env =
      if List.is_empty args
      then LLetIn ((id, ty), e1, e2), env
      else e2, extend_env env id (LLet ((id, ty), List.rev args, expr_with_pat_hole e1))
    in
    expr, env
  | TLetIn (TPVar (id, ty), e1, e2) ->
    let args, expr_with_pat_hole = get_args_let ([], fun x -> x) e1 in
    let e1, env = lambda_lift_expr env e1 in
    let e2, env = lambda_lift_expr env e2 in
    let expr, env =
      if List.is_empty args
      then LLetIn ((id, ty), e1, e2), env
      else e2, extend_env env id (LLet ((id, ty), List.rev args, expr_with_pat_hole e1))
    in
    expr, env
  | TLetIn (TPTuple (pat_lst, ty), e1, e2) ->
    let e1, env = lambda_lift_expr env e1 in
    let e2, env = lambda_lift_expr env e2 in
    let new_id = genid "#tuple_out" in
    let expr_with_hole, _ =
      let expr_with_hole e2 = LLetIn ((new_id, ty), e1, e2) in
      let dispose_of_pattern pat_lst expr_with_hole counter =
        List.fold_left
          ~f:(fun (expr_with_hole, counter) ->
            function
            | TPVar (id, typ) ->
              ( (fun e2 ->
                  expr_with_hole
                    (LLetIn ((id, typ), LTake (LVar (new_id, ty), counter), e2)))
              , counter + 1 )
            | TPTuple (pat_lst, _) ->
              let expr_with_hole, _, _ =
                dispose_of_tuple_in
                  pat_lst
                  expr_with_hole
                  (LTake (LVar (new_id, ty), counter))
              in
              expr_with_hole, counter + 1
            | _ -> expr_with_hole, counter + 1)
          ~init:(expr_with_hole, counter)
          (List.rev pat_lst)
      in
      dispose_of_pattern pat_lst expr_with_hole 0
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
    let args, expr_with_pat_hole = get_args_let ([], fun x -> x) expr in
    let expr, env = lambda_lift_expr env expr in
    LLet ((id, ty), List.rev args, expr_with_pat_hole expr), env, []
  | TLet (TPTuple (pat_lst, ty), expr) ->
    let args, expr_with_pat_hole = get_args_let ([], fun x -> x) expr in
    let expr, env = lambda_lift_expr env expr in
    let new_id = genid "#tuple_out" in
    let lst, _ =
      let dispose_of_pattern pat_lst lst counter =
        List.fold_left
          ~f:(fun (env, counter) ->
            function
            | TPVar (id, typ) ->
              LLet ((id, typ), [], LTake (LVar (new_id, ty), counter)) :: env, counter + 1
            | TPTuple (pat_lst, _) ->
              let env, _, _ =
                dispose_of_tuple pat_lst env (LTake (LVar (new_id, ty), counter))
              in
              env, counter + 1
            | _ -> env, counter + 1)
          ~init:(lst, counter)
          (List.rev pat_lst)
      in
      dispose_of_pattern pat_lst [] 0
    in
    LLet ((new_id, ty), List.rev args, expr_with_pat_hole expr), env, lst
  | TLet (TPConst (_, ty), expr) | TLet (TPWildcard ty, expr) ->
    let expr, env = lambda_lift_expr env expr in
    let new_id = genid "#wildcard" in
    LLet ((new_id, ty), [], expr), env, []
  | TLetRec ((id, ty), expr) ->
    let args, expr_with_pat_hole = get_args_let ([], fun x -> x) expr in
    let expr, env = lambda_lift_expr env expr in
    LLetRec ((id, ty), List.rev args, expr_with_pat_hole expr), env, []
;;

let lambda_lift expr =
  reset 0;
  let empty = EnvM.empty in
  let stms =
    List.fold expr ~init:[] ~f:(fun stms el ->
      let stmt, env, lst = lambda_lift_bindings empty el in
      let mapped_env = List.map ~f:(fun (_, expr) -> expr) (EnvM.to_alist env) in
      (lst @ (stmt :: List.rev mapped_env)) @ stms)
  in
  List.rev stms
;;
