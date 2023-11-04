(** Copyright 2023-2024, Ilya Pankratov, Maxim Drumov *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open Base
open Typedtree
open Typetree

module TS = struct
  type t = string * Typetree.ty

  let compare ((n1, _) : t) ((n2, _) : t) = Base.Poly.compare n1 n2
end

(* Set constructed with type t = (id, ty).
   id is a string representation of variables or function names;
   ty is its type *)
module NameS = Stdlib.Set.Make (TS)

(* Environment Map
   key -- string name of function;
   value -- ((id, ty) list, ty, option constr):
   (id, ty) list -- list of arguments that we must add to create a closure in the function;
   ty -- the final modified function type to which the function should be cast;
   option constr -- used to store let in constructors created from anonymous functions.
   In the constructor we put expr which should be in the (in) part of let in *)
module EnvM = Base.Map.Poly

let find id env = EnvM.find_exn env id
let extend_env key data env = EnvM.set env ~key ~data

(* Search for free variables in the function body. *)
let rec free_variables texpr =
  let union = NameS.union in
  match texpr with
  | TVar (x, ty) -> NameS.singleton (x, ty)
  | TApp (_, expr, _) | TFun (_, expr, _) -> free_variables expr
  | TIfThenElse (cond, e1, e2, _) ->
    let fv_cond = free_variables cond in
    let fv_e1 = free_variables e1 in
    let fv_e2 = free_variables e2 in
    union (union fv_e1 fv_e2) fv_cond
  | TBinop (_, e1, e2, _) | TLetIn (_, e1, e2, _) | TLetRecIn (_, e1, e2, _) ->
    let fv_e1 = free_variables e1 in
    let fv_e2 = free_variables e2 in
    union fv_e1 fv_e2
  | TConst _ -> NameS.empty
;;

(* The function is designed to create a closure when declaring a function.
   Missing variables are added explicitly. *)
let put_diff_arg diff acc =
  List.fold diff ~init:acc ~f:(fun (expr, ty) (var, ty1) ->
    let ty2 = Arrow (ty1, ty) in
    TFun (Arg (var, ty1), expr, ty2), ty2)
;;

(* The function is designed to create a closure when declaring a function. *)
let put_diff_app diff acc =
  List.fold diff ~init:acc ~f:(fun (expr, expr_ty) (var, ty1) ->
    let ty2 = Arrow (ty1, expr_ty) in
    TApp (expr, TVar (var, ty1), ty2), ty2)
;;

(* Function for declaring anonymous functions in let in. *)
let create_closure_let (elm, env) diff =
  List.fold
    (EnvM.to_alist diff)
    ~init:(elm, env)
    ~f:(fun (elm, env) (key, (_, _, constr)) ->
      match constr with
      | Some constr -> constr elm, EnvM.remove env key
      | None -> elm, env)
;;

(* Function to find variables in function arguments and then store them in Set.
   Necessary to separate TFuns that refer to function arguments and anonymous functions. *)
let rec get_args_let env known = function
  | TFun (Arg (id, ty1), e2, ty2) ->
    let known' = NameS.add (id, ty1) known in
    let e2, known, env = get_args_let env known' e2 in
    TFun (Arg (id, ty1), e2, ty2), known, env
  | other -> closure_expr env known other

and closure_expr env known expr =
  match expr with
  | TVar (x, ty) as tvar ->
    let expr, _ =
      match find x env with
      | diff, ty1, _ -> put_diff_app diff (tvar, ty1)
      | (exception Stdlib.Not_found) | (exception Not_found_s _) -> tvar, ty
    in
    expr, known, env
  | TBinop (op, e1, e2, ty) ->
    let e1, known, env = closure_expr env known e1 in
    let e2, known, env = closure_expr env known e2 in
    TBinop (op, e1, e2, ty), known, env
  | TFun (arg, expr, ty2) ->
    let expr, known, env = get_args_let env known (TFun (arg, expr, ty2)) in
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
    let e1, known', env = get_args_let env known e1 in
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
    let e1, known', env = get_args_let env NameS.empty e1 in
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
    let expr, _, env = get_args_let env NameS.empty expr in
    TLet (id, expr, ty), env
  | TLetRec (id, expr, ty) ->
    let expr, _, env = get_args_let env (NameS.singleton (id, ty)) expr in
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
