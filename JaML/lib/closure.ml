(** Copyright 2023-2024, Ilya Pankratov, Maxim Drumov *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open Base
open Typedtree
open Ty
open Monads.VariableNameGeneratorMonad

(* Set constructed with type t = (id, ty).
   id is a string representation of variables or function names;
   ty is its type *)
module TS = struct
  type t = string * Ty.ty

  let compare ((n1, _) : t) ((n2, _) : t) = Base.Poly.compare n1 n2
end

module NameS = Stdlib.Set.Make (TS)

(* Environment Map
   key -- string name of function;
   value -- ((id, ty) list, ty, option constr):
   (id, ty) list -- list of arguments that we must add to create a closure in the function;
   ty -- the final modified function type to which the function should be cast;
   option constr -- used to store let in constructors created from anonymous functions.
   In the constructor we put expr which should be in the (in) part of let in *)
type map_type =
  (String.t, (String.t * Ty.ty) list * Ty.ty * (texpr -> texpr) option) Map.Poly.t

module EnvM = Base.Map.Poly

let extend_env
  (key : string)
  (data : (string * Ty.ty) list * Ty.ty * (texpr -> texpr) option)
  (env : map_type)
  =
  EnvM.set env ~key ~data
;;

let find id env = EnvM.find_exn env id

(* Function for obtaining a type related from a typed ast *)
let get_ty = function
  | TConst (_, ty)
  | TVar (_, ty)
  | TTuple (_, ty)
  | TFun (_, _, ty)
  | TLetRecIn ((_, ty), _, _)
  | TApp (_, _, ty)
  | TIfThenElse (_, _, _, ty)
  | TBinop ((_, ty), _, _)
  | TLetIn (TPVar (_, ty), _, _)
  | TLetIn (TPTuple (_, ty), _, _)
  | TLetIn (TPWildcard ty, _, _) -> ty
;;

(* Search for free variables in the function body. *)
let rec free_variables texpr =
  let union = NameS.union in
  let remove = NameS.remove in
  match texpr with
  | TVar (x, ty) -> NameS.singleton (x, ty)
  | TTuple (e, _) ->
    List.fold ~f:(fun x e -> union x (free_variables e)) ~init:NameS.empty e
  | TFun (pattern, expr, _) ->
    let rec helper_fv env = function
      | TPVar (x, ty) -> remove (x, ty) env
      | TPTuple (texpr, _) -> List.fold ~f:(fun fe e -> helper_fv fe e) ~init:env texpr
      | _ -> env
    in
    helper_fv (free_variables expr) pattern
  | TApp (e1, e2, _) ->
    let fv_e1 = free_variables e1 in
    let fv_e2 = free_variables e2 in
    union fv_e1 fv_e2
  | TIfThenElse (cond, e1, e2, _) ->
    let fv_cond = free_variables cond in
    let fv_e1 = free_variables e1 in
    let fv_e2 = free_variables e2 in
    union (union fv_e1 fv_e2) fv_cond
  | TBinop (_, e1, e2) ->
    let fv_e1 = free_variables e1 in
    let fv_e2 = free_variables e2 in
    union fv_e1 fv_e2
  | TConst _ | TLetIn _ | TLetRecIn _ -> NameS.empty
;;

(* The function is designed to create a closure when declaring a function.
   Missing variables are added explicitly. *)
let put_diff_arg diff acc =
  List.fold diff ~init:acc ~f:(fun (expr, ty) (var, ty1) ->
    let ty2 = Arrow (ty1, ty) in
    TFun (TPVar (var, ty1), expr, ty2), ty2)
;;

(* The function is designed to create a closure when declaring a function. *)
let put_diff_app diff acc =
  List.fold diff ~init:acc ~f:(fun (expr, expr_ty) (var, ty1) ->
    let ty2 = Arrow (ty1, expr_ty) in
    TApp (expr, TVar (var, ty1), ty2), ty2)
;;

(* Function for declaring anonymous functions in let in. *)
let rec create_closure_let (elm, env) diff =
  match elm with
  | TFun (pattern, expr, ty) ->
    let expr, env = create_closure_let (expr, env) diff in
    TFun (pattern, expr, ty), env
  | elm ->
    List.fold
      (List.rev @@ EnvM.to_alist diff)
      ~init:(elm, env)
      ~f:(fun (elm, env) (key, (_, _, constr)) ->
        match constr with
        | Some constr -> constr elm, EnvM.remove env key
        | None -> elm, env)
;;

(* Function to find variables in function arguments and then store them in Set.
   Necessary to separate TFuns that refer to function arguments and anonymous functions. *)
let rec get_args_let env known = function
  | TFun (TPVar (id, ty1), e2, ty2) ->
    let known' = NameS.add (id, ty1) known in
    let* fv_known, e2, known, env = get_args_let env known' e2 in
    return (fv_known, TFun (TPVar (id, ty1), e2, ty2), known, env)
  | TFun (TPTuple (texpr, ty), e2, ty2) ->
    let rec helper texpr =
      List.fold
        ~f:(fun acc ->
          function
          | TPVar (x, ty) -> NameS.add (x, ty) acc
          | TPTuple (x, _) -> helper x
          | _ -> acc)
        ~init:known
        texpr
    in
    let known' = helper texpr in
    let* fv_known, e2, known, env = get_args_let env known' e2 in
    return (fv_known, TFun (TPTuple (texpr, ty), e2, ty2), known, env)
  | TFun (tpattern, e2, ty2) ->
    let* fv_known, e2, known, env = get_args_let env known e2 in
    return (fv_known, TFun (tpattern, e2, ty2), known, env)
  | other ->
    let fv_known = free_variables other in
    let* expr, _, env = closure_expr env known other in
    return (fv_known, expr, known, env)

and closure_expr env known = function
  | TVar (x, ty) as tvar ->
    let expr, _ =
      match find x env with
      | diff, ty1, _ -> put_diff_app (List.rev diff) (tvar, ty1)
      | (exception Stdlib.Not_found) | (exception Not_found_s _) -> tvar, ty
    in
    return (expr, known, env)
  | TTuple (x, _) ->
    let* exprlst, ty, known, env =
      monad_fold
        ~f:(fun (exprlst, tylst, known, env) texpr ->
          let* expr, known, env = closure_expr env known texpr in
          return (expr :: exprlst, get_ty expr :: tylst, known, env))
        ~init:([], [], known, env)
        (List.rev x)
    in
    return (TTuple (exprlst, Tuple ty), known, env)
  | TBinop ((op, ty), e1, e2) ->
    let* e1, known, env = closure_expr env known e1 in
    let* e2, known, env = closure_expr env known e2 in
    return (TBinop ((op, ty), e1, e2), known, env)
  | TFun (tpattern, expr, ty2) ->
    let* fv_known, expr, known, env =
      get_args_let env NameS.empty (TFun (tpattern, expr, ty2))
    in
    let diff = NameS.diff fv_known known |> NameS.elements in
    let e1, ty = put_diff_arg diff (expr, ty2) in
    let* new_id = fresh "closure_fun" in
    let env =
      let constr_new_let e2 = TLetIn (TPVar (new_id, ty), e1, e2) in
      extend_env new_id (diff, ty, Some constr_new_let) env
    in
    let expr, _ = put_diff_app (List.rev diff) (TVar (new_id, ty), ty2) in
    return (expr, known, env)
  | TApp (fst, scd, ty) ->
    let* fst, known, env = closure_expr env known fst in
    let fst, ty =
      match fst with
      | TVar (x, ty) ->
        (match Map.find env x with
         | Some (free_var, new_ty, _) -> put_diff_app free_var (TVar (x, new_ty), ty)
         | None -> fst, ty)
      | _ -> fst, ty
    in
    let* scd, known, env = closure_expr env known scd in
    return (TApp (fst, scd, ty), known, env)
  | TIfThenElse (cond, e1, e2, ty) ->
    let* cond, known, env = closure_expr env known cond in
    let* e1, known, env = closure_expr env known e1 in
    let* e2, known, env = closure_expr env known e2 in
    return (TIfThenElse (cond, e1, e2, ty), known, env)
  | TLetRecIn ((id, ty), e1, e2) ->
    let known = NameS.singleton (id, ty) in
    let* fv_known, e1, known', env = get_args_let env known e1 in
    let diff =
      (if NameS.cardinal known' = 1 then known' else NameS.diff fv_known known')
      |> NameS.elements
    in
    let e1, ty = put_diff_arg diff (e1, ty) in
    let env = if List.is_empty diff then env else extend_env id (diff, ty, None) env in
    let* e2, known, env = closure_expr env known' e2 in
    return (TLetRecIn ((id, ty), e1, e2), known, env)
  | TLetIn (TPVar (id, ty), e1, e2) ->
    let* fv_known, e1, known', env = get_args_let env NameS.empty e1 in
    let diff =
      (if NameS.is_empty known' then known' else NameS.diff fv_known known')
      |> NameS.elements
    in
    let e1, ty = put_diff_arg diff (e1, ty) in
    let env = if List.is_empty diff then env else extend_env id (diff, ty, None) env in
    let* e2, known, env = closure_expr env known' e2 in
    return (TLetIn (TPVar (id, ty), e1, e2), known, env)
  | TLetIn (id, e1, e2) ->
    let* e1, known, env = closure_expr env known e1 in
    let* e2, known, env = closure_expr env known e2 in
    return (TLetIn (id, e1, e2), known, env)
  | other -> return (other, known, env)
;;

let closure_bindings = function
  | TLet (tpattern, expr) ->
    let* _, expr, _, env = get_args_let EnvM.empty NameS.empty expr in
    let expr, _ = create_closure_let (expr, env) env in
    return (TLet (tpattern, expr))
  | TLetRec ((id, ty), expr) ->
    let* _, expr, _, env = get_args_let EnvM.empty (NameS.singleton (id, ty)) expr in
    let expr, _ = create_closure_let (expr, env) env in
    return (TLetRec ((id, ty), expr))
;;

let closure expr =
  let stms =
    monad_fold expr ~init:[] ~f:(fun stms el ->
      let* stmt = closure_bindings el in
      return (stmt :: stms))
  in
  List.rev (run stms)
;;
