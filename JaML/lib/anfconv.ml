(** Copyright 2023-2024, Ilya Pankratov, Maxim Drumov *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open Anf
open LL_ast
open Ast
open Base
open Monads.VariableNameGeneratorMonad

(*
   Some thoughts about anf conversion.

   Let a top-level declaration be called the following:
   let f ... = ....
   let f = ...
   In this case f is a top-level declaration.

   Let the following constructs be called any level declaration:
   let f ... = ...
   let f = ...
   let ... =
   let f = ... in

   In all such cases, f is a declaration at any level.

   All top-level declarations without arguments are considered functions, which
   require 0 arguments to be passed to call them.

   Working with functions adheres to the following rules:
   1. If a top-level declaration with more than zero arguments is returned from
   any level declaration, that top-level declaration is turned into a closure to
   which zero arguments are applied.
   2. If a top-level declaration with zero arguments is returned from any level declaration,
   then that top-level declaration is considered a function to which zero arguments should be
   applied, and it is immediately called, and then we return the result of that application
   from the declaration.
   4. If the top-level declaration represents a higher-order function, it expects any function
   passed to it to be represented by a closure.
   3. If we pass a top-level declaration to a top-level declaration, the caller must
   turn the top-level declaration into a closure to which zero arguments are applied.
   4. If fewer arguments than expected are applied to the top-level declaration, then
   a closure is created from that top-level declaration to which the passed number of
   arguments is applied.
   5. If exactly as many arguments are applied to the top-level declaration as it expects,
   then an application of all arguments to the top-level declaration is called.
   6. If more arguments are applied to the top-level declaration than it expects, we first
   call the application with the arguments that the top-level declaration expects, and then
   apply the arguments to the result using AddArgsToClosure, since we expect the result to
   return a closure.
*)

(* Simply convert type from const to immexpr *)
let const_to_immexpr = function
  | CInt i -> ImmNum i
  | CBool b -> ImmBool b
;;

module Env : sig
  type argsEnv

  val add : argsEnv -> string -> int -> argsEnv
  val get_opt : argsEnv -> string -> int option
  val get : argsEnv -> string -> int
  val empty : argsEnv
end = struct
  module E = Stdlib.Map.Make (String)

  type argsEnv = int E.t

  let add env f n_arg = E.add f n_arg env
  let get_opt env f = E.find_opt f env

  let get env f =
    match get_opt env f with
    | Some x -> x
    | _ -> 0
  ;;

  let empty = E.empty
end

(*
   Converts llexpr to aexpr
   Argument expr_with_hole helps to create anf tree in cps
*)

let is_top_declaration env = function
  | LVar (var, _) ->
    (match Env.get_opt env var with
     | Some args_n -> Some (var, args_n)
     | _ -> None)
  | _ -> None
;;

let check_cexpr_is_top_declaration env e immval =
  match is_top_declaration env e with
  | Some (var, 0) -> CImmExpr (ImmVariable var)
  | Some _ -> CMakeClosure (immval, [])
  | _ -> CImmExpr immval
;;

let check_aexpr_is_top_declaration env e aexpr =
  match is_top_declaration env e with
  | Some (var, 0) -> ACEexpr (CImmExpr (ImmVariable var))
  | Some (var, _) -> ACEexpr (CMakeClosure (ImmId var, []))
  | _ -> aexpr
;;

let check_immexpr_is_top_level_var env imm =
  match imm with
  | ImmId var ->
    (match Env.get_opt env var with
     | Some 0 -> ImmVariable var
     | _ -> imm)
  | _ -> imm
;;

let is_imm_top_declaration env = function
  | ImmId var ->
    (match Env.get_opt env var with
     | Some args_n -> Some (var, args_n)
     | _ -> None)
  | _ -> None
;;

let process_arg env i =
  match i with
  | ImmId var ->
    (match Env.get_opt env var with
     | Some 0 -> ImmVariable var
     | Some _ -> PassFunctionAsArgument var
     | _ -> i)
  | _ -> i
;;

let anf env e expr_with_hole =
  let rec helper (e : llexpr) (expr_with_hole : immexpr -> aexpr t) =
    match e with
    | LConst (const, _) -> expr_with_hole (const_to_immexpr const)
    | LVar (name, _) -> expr_with_hole (ImmId name)
    | LBinop ((op, _), e1, e2) ->
      helper e1 (fun limm ->
        helper e2 (fun rimm ->
          let* new_name = fresh "#binop" in
          let* hole = expr_with_hole @@ ImmId new_name in
          let limm = check_immexpr_is_top_level_var env limm in
          let rimm = check_immexpr_is_top_level_var env rimm in
          return (ALet (new_name, CBinOp (op, limm, rimm), hole))))
    | LApp _ as application ->
      let construct_app expr_with_hole imm args =
        let* new_name = fresh "#app" in
        let* hole = expr_with_hole (ImmId new_name) in
        return (ALet (new_name, CApp (imm, args), hole))
      in
      let construct_closure expr_with_hole imm args =
        let* new_name = fresh "#closure" in
        let* hole = expr_with_hole (ImmId new_name) in
        return (ALet (new_name, CMakeClosure (imm, args), hole))
      in
      let construct_add_args_to_closure expr_with_hole imm args =
        let* new_name = fresh "#closure" in
        let* hole = expr_with_hole (ImmId new_name) in
        return (ALet (new_name, CAddArgsToClosure (imm, args), hole))
      in
      let construct_app_add_args_to_closure expr_with_hole imm app_args cl_args =
        let app = CApp (imm, app_args) in
        let* new_app = fresh "#app" in
        let* new_closure = fresh "#closure" in
        let* hole = expr_with_hole (ImmId new_closure) in
        return
          (ALet
             ( new_app
             , app
             , ALet (new_closure, CAddArgsToClosure (ImmId new_app, cl_args), hole) ))
      in
      (* app_helper is used for collecting all arguments at the application *)
      let rec app_helper curr_args = function
        | LApp (a, b, _) -> helper b (fun imm -> app_helper (imm :: curr_args) a)
        | f ->
          helper f (fun imm ->
            let curr_args = List.map ~f:(process_arg env) curr_args in
            match is_imm_top_declaration env imm with
            | None ->
              (* Not top-level declaration. Expect that it's closure *)
              construct_add_args_to_closure expr_with_hole imm curr_args
            | Some (_, 0) ->
              (* It's top level declaration, that takes zero arguments.
                 We expect that the top level declaration is closure. *)
              construct_add_args_to_closure expr_with_hole imm curr_args
            | Some (_, n) ->
              if n == List.length curr_args
              then
                (* If we can apply all arguments to a top level declaration, we do it. *)
                construct_app expr_with_hole imm curr_args
              else if List.length curr_args < n
              then
                (* Top level declaration need more arguments. Create a closure for now.*)
                construct_closure expr_with_hole imm curr_args
              else (
                let app_args, closure_args = List.split_n curr_args n in
                (* Apply all arguments that we can to the top level declarations.
                   We expect that return value will be closure. So then we add all arguments to the closure.
                *)
                construct_app_add_args_to_closure expr_with_hole imm app_args closure_args))
      in
      app_helper [] application
    | LLetIn ((name, _), e1, e2) ->
      helper e1 (fun immval ->
        let* aexpr = helper e2 expr_with_hole in
        let cimmval = check_cexpr_is_top_declaration env e1 immval in
        let aexpr = check_aexpr_is_top_declaration env e2 aexpr in
        return (ALet (name, cimmval, aexpr)))
    | LIfThenElse (i, t, e, _) ->
      helper i (fun immif ->
        let* athen = helper t (fun immthen -> return @@ ACEexpr (CImmExpr immthen)) in
        let athen = check_aexpr_is_top_declaration env t athen in
        let* aelse = helper e (fun immelse -> return @@ ACEexpr (CImmExpr immelse)) in
        let aelse = check_aexpr_is_top_declaration env e aelse in
        let* new_name = fresh "#if" in
        let* hole = expr_with_hole @@ ImmId new_name in
        return @@ ALet (new_name, CIfThenElse (immif, athen, aelse), hole))
    | LTuple (elems, _) ->
      let* new_name = fresh "#tuple" in
      let rec tuple_helper l = function
        | hd :: tl -> helper hd (fun imm -> tuple_helper (imm :: l) tl)
        | _ ->
          let* hole = expr_with_hole (ImmId new_name) in
          return (ALet (new_name, CTuple (List.rev l), hole))
      in
      tuple_helper [] elems
    | LTake (lexpr, n) ->
      helper lexpr (fun imm ->
        let* new_name = fresh "#take" in
        let* hole = expr_with_hole (ImmId new_name) in
        return (ALet (new_name, CTake (imm, n), hole)))
  in
  helper e expr_with_hole
;;

(* Performs transformation from llbinding to anfexpr *)
let anf_binding env = function
  | LLet ((name, _), args, expr) | LLetRec ((name, _), args, expr) ->
    let constructor name args aexpr =
      let aexpr = check_aexpr_is_top_declaration env expr aexpr in
      AnfLetFun (name, args, aexpr)
    in
    let args = List.map ~f:fst args in
    let env = Env.add env name (List.length args) in
    let* aexpr = anf env expr (fun imm -> return (ACEexpr (CImmExpr imm))) in
    return @@ (env, constructor name args aexpr)
;;

(* Performs transformation from Toplevel.llstatements to Anf.anfstatements *)
let anf lstatements =
  List.rev
  @@ snd
  @@ run
  @@ monad_fold
       ~init:(Env.empty, [])
       ~f:(fun (env, stmts) lbinding ->
         let* env, stmt = anf_binding env lbinding in
         return @@ (env, stmt :: stmts))
       lstatements
;;
