(** Copyright 2023-2024, Ilya Pankratov, Maxim Drumov *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open Anf
open LL_ast
open Ast
open Base
open Monads.VariableNameGeneratorMonad

(* Simply convert type from const to immexpr *)
let const_to_immexpr = function
  | CInt i -> ImmNum i
  | CBool b -> ImmBool b
;;

module Env : sig
  type argsEnv

  val add : argsEnv -> string -> int -> argsEnv
  val get : argsEnv -> string -> int
  val empty : argsEnv
end = struct
  module E = Stdlib.Map.Make (String)

  type argsEnv = int E.t

  let add env f n_arg = E.add f n_arg env

  let get env f =
    match E.find_opt f env with
    | Some x -> x
    | _ -> 0
  ;;

  let empty = E.empty
end

(*
   Converts llexpr to aexpr
   Argument expr_with_hole helps to create anf tree in cps
*)
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
          return (ALet (new_name, CBinOp (op, limm, rimm), hole))))
    | LApp _ as application ->
      let construct_app expr_with_hole imm args =
        let* new_name = fresh "#app" in
        let* hole = expr_with_hole (ImmId new_name) in
        return (ALet (new_name, CApp (imm, args), hole))
      in
      let closure_expr_with_hole next_expr_with_hole args imm =
        let rec helper args imm =
          let* new_name = fresh "#closure" in
          match args with
          | [] ->
            let* hole = next_expr_with_hole (ImmId new_name) in
            return (ALet (new_name, CImmExpr imm, hole))
          | hd :: tl ->
            let* hole = helper tl (ImmId new_name) in
            return (ALet (new_name, CMakeClosure (imm, hd), hole))
        in
        helper args imm
      in
      let rec get_f_name = function
        | LConst _ | LTuple _ | LBinop _ | LApp _ | LIfThenElse _ | LTake _ -> ""
        | LLetIn (_, _, n) -> get_f_name n
        | LVar (name, _) -> name
      in
      let rec app_helper curr_args = function
        | LApp (a, b, _) -> helper b (fun imm -> app_helper (imm :: curr_args) a)
        | f ->
          helper f (fun imm ->
            let number_of_fun_args = Env.get env (get_f_name f) in
            if number_of_fun_args <> 0 && List.length curr_args >= number_of_fun_args
            then (
              let to_app, to_closure = List.split_n curr_args number_of_fun_args in
              construct_app
                (fun next_imm ->
                  closure_expr_with_hole expr_with_hole to_closure next_imm)
                imm
                to_app)
            else closure_expr_with_hole expr_with_hole curr_args imm)
      in
      app_helper [] application
    | LLetIn ((name, _), e1, e2) ->
      helper e1 (fun immval ->
        let* aexpr = helper e2 expr_with_hole in
        return (ALet (name, CImmExpr immval, aexpr)))
    | LIfThenElse (i, t, e, _) ->
      helper i (fun immif ->
        let* athen = helper t (fun immthen -> return @@ ACEexpr (CImmExpr immthen)) in
        let* aelse = helper e (fun immelse -> return @@ ACEexpr (CImmExpr immelse)) in
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
  | (LLet ((name, _), args, expr) | LLetRec ((name, _), args, expr)) as binding ->
    let binding_to_anf_expr = function
      | LLet _ ->
        fun name args aexpr ->
          if List.is_empty args
          then AnfLetVar (name, aexpr)
          else AnfLetFun (name, args, aexpr)
      | LLetRec _ -> fun name args aexpr -> AnfLetRec (name, args, aexpr)
    in
    let constructor = binding_to_anf_expr binding in
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
