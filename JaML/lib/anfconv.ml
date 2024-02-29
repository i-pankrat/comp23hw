(** Copyright 2023-2024, Ilya Pankratov, Maxim Drumov *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open Anf
open Toplevel
open Ast
open Base
open Monads.ResultStateMonad

(* Simply convert type from const to immexpr *)
let const_to_immexpr = function
  | CInt i -> ImmNum i
  | CBool b -> ImmBool b
;;

(*
   Converts llexpr to aexpr
   Argument expr_with_hole helps to create anf tree in cps
*)
let anf e expr_with_hole =
  let rec helper (e : llexpr) (expr_with_hole : immexpr -> (aexpr, string) t) =
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
      let count_args =
        let rec helper num = function
          | Ty.Arrow (_, r) -> helper (num + 1) r
          | _ -> num
        in
        helper 0
      in
      let rec app_helper curr_args = function
        | LApp (a, b, _) -> helper b (fun imm -> app_helper (imm :: curr_args) a)
        | LVar (var, ty) ->
          let helper left_args max_args expr_with_hole =
            let applied_args = List.length left_args in
            let diff = max_args - applied_args in
            match diff with
            | _ when diff > 0 ->
              let* new_name = fresh "#make_closure" in
              let* hole = expr_with_hole @@ ImmId new_name in
              return
                (ALet
                   ( new_name
                   , CMakeClosure (ImmId var, max_args, applied_args, List.rev left_args)
                   , hole ))
            | _ ->
              let* new_name = fresh "#app" in
              let* hole = expr_with_hole @@ ImmId new_name in
              return (ALet (new_name, CApp (ImmId var, left_args), hole))
          in
          helper curr_args (count_args ty) expr_with_hole
        | _ -> fail "Left opperand of application is not a variable"
      in
      app_helper [] application
    | LLetIn ((name, _), e1, e2) ->
      helper e1 (fun immval ->
        let* aexpr = helper e2 expr_with_hole in
        return (ALet (name, CImmExpr immval, aexpr)))
    | LIfThenElse (i, t, e, _) ->
      helper i (fun immif ->
        let* athen = helper t (fun immthen -> expr_with_hole immthen) in
        let* aelse = helper e (fun immelse -> expr_with_hole immelse) in
        return (AIfThenElse (CImmExpr immif, athen, aelse)))
    | LTuple (elems, _) ->
      let* new_name = fresh "#tuple" in
      let rec tuple_helper l = function
        | hd :: tl -> helper hd (fun imm -> tuple_helper (imm :: l) tl)
        | _ ->
          let* hole = expr_with_hole (ImmId new_name) in
          return (ALet (new_name, CImmExpr (ImmTuple (List.rev l)), hole))
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
let anf_binding = function
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
    let* aexpr = anf expr (fun imm -> return (ACEexpr (CImmExpr imm))) in
    return @@ constructor name args aexpr
;;

(* Performs transformation from Toplevel.llstatements to Anf.anfstatements *)
let anf lstatements =
  run @@ monad_map ~f:(fun lbinding -> anf_binding lbinding) lstatements
;;
