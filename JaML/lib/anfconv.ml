(** Copyright 2023-2024, Ilya Pankratov, Maxim Drumov *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open Anf
open Toplevel
open Ast
open Base

(* Simply convert type from const to immexpr *)
let const_to_immexpr = function
  | CInt i -> ImmNum i
  | CBool b -> ImmBool b
;;

(* Maps binary operations from ast.ml to binary operations from anf.ml*)
let binop_to_cexpr_constr op e1 e2 =
  match op with
  | Add -> CPlus (e1, e2)
  | Sub -> CMinus (e1, e2)
  | Div -> CDivide (e1, e2)
  | Mul -> CMultiply (e1, e2)
  | Xor -> CXor (e1, e2)
  | And -> CAnd (e1, e2)
  | Or -> COr (e1, e2)
  | Eq -> CEq (e1, e2)
  | Neq -> CNeq (e1, e2)
  | Gt -> CGt (e1, e2)
  | Lt -> CLt (e1, e2)
  | Gte -> CGte (e1, e2)
  | Lte -> CLte (e1, e2)
;;

(*
   Converts llexpr to aexpr
   Argument expr_with_hole helps to create anf tree in cps
*)
let anf (e : llexpr) (expr_with_hole : immexpr -> aexpr) =
  let open Counter.Counter in
  let fresh name = genid name in
  reset 0;
  let rec helper (e : llexpr) (expr_with_hole : immexpr -> aexpr) =
    match e with
    | LConst (const, _) -> expr_with_hole (const_to_immexpr const)
    | LVar (name, _) -> expr_with_hole (ImmId name)
    | LBinop ((op, _), e1, e2) ->
      helper e1 (fun limm ->
        helper e2 (fun rimm ->
          let new_name = fresh "#binop" in
          let op = binop_to_cexpr_constr op in
          ALet (new_name, op limm rimm, expr_with_hole (ImmId new_name))))
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
              let new_name = fresh "#make_closure" in
              ALet
                ( new_name
                , CMakeClosure (ImmId var, max_args, applied_args, left_args)
                , expr_with_hole @@ ImmId new_name )
            | _ ->
              let new_name = fresh "#app" in
              ALet
                (new_name, CApp (ImmId var, left_args), expr_with_hole @@ ImmId new_name)
          in
          helper curr_args (count_args ty) expr_with_hole
        | _ ->
          failwith
            "Unreachable: After all transformations in closure converison and lambda \
             lifting LApp must contain a LVar as the left operand."
      in
      app_helper [] application
    | LLetIn ((name, _), e1, e2) ->
      helper e1 (fun immval -> ALet (name, CImmExpr immval, helper e2 expr_with_hole))
    | LIfThenElse (i, t, e, _) ->
      helper i (fun immif ->
        AIfThenElse
          ( CImmExpr immif
          , helper t (fun immthen -> expr_with_hole immthen)
          , helper e (fun immelse -> expr_with_hole immelse) ))
    | LTuple (elems, _) ->
      let new_name = fresh "#tuple" in
      let rec tuple_helper l = function
        | hd :: tl -> helper hd (fun imm -> tuple_helper (imm :: l) tl)
        | _ ->
          ALet
            (new_name, CImmExpr (ImmTuple (List.rev l)), expr_with_hole (ImmId new_name))
      in
      tuple_helper [] elems
    | LTake (lexpr, n) ->
      helper lexpr (fun imm ->
        let new_name = fresh "#take" in
        ALet (new_name, CTake (imm, n), expr_with_hole (ImmId new_name)))
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
    let args =
      List.map
        ~f:(function
          | name, _ -> name)
        args
    in
    constructor name args (anf expr (fun imm -> ACEexpr (CImmExpr imm)))
;;

(* Performs transformation from Toplevel.llstatements to Anf.anfstatements *)
let anf lstatements = List.map ~f:(fun lbinding -> anf_binding lbinding) lstatements
