(** Copyright 2024, Ilya Pankratov, Maxim Drumov *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open Anf
open Stdlib.Format

let pp_name ppf = fprintf ppf "%s"
let space ppf depth = fprintf ppf "\n%*s" (4 * depth) ""

let pp_tuple ppf pp_el =
  fprintf ppf "(%a)" (pp_print_list ~pp_sep:(fun ppf _ -> fprintf ppf ", ") pp_el)
;;

let pp_args pp_el ppf =
  fprintf ppf "%a" (pp_print_list ~pp_sep:(fun ppf _ -> fprintf ppf " ") pp_el)
;;

let rec pp_immexpr ppf = function
  | ImmNum i -> fprintf ppf "%d" i
  | ImmBool b -> fprintf ppf "%B" b
  | ImmId s -> fprintf ppf "%s" s
  | ImmTuple elems -> pp_tuple ppf pp_immexpr elems
;;

let pp_cexpr ppf = function
  | CPlus (l, r) -> fprintf ppf "(%a + %a)" pp_immexpr l pp_immexpr r
  | CMinus (l, r) -> fprintf ppf "(%a - %a)" pp_immexpr l pp_immexpr r
  | CDivide (l, r) -> fprintf ppf "(%a / %a)" pp_immexpr l pp_immexpr r
  | CMultiply (l, r) -> fprintf ppf "(%a * %a)" pp_immexpr l pp_immexpr r
  | CXor (l, r) -> fprintf ppf "(%a ^ %a)" pp_immexpr l pp_immexpr r
  | CAnd (l, r) -> fprintf ppf "(%a && %a)" pp_immexpr l pp_immexpr r
  | COr (l, r) -> fprintf ppf "(%a || %a)" pp_immexpr l pp_immexpr r
  | CEq (l, r) -> fprintf ppf "(%a = %a)" pp_immexpr l pp_immexpr r
  | CNeq (l, r) -> fprintf ppf "(%a <> %a)" pp_immexpr l pp_immexpr r
  | CGt (l, r) -> fprintf ppf "(%a > %a)" pp_immexpr l pp_immexpr r
  | CLt (l, r) -> fprintf ppf "(%a < %a)" pp_immexpr l pp_immexpr r
  | CGte (l, r) -> fprintf ppf "(%a >= %a)" pp_immexpr l pp_immexpr r
  | CLte (l, r) -> fprintf ppf "(%a <= %a)" pp_immexpr l pp_immexpr r
  | CApp (func, args) -> fprintf ppf "(%a %a)" pp_immexpr func (pp_args pp_immexpr) args
  | CImmExpr imm -> fprintf ppf "%a" pp_immexpr imm
  | CTake (imm, n) -> fprintf ppf "take(%a, %i)" pp_immexpr imm n
  | CMakeClosure (imm, _, _, args) ->
    fprintf ppf "make_closure(%a, %a)" pp_immexpr imm (pp_args pp_immexpr) args
;;

let pp_aexpr =
  let rec helper tabs ppf =
    let helper = helper tabs in
    function
    | ALet (name, cexpr, aexpr) ->
      fprintf
        ppf
        "%alet %a = %a in %a"
        space
        tabs
        pp_name
        name
        pp_cexpr
        cexpr
        helper
        aexpr
    | AIfThenElse (if_aexpr, then_aexpr, else_aexpr) ->
      fprintf
        ppf
        "if %a then %a else %a"
        pp_cexpr
        if_aexpr
        helper
        then_aexpr
        helper
        else_aexpr
    | ACEexpr cexpr -> fprintf ppf "%a" pp_cexpr cexpr
  in
  helper 1
;;

let pp_args =
  pp_print_list
    ~pp_sep:(fun ppf _ -> fprintf ppf " ")
    (fun ppf binding -> (fun ppf -> fprintf ppf "%s") ppf binding)
;;

let pp_anfexpr ppf = function
  | AnfLetVar (name, aexpr) -> fprintf ppf "let %a = %a" pp_name name pp_aexpr aexpr
  | AnfLetFun (name, args, aexpr) ->
    fprintf ppf "let %a %a = %a" pp_name name pp_args args pp_aexpr aexpr
  | AnfLetRec (name, args, aexpr) ->
    fprintf ppf "let rec %a %a = %a" pp_name name pp_args args pp_aexpr aexpr
;;

let pp_anfstatements =
  pp_print_list
    ~pp_sep:(fun ppf _ -> fprintf ppf ";\n")
    (fun ppf binding -> pp_anfexpr ppf binding)
;;
