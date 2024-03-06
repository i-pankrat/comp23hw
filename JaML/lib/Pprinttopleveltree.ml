(** Copyright 2024, Ilya Pankratov, Maxim Drumov *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open LL_ast
open Stdlib.Format

let space ppf depth = fprintf ppf "\n%*s" (4 * depth) ""
let pp_name ppf = fprintf ppf "%s"

let pp_tuple ppf pp_el =
  fprintf ppf "(%a)" (pp_print_list ~pp_sep:(fun ppf _ -> fprintf ppf ", ") pp_el)
;;

let pp_llexpt_wt =
  let rec helper tabs ppf =
    let helper = helper tabs in
    function
    | LConst (const, _) -> Ast.pp_const ppf const
    | LVar (var_name, _) -> fprintf ppf "%s" var_name
    | LTuple (elements, _) -> pp_tuple ppf (fun el -> helper el) elements
    | LBinop ((binop, _), ltexpr, rtexpr) ->
      fprintf ppf "(%a %a %a)" helper ltexpr Ast.pp_bin_op binop helper rtexpr
    | LApp (fun_texpr, arg_texpr, _) ->
      fprintf ppf "%a %a" helper fun_texpr helper arg_texpr
    | LIfThenElse (cond_texpr, then_texpr, else_texpr, _) ->
      fprintf
        ppf
        "%aif %a then %a else %a"
        space
        tabs
        helper
        cond_texpr
        helper
        then_texpr
        helper
        else_texpr
    | LLetIn ((name, _), body_texpt, in_texpr) ->
      fprintf
        ppf
        "%alet %a = %a in %a"
        space
        tabs
        pp_name
        name
        helper
        body_texpt
        helper
        in_texpr
    | LTake (llexpr, n) -> fprintf ppf "take(%a, %i)" helper llexpr n
  in
  helper 1
;;

let pp_args ppf = function
  | Used (name, _) -> fprintf ppf "%s" name
  | Unused _ -> fprintf ppf "_"
;;

let pp_args =
  pp_print_list ~pp_sep:(fun ppf _ -> fprintf ppf " ") (fun ppf arg -> pp_args ppf arg)
;;

let pp_llbinding_wt ppf = function
  | LLetRec ((name, _), args, body) ->
    fprintf ppf "let rec %a %a = %a" pp_name name pp_args args pp_llexpt_wt body
  | LLet ((name, _), args, body) ->
    fprintf ppf "let %a %a = %a" pp_name name pp_args args pp_llexpt_wt body
;;

let pp_llstatements_without_types =
  pp_print_list
    ~pp_sep:(fun ppf _ -> fprintf ppf ";\n")
    (fun ppf binding -> pp_llbinding_wt ppf binding)
;;
