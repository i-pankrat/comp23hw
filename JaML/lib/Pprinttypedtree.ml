(** Copyright 2023-2024, Ilya Pankratov, Maxim Drumov *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open Typedtree
open Base
open Stdlib.Format

type mode =
  | Brief
  | Complete

let get_texpr_subst =
  let rec helper subs index = function
    | TVar (_, typ) -> Pprintty.get_ty_subs subs index typ
    | TLetIn (_, e1, e2, typ)
    | TBinop (_, e1, e2, typ)
    | TApp (e1, e2, typ)
    | TLetRecIn (_, e1, e2, typ) ->
      let subs, index = Pprintty.get_ty_subs subs index typ in
      let subs, index = helper subs index e1 in
      helper subs index e2
    | TFun (_, e, typ) ->
      let subs, index = Pprintty.get_ty_subs subs index typ in
      helper subs index e
    | TIfThenElse (e1, e2, e3, typ) ->
      let subs, index = Pprintty.get_ty_subs subs index typ in
      let subs, index = helper subs index e1 in
      let subs, index = helper subs index e2 in
      helper subs index e3
    | TConst _ -> subs, index
    | TTuple (elems, typ) ->
      let subs, index = Pprintty.get_ty_subs subs index typ in
      List.fold
        ~init:(subs, index)
        ~f:(fun (subs, index) elem -> helper subs index elem)
        elems
  in
  helper (Base.Map.empty (module Base.Int)) 0
;;

let space ppf depth = fprintf ppf "\n%*s" (4 * depth) ""

let pp_arg subs ppf =
  let pp_ty = Pprintty.pp_ty_with_subs subs in
  function
  | Arg (name, typ) -> fprintf ppf "(%s: %a)" name pp_ty typ
;;

(* A monster that needs refactoring *)
let pp_texpr_with_subs ppf depth subs texpr =
  let pp_ty = Pprintty.pp_ty_with_subs (Some subs) in
  let pp_arg = pp_arg (Some subs) in
  let rec helper depth ppf =
    let pp = helper (depth + 1) in
    function
    | TVar (name, typ) -> fprintf ppf "(%s: %a)" name pp_ty typ
    | TBinop (op, e1, e2, typ) ->
      fprintf
        ppf
        "(%s: %a (%a%a, %a%a%a))"
        (Ast.show_bin_op op)
        pp_ty
        typ
        space
        depth
        pp
        e1
        space
        depth
        pp
        e2
        space
        (depth - 1)
    | TApp (e1, e2, typ) ->
      fprintf
        ppf
        "(TApp: %a (%a%a, %a%a%a))"
        pp_ty
        typ
        space
        depth
        pp
        e1
        space
        depth
        pp
        e2
        space
        (depth - 1)
    | TLetIn (name, e1, e2, typ) ->
      fprintf
        ppf
        "(TLetIn(%a%s: %a,%a%a,%a%a%a))"
        space
        depth
        name
        pp_ty
        typ
        space
        depth
        pp
        e1
        space
        depth
        pp
        e2
        space
        (depth - 1)
    | TLetRecIn (name, e1, e2, typ) ->
      fprintf
        ppf
        "(TLetRecIn(%a%s: %a,%a%a,%a%a%a))"
        space
        depth
        name
        pp_ty
        typ
        space
        depth
        pp
        e1
        space
        depth
        pp
        e2
        space
        (depth - 1)
    | TFun (arg, e, typ) ->
      fprintf
        ppf
        "(TFun: %a (%a%a, %a%a%a))"
        pp_ty
        typ
        space
        depth
        pp_arg
        arg
        space
        depth
        pp
        e
        space
        (depth - 1)
    | TIfThenElse (i, t, e, typ) ->
      fprintf
        ppf
        "(TIfThenElse: %a%a(%a, %a%a, %a%a%a))"
        pp_ty
        typ
        space
        depth
        pp
        i
        space
        depth
        pp
        t
        space
        depth
        pp
        e
        space
        (depth - 1)
    | TConst (c, typ) -> fprintf ppf "(TConst(%s: %a))" (Ast.show_const c) pp_ty typ
  in
  helper depth ppf texpr
;;

let pp_texpr ppf texpr =
  let subs, _ = get_texpr_subst texpr in
  pp_texpr_with_subs ppf 1 subs texpr
;;

let show_binding = function
  | TLetRec _ -> "TLetRec"
  | TLet _ -> "TLet"
;;

let pp_tbinding_complete ppf = function
  | (TLetRec (name, e, typ) | TLet (name, e, typ)) as binding ->
    let subs, _ = get_texpr_subst e in
    let pp_ty = Pprintty.pp_ty_with_subs (Some subs) in
    let pp_expr ppf = pp_texpr_with_subs ppf 2 subs in
    fprintf
      ppf
      "(%s(%a%s: %a, %a%a\n))"
      (show_binding binding)
      space
      1
      name
      pp_ty
      typ
      space
      1
      pp_expr
      e
;;

let pp_tbinding_brief ppf = function
  | TLetRec (name, _, typ) | TLet (name, _, typ) ->
    let pp_ty = Pprintty.pp_ty in
    fprintf ppf "%s: %a" name pp_ty typ
;;

let pp_tbinding = pp_tbinding_complete

let pp_statements sep mode =
  let pp =
    match mode with
    | Brief -> pp_tbinding_brief
    | Complete -> pp_tbinding_complete
  in
  pp_print_list ~pp_sep:(fun ppf _ -> fprintf ppf sep) (fun ppf binding -> pp ppf binding)
;;

let pp_name ppf = fprintf ppf "%s"

let pp_texpt_wt =
  let rec helper tabs ppf =
    let helper = helper tabs in
    function
    | TConst (const, _) -> Ast.pp_const ppf const
    | TVar (var_name, _) -> fprintf ppf "%s" var_name
    | TBinop (binop, ltexpr, rtexpr, _) ->
      fprintf ppf "(%a %a %a)" helper ltexpr Ast.pp_bin_op binop helper rtexpr
    | TApp (fun_texpr, arg_texpr, _) ->
      fprintf ppf "%a %a" helper fun_texpr helper arg_texpr
    | TIfThenElse (cond_texpr, then_texpr, else_texpr, _) ->
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
    | TFun (Arg (arg_name, _), texpr, _) ->
      fprintf ppf "fun %a -> %a" pp_name arg_name helper texpr
    | TLetIn (name, body_texpt, in_texpr, _) ->
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
    | TLetRecIn (name, body_texpt, in_texpr, _) ->
      fprintf
        ppf
        "%alet rec %a = %a in %a"
        space
        tabs
        pp_name
        name
        helper
        body_texpt
        helper
        in_texpr
  in
  helper 1
;;

let pp_tbinding_wt ppf = function
  | TLetRec (name, body, _) -> fprintf ppf "let rec %a = %a" pp_name name pp_texpt_wt body
  | TLet (name, body, _) -> fprintf ppf "let %a = %a" pp_name name pp_texpt_wt body
;;

let pp_statements_without_types =
  pp_print_list
    ~pp_sep:(fun ppf _ -> fprintf ppf "\n")
    (fun ppf binding -> pp_tbinding_wt ppf binding)
;;
