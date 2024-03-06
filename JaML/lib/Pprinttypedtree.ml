(** Copyright 2023-2024, Ilya Pankratov, Maxim Drumov *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open Typedtree
open Base
open Stdlib.Format

type mode =
  | Brief
  | Complete

let get_tpattern_subst =
  let rec helper subs index = function
    | TPVar (_, typ) | TPWildcard typ -> Pprintty.get_ty_subs subs index typ
    | TPTuple (patterns, typ) ->
      List.fold
        ~init:(Pprintty.get_ty_subs subs index typ)
        ~f:(fun (a, b) c -> helper a b c)
        (List.rev patterns)
  in
  helper
;;

let get_texpr_subst =
  let rec helper subs index = function
    | TVar (_, typ) -> Pprintty.get_ty_subs subs index typ
    | TLetIn (pattern, e1, e2) ->
      let subs, index = get_tpattern_subst subs index pattern in
      let subs, index = helper subs index e1 in
      helper subs index e2
    | TBinop ((_, typ), e1, e2) | TApp (e1, e2, typ) | TLetRecIn ((_, typ), e1, e2) ->
      let subs, index = Pprintty.get_ty_subs subs index typ in
      let subs, index = helper subs index e1 in
      helper subs index e2
    | TFun (tpattern, e, typ) ->
      let subs, index = get_tpattern_subst subs index tpattern in
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

let pp_tpattern subs ppf =
  let pp_ty = Pprintty.pp_ty_with_subs subs in
  let rec helper ppf = function
    | TPVar (var, typ) -> fprintf ppf "%s: %a" var pp_ty typ
    | TPWildcard typ -> fprintf ppf "_: %a" pp_ty typ
    | TPTuple (patterns, typ) ->
      fprintf
        ppf
        "(%a): %a"
        (pp_print_list
           ~pp_sep:(fun ppf _ -> fprintf ppf ", ")
           (fun ppf pattern -> helper ppf pattern))
        (List.rev patterns)
        pp_ty
        typ
  in
  helper ppf
;;

(* A monster that needs refactoring *)
let pp_texpr_with_subs ppf depth subs texpr =
  let pp_ty = Pprintty.pp_ty_with_subs (Some subs) in
  let pp_tpattern = pp_tpattern (Some subs) in
  let rec helper depth ppf =
    let pp = helper (depth + 1) in
    function
    | TVar (name, typ) -> fprintf ppf "(%s: %a)" name pp_ty typ
    | TBinop ((op, typ), e1, e2) ->
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
    | TLetIn (tpattern, e1, e2) ->
      fprintf
        ppf
        "(TLetIn(%a%a,%a%a,%a%a%a))"
        space
        depth
        pp_tpattern
        tpattern
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
    | TLetRecIn ((name, typ), e1, e2) ->
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
    | TFun (pat, e, typ) ->
      fprintf
        ppf
        "(TFun: %a (%a%a, %a%a%a))"
        pp_ty
        typ
        space
        depth
        pp_tpattern
        pat
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
    | TTuple (tuples, typ) ->
      fprintf
        ppf
        "(%a) : %a"
        (pp_print_list
           ~pp_sep:(fun ppf _ -> fprintf ppf ", ")
           (fun ppf elem -> helper depth ppf elem))
        tuples
        pp_ty
        typ
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
  | TLetRec ((name, typ), e) as binding ->
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
  | TLet (pattern, e) as binding ->
    let subs, _ = get_texpr_subst e in
    let pp_expr ppf = pp_texpr_with_subs ppf 2 subs in
    let pp_tpattern = pp_tpattern (Some subs) in
    fprintf
      ppf
      "(%s(%a%a, %a%a\n))"
      (show_binding binding)
      space
      1
      pp_tpattern
      pattern
      space
      1
      pp_expr
      e
;;

let rec pp_tpattern_without_type ppf = function
  | TPVar (name, _) -> fprintf ppf "%s" name
  | TPWildcard _ -> fprintf ppf "_"
  | TPTuple (elems, _) ->
    fprintf
      ppf
      "(%a)"
      (pp_print_list
         ~pp_sep:(fun ppf _ -> fprintf ppf ", ")
         (fun ppf elem -> pp_tpattern_without_type ppf elem))
      elems
;;

let pp_tpattern_with_type ppf = function
  | TPVar (name, typ) -> fprintf ppf "%s: %a" name Pprintty.pp_ty typ
  | TPWildcard typ -> fprintf ppf "_ : %a" Pprintty.pp_ty typ
  | TPTuple (elems, typ) ->
    fprintf
      ppf
      "(%a) : %a"
      (pp_print_list
         ~pp_sep:(fun ppf _ -> fprintf ppf ", ")
         (fun ppf elem -> pp_tpattern_without_type ppf elem))
      elems
      Pprintty.pp_ty
      typ
;;

let pp_tbinding_brief ppf = function
  | TLetRec ((name, typ), _) -> fprintf ppf "%s: %a" name Pprintty.pp_ty typ
  | TLet (tpattern, _) -> fprintf ppf "%a" pp_tpattern_with_type tpattern
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

let pp_tpattern_wt =
  let rec helper ppf = function
    | TPVar (var, _) -> fprintf ppf "%s" var
    | TPWildcard _ -> fprintf ppf "_"
    | TPTuple (tpatterns, _) ->
      fprintf
        ppf
        "(%a)"
        (pp_print_list
           ~pp_sep:(fun ppf _ -> fprintf ppf ", ")
           (fun ppf elem -> helper ppf elem))
        (List.rev tpatterns)
  in
  helper
;;

let pp_texpt_wt =
  let rec helper tabs ppf =
    let helper = helper tabs in
    function
    | TConst (const, _) -> Ast.pp_const ppf const
    | TVar (var_name, _) -> fprintf ppf "%s" var_name
    | TBinop ((binop, _), ltexpr, rtexpr) ->
      fprintf ppf "(%a %a %a)" helper ltexpr Ast.pp_bin_op binop helper rtexpr
    | TApp (fun_texpr, arg_texpr, _) ->
      fprintf ppf "(%a %a)" helper fun_texpr helper arg_texpr
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
    | TFun (tpattern, texpr, _) ->
      fprintf ppf "fun %a -> %a" pp_tpattern_wt tpattern helper texpr
    | TLetIn (tpattern, body_texpt, in_texpr) ->
      fprintf
        ppf
        "%alet %a = %a in %a"
        space
        tabs
        pp_tpattern_wt
        tpattern
        helper
        body_texpt
        helper
        in_texpr
    | TLetRecIn ((name, _), body_texpt, in_texpr) ->
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
    | TTuple (elems, _) ->
      fprintf
        ppf
        "(%a)"
        (pp_print_list
           ~pp_sep:(fun ppf _ -> fprintf ppf ", ")
           (fun ppf elem -> helper ppf elem))
        elems
  in
  helper 1
;;

let pp_tbinding_wt ppf = function
  | TLetRec ((name, _), body) ->
    fprintf ppf "let rec %a = %a" pp_name name pp_texpt_wt body
  | TLet (tpattern, body) ->
    fprintf ppf "let %a = %a" pp_tpattern_wt tpattern pp_texpt_wt body
;;

let pp_statements_without_types =
  pp_print_list
    ~pp_sep:(fun ppf _ -> fprintf ppf "\n")
    (fun ppf binding -> pp_tbinding_wt ppf binding)
;;
