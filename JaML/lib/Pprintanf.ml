open Anf
open Stdlib.Format

let pp_name ppf = fprintf ppf "%s"
let space ppf depth = fprintf ppf "\n%*s" (4 * depth) ""

let pp_immexpr ppf = function
  | ImmNum i -> fprintf ppf "%d" i
  | ImmBool b -> fprintf ppf "%B" b
  | ImmId s -> fprintf ppf "%s" s
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
  | CApp (func, arg) -> fprintf ppf "(%a %a)" pp_immexpr func pp_immexpr arg
  | CImmExpr immexpr -> fprintf ppf "%a" pp_immexpr immexpr
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
    | ALetRec (name, cexpr, aexpr) ->
      fprintf
        ppf
        "%alet rec %a = %a in %a"
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
        helper
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
  | AnfLet (name, args, aexpr) ->
    fprintf ppf "let %a %a = %a" pp_name name pp_args args pp_aexpr aexpr
  | AnfLetRec (name, args, aexpr) ->
    fprintf ppf "let rec %a %a = %a" pp_name name pp_args args pp_aexpr aexpr
;;

let pp_anfstatements =
  pp_print_list
    ~pp_sep:(fun ppf _ -> fprintf ppf ";\n")
    (fun ppf binding -> pp_anfexpr ppf binding)
;;
