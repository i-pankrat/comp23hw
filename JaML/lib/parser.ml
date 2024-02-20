(** Copyright 2023-2024, Ilya Pankratov, Maxim Drumov *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open Ast
open Angstrom

type error = string

let pp_error ppf error = Format.fprintf ppf "%s" error
let parse_string p s = Angstrom.parse_string ~consume:Consume.All p s

let is_whitespace = function
  | '\x20' | '\x0a' | '\x0d' | '\x09' -> true
  | _ -> false
;;

let is_digit = function
  | '0' .. '9' -> true
  | _ -> false
;;

let is_letter = function
  | 'a' .. 'z' | 'A' .. 'Z' -> true
  | _ -> false
;;

let keywords = function
  | "let" | "rec" | "in" | "fun" | "if" | "then" | "else" | "true" | "false" -> true
  | _ -> false
;;

let empty = take_while is_whitespace
let empty1 = take_while1 is_whitespace
let wspaces_p p = empty *> p <* empty
let wspace_l p = empty *> p
let wspaces_char ch = wspace_l @@ char ch
let wspaces_str str = wspaces_p @@ string_ci str
let parens p = wspaces_char '(' *> p <* wspaces_char ')'
let parens_or_not_desc p = p <|> parens p
let parens_or_not_asc p = parens p <|> p
let cint i = CInt i
let cbool b = CBool b
let wc_p = empty *> wspaces_char '_'

let var_p =
  empty *> take_while1 (fun ch -> is_letter ch || is_digit ch || Char.equal ch '_')
  >>= fun v ->
  if keywords v
  then fail "Variables cannot be keyword"
  else if is_digit @@ String.get v 0
  then fail "The first character must be a letter"
  else if String.get v 0 = '_' && String.length v = 1
  then fail "The variable cannot be named wildcard"
  else return v
;;

let sign_p =
  empty
  *> (wspaces_char '-' *> return (-1)
      <|> wspaces_char '+' *> return 1
      <|> wspaces_str "" *> return 1)
;;

let cint_p =
  empty
  *> lift2 (fun sign num -> cint (int_of_string num * sign)) sign_p (take_while1 is_digit)
;;

let cbool_p =
  empty
  *> lift (fun b -> cbool @@ bool_of_string b) (wspaces_str "false" <|> wspaces_str "true")
;;

let const_p = cint_p <|> cbool_p

let chainl1 e op =
  let rec go acc = lift2 (fun f x -> f acc x) op e >>= go <|> return acc in
  e >>= fun init -> go init
;;

type pdispatch =
  { value : pdispatch -> pattern t
  ; ptuple : pdispatch -> pattern t
  ; pattern : pdispatch -> pattern t (** Parser for patterns in arguments *)
  ; pattern_name : pdispatch -> pattern t
  (** Parser for patterns when declaring a function or variables *)
  }

let pvar_p = (fun v -> PVar v) <$> var_p
let pwc_p = (fun _ -> PWildcard) <$> wc_p

let tuple_p p =
  empty
  *> lift2
       (fun a b -> a :: b)
       (wspaces_p p <* wspaces_char ',')
       (sep_by1 (wspaces_char ',') p)
;;

let ptuple_p p = (fun t -> PTuple t) <$> parens_or_not_asc @@ tuple_p p

let pack =
  let pattern pack = pack.ptuple pack <|> pack.value pack in
  let pattern_name pack = pack.ptuple pack <|> pwc_p <|> pvar_p in
  let value _ = parens_or_not_desc @@ (pwc_p <|> pvar_p) in
  let ptuple pack =
    fix
    @@ fun _ ->
    ptuple_p (parens @@ pack.ptuple pack <|> pack.value pack)
    <|> parens @@ pack.ptuple pack
  in
  { value; ptuple; pattern; pattern_name }
;;

let patt_p = pack.pattern pack
let patt_name_p = pack.pattern_name pack

type edispatch =
  { evar : edispatch -> expr t
  ; econst : edispatch -> expr t
  ; etuple : edispatch -> expr t
  ; econdition : edispatch -> expr t
  ; eletin : edispatch -> expr t
  ; eletrecin : edispatch -> expr t
  ; ebinop : edispatch -> expr t
  ; efun : edispatch -> expr t
  ; eapply : edispatch -> expr t
  ; expr : edispatch -> expr t
  }

let econst_p = (fun v -> EConst v) <$> const_p
let evar_p = (fun v -> EVar v) <$> var_p
let etuple_p p = (fun t -> ETuple t) <$> parens_or_not_asc @@ tuple_p p

let ebinop_p expr =
  let helper p op = empty *> p *> return (fun e1 e2 -> EBinop (op, e1, e2)) in
  let add_p = helper (char '+') Add in
  let sub_p = helper (char '-') Sub in
  let mul_p = helper (char '*') Mul in
  let div_p = helper (char '/') Div in
  let xor_p = helper (char '^') Xor in
  let and_p = helper (string "&&") And in
  let or_p = helper (string "||") Or in
  let eq_p = helper (char '=') Eq in
  let neq_p = helper (string "<>") Neq in
  let gt_p = helper (char '>') Gt in
  let lt_p = helper (char '<') Lt in
  let gte_p = helper (string ">=") Gte in
  let lte_p = helper (string "<=") Lte in
  let muldiv_op = chainl1 expr (mul_p <|> div_p) in
  let addsub_op = chainl1 muldiv_op (add_p <|> sub_p) in
  let compare_op =
    chainl1 addsub_op (neq_p <|> gte_p <|> gt_p <|> lte_p <|> lt_p <|> eq_p)
  in
  let and_op = chainl1 compare_op and_p in
  let or_op = chainl1 and_op or_p in
  let xor_op = chainl1 or_op xor_p in
  xor_op
;;

let eapp_p expr_p1 expr_p2 =
  empty
  *> lift2
       (fun expr arg ->
         let eapp = List.fold_left (fun a b -> EApp (a, b)) expr arg in
         eapp)
       expr_p1
       (many1 @@ (empty1 *> expr_p2))
;;

let fun_args_p = many (parens_or_not_desc patt_p)
let fun_args_p1 = many1 (parens_or_not_desc patt_p)
let efun args body = List.fold_right (fun arg acc -> EFun (arg, acc)) args body

let econd pif expr_p =
  empty
  *> lift3
       (fun i t e -> EIfThenElse (i, t, e))
       (wspaces_str "if" *> pif)
       (wspaces_str "then" *> expr_p)
       (wspaces_str "else" *> expr_p)
;;

let efun_p expr_p =
  empty
  *> lift2
       (fun args expr -> efun args expr)
       (wspaces_str "fun" *> fun_args_p1)
       (wspaces_str "->" *> expr_p)
;;

let rec_p = wspaces_str "let" *> wspaces_str "rec"
let let_p = wspaces_str "let"

let elet_fun_p expr_p =
  empty
  *> (lift3
        (fun name args body ->
          let body = efun args body in
          ELetRec (name, body))
        (rec_p *> var_p)
        fun_args_p
        (wspaces_str "=" *> expr_p)
      <|> lift3
            (fun name args body ->
              let body = efun args body in
              ELet (name, body))
            (let_p *> patt_name_p)
            fun_args_p
            (wspaces_str "=" *> expr_p))
;;

let elet_fun_in_p expr_p =
  empty
  *> (lift4
        (fun name args body1 body2 ->
          let body1 = efun args body1 in
          ELetRecIn (name, body1, body2))
        (rec_p *> var_p)
        fun_args_p
        (wspaces_str "=" *> expr_p)
        (wspaces_str "in" *> expr_p)
      <|> lift4
            (fun name args body1 body2 ->
              let body1 = efun args body1 in
              ELetIn (name, body1, body2))
            (let_p *> patt_name_p)
            fun_args_p
            (wspaces_str "=" *> expr_p)
            (wspaces_str "in" *> expr_p))
;;

let pack =
  let econst pack = fix @@ fun _ -> econst_p <|> parens @@ pack.econst pack in
  let evar pack = fix @@ fun _ -> evar_p <|> parens @@ pack.evar pack in
  let letsin pack = pack.eletin pack <|> pack.eletrecin pack in
  let etuple pack =
    fix
    @@ fun _ ->
    let parsers =
      pack.ebinop pack <|> pack.eapply pack <|> pack.econdition pack <|> pack.efun pack
    in
    etuple_p (parens @@ pack.etuple pack <|> parsers) <|> parens @@ pack.etuple pack
  in
  let expr pack =
    pack.ebinop pack
    <|> pack.etuple pack
    <|> pack.eapply pack
    <|> pack.econdition pack
    <|> pack.efun pack
    <|> letsin pack
  in
  let econdition pack =
    fix
    @@ fun _ ->
    let econd_parser =
      pack.ebinop pack
      <|> parens
            (pack.ebinop pack
             <|> letsin pack
             <|> pack.eapply pack
             <|> pack.econdition pack)
    in
    econd econd_parser (pack.expr pack) <|> parens @@ pack.econdition pack
  in
  let ebinop pack =
    fix
    @@ fun _ ->
    let ebinop_parse =
      letsin pack
      <|> pack.eapply pack
      <|> parens @@ pack.econdition pack
      <|> parens @@ pack.ebinop pack
      <|> pack.evar pack
      <|> pack.econst pack
    in
    ebinop_p ebinop_parse <|> parens @@ pack.ebinop pack
  in
  let efun pack =
    fix
    @@ fun _ ->
    let efun_parse =
      pack.ebinop pack
      <|> pack.etuple pack
      <|> pack.eapply pack
      <|> pack.econdition pack
      <|> pack.efun pack
      <|> letsin pack
    in
    efun_p efun_parse <|> parens @@ pack.efun pack
  in
  let eapply pack =
    fix
    @@ fun _ ->
    let eapply_fun pack =
      pack.evar pack
      <|> parens
            (pack.econdition pack <|> pack.efun pack <|> pack.eapply pack <|> letsin pack)
    in
    let eapply_parse pack =
      parens
        (pack.ebinop pack
         <|> pack.econdition pack
         <|> pack.eapply pack
         <|> pack.efun pack
         <|> letsin pack)
      <|> pack.evar pack
      <|> pack.econst pack
      <|> parens_or_not_asc @@ pack.etuple pack
    in
    eapp_p (eapply_fun pack) (eapply_parse pack) <|> parens @@ pack.eapply pack
  in
  let eletin pack =
    fix @@ fun _ -> elet_fun_in_p @@ pack.expr pack <|> parens @@ pack.eletin pack
  in
  let eletrecin pack =
    fix @@ fun _ -> elet_fun_in_p @@ pack.expr pack <|> parens @@ pack.eletrecin pack
  in
  { evar; econst; ebinop; etuple; econdition; efun; eletin; eletrecin; eapply; expr }
;;

let expr_p = wspaces_p @@ pack.expr pack
let bindings_p = elet_fun_p expr_p
let statements_p = sep_by (wspaces_str ";;" <|> empty) bindings_p
let parse program = parse_string statements_p (String.trim program)
