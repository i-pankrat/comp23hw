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
let parens_or_not p = p <|> parens p
let cint i = CInt i
let cbool b = CBool b

let var_p =
  empty *> take_while1 (fun ch -> is_letter ch || is_digit ch || Char.equal ch '_')
  >>= fun v ->
  if keywords v
  then fail "Variables cannot be keyword"
  else if is_digit @@ String.get v 0
  then fail "The first character must be a letter"
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

let chainl1 e op =
  let rec go acc = lift2 (fun f x -> f acc x) op e >>= go <|> return acc in
  e >>= fun init -> go init
;;

type edispatch =
  { evar : edispatch -> expr t
  ; econst : edispatch -> expr t
  ; econdition : edispatch -> expr t
  ; eletin : edispatch -> expr t
  ; eletrecin : edispatch -> expr t
  ; ebinop : edispatch -> expr t
  ; efun : edispatch -> expr t
  ; eapply : edispatch -> expr t
  ; expr : edispatch -> expr t
  }

let const_p = cint_p <|> cbool_p
let econst_p = (fun v -> EConst v) <$> const_p
let evar_p = (fun v -> EVar v) <$> var_p

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

let fun_args_p = many (parens_or_not var_p)
let fun_args_p1 = many1 (parens_or_not var_p)
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

let rec_p = wspaces_str "let" *> option None ((fun r -> Some r) <$> wspaces_str "rec")

let elet_fun_p expr_p =
  empty
  *> lift4
       (fun opt name args body ->
         let body = efun args body in
         match opt with
         | None -> ELet (name, body)
         | Some _ -> ELetRec (name, body))
       rec_p
       var_p
       fun_args_p
       (wspaces_str "=" *> expr_p)
;;

let elet_fun_in_p expr_p =
  let lift5 f p1 p2 p3 p4 p5 = f <$> p1 <*> p2 <*> p3 <*> p4 <*> p5 in
  empty
  *> lift5
       (fun opt name args body1 body2 ->
         let body1 = efun args body1 in
         match opt with
         | None -> ELetIn (name, body1, body2)
         | Some _ -> ELetRecIn (name, body1, body2))
       rec_p
       var_p
       fun_args_p
       (wspaces_str "=" *> expr_p)
       (wspaces_str "in" *> expr_p)
;;

let pack =
  let econst pack = fix @@ fun _ -> econst_p <|> parens @@ pack.econst pack in
  let evar pack = fix @@ fun _ -> evar_p <|> parens @@ pack.evar pack in
  let letsin pack = pack.eletin pack <|> pack.eletrecin pack in
  let expr pack =
    pack.ebinop pack
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
    in
    eapp_p (eapply_fun pack) (eapply_parse pack) <|> parens @@ pack.eapply pack
  in
  let eletin pack =
    fix @@ fun _ -> elet_fun_in_p @@ pack.expr pack <|> parens @@ pack.eletin pack
  in
  let eletrecin pack =
    fix @@ fun _ -> elet_fun_in_p @@ pack.expr pack <|> parens @@ pack.eletrecin pack
  in
  { evar; econst; ebinop; econdition; efun; eletin; eletrecin; eapply; expr }
;;

let expr_p = wspaces_p @@ pack.expr pack
let bindings_p = elet_fun_p expr_p
let statements_p = sep_by (wspaces_str ";;" <|> empty) bindings_p
let parse program = parse_string statements_p (String.trim program)

let interpret_parse f p str =
  match parse_string p str with
  | Result.Error e -> Format.printf "Error: %s" e
  | Result.Ok ast -> Format.printf "%s" (f ast)
;;

let%expect_test _ =
  interpret_parse show_const const_p "123";
  [%expect {| (CInt 123) |}]
;;

let%expect_test _ =
  interpret_parse show_const const_p "-123";
  [%expect {| (CInt -123) |}]
;;

let%expect_test _ =
  interpret_parse show_const const_p "false";
  [%expect {| (CBool false) |}]
;;

let%expect_test _ =
  interpret_parse show_const const_p "true";
  [%expect {| (CBool true) |}]
;;

let%expect_test _ =
  interpret_parse show_expr expr_p "1 + 5";
  [%expect {|
        (EBinop (Add, (EConst (CInt 1)), (EConst (CInt 5)))) |}]
;;

let%expect_test _ =
  interpret_parse show_expr expr_p "x + 5";
  [%expect {| (EBinop (Add, (EVar "x"), (EConst (CInt 5)))) |}]
;;

let%expect_test _ =
  interpret_parse show_expr expr_p "(x + 5)";
  [%expect {| (EBinop (Add, (EVar "x"), (EConst (CInt 5)))) |}]
;;

let%expect_test _ =
  interpret_parse show_expr expr_p "(x + 5) * 4";
  [%expect
    {|
    (EBinop (Mul, (EBinop (Add, (EVar "x"), (EConst (CInt 5)))),
       (EConst (CInt 4)))) |}]
;;

let%expect_test _ =
  interpret_parse show_expr expr_p "(a + b) * (c - d) * (e / d)";
  [%expect
    {|
        (EBinop (Mul,
           (EBinop (Mul, (EBinop (Add, (EVar "a"), (EVar "b"))),
              (EBinop (Sub, (EVar "c"), (EVar "d"))))),
           (EBinop (Div, (EVar "e"), (EVar "d"))))) |}]
;;

let%expect_test _ =
  interpret_parse show_expr expr_p "a > 1";
  [%expect {| (EBinop (Gt, (EVar "a"), (EConst (CInt 1)))) |}]
;;

let%expect_test _ =
  interpret_parse show_expr expr_p "a >= 1";
  [%expect {| (EBinop (Gte, (EVar "a"), (EConst (CInt 1)))) |}]
;;

let%expect_test _ =
  interpret_parse show_expr expr_p "a < 1";
  [%expect {| (EBinop (Lt, (EVar "a"), (EConst (CInt 1)))) |}]
;;

let%expect_test _ =
  interpret_parse show_expr expr_p "a <= 1";
  [%expect {| (EBinop (Lte, (EVar "a"), (EConst (CInt 1)))) |}]
;;

let%expect_test _ =
  interpret_parse show_expr expr_p "a ^ b";
  [%expect {| (EBinop (Xor, (EVar "a"), (EVar "b"))) |}]
;;

let%expect_test _ =
  interpret_parse show_expr expr_p "a <= 1 && a <> b";
  [%expect
    {|
    (EBinop (And, (EBinop (Lte, (EVar "a"), (EConst (CInt 1)))),
       (EBinop (Neq, (EVar "a"), (EVar "b"))))) |}]
;;

let%expect_test _ =
  interpret_parse show_expr expr_p "a <= 1 || a = b";
  [%expect
    {|
    (EBinop (Or, (EBinop (Lte, (EVar "a"), (EConst (CInt 1)))),
       (EBinop (Eq, (EVar "a"), (EVar "b"))))) |}]
;;

let%expect_test _ =
  interpret_parse show_expr expr_p "f x";
  [%expect {| (EApp ((EVar "f"), (EVar "x"))) |}]
;;

let%expect_test _ =
  interpret_parse show_bindings bindings_p "let x y = y";
  [%expect {| (ELet ("x", (EFun ("y", (EVar "y"))))) |}]
;;

let%expect_test _ =
  interpret_parse show_expr expr_p "in a then b else c";
  [%expect {| Error: : char '(' |}]
;;

let%expect_test _ =
  interpret_parse show_expr expr_p "if a > 1 then true else false";
  [%expect
    {|
    (EIfThenElse ((EBinop (Gt, (EVar "a"), (EConst (CInt 1)))),
       (EConst (CBool true)), (EConst (CBool false)))) |}]
;;

let%expect_test _ =
  interpret_parse show_expr expr_p "fun x y z -> x + y - z";
  [%expect
    {|
    (EFun ("x",
       (EFun ("y",
          (EFun ("z",
             (EBinop (Sub, (EBinop (Add, (EVar "x"), (EVar "y"))), (EVar "z")))))
          ))
       )) |}]
;;

let%expect_test _ =
  interpret_parse show_bindings bindings_p "let x y = y * y";
  [%expect {|
    (ELet ("x", (EFun ("y", (EBinop (Mul, (EVar "y"), (EVar "y"))))))) |}]
;;

let%expect_test _ =
  interpret_parse show_bindings bindings_p "let x y = y * y";
  [%expect {| (ELet ("x", (EFun ("y", (EBinop (Mul, (EVar "y"), (EVar "y"))))))) |}]
;;

let%expect_test _ =
  interpret_parse show_bindings bindings_p "let x = fun y -> y * y";
  [%expect {| (ELet ("x", (EFun ("y", (EBinop (Mul, (EVar "y"), (EVar "y"))))))) |}]
;;

let%expect_test _ =
  interpret_parse show_bindings bindings_p "let apply f x = f x";
  [%expect
    {|
      (ELet ("apply", (EFun ("f", (EFun ("x", (EApp ((EVar "f"), (EVar "x"))))))))) |}]
;;

let%expect_test _ =
  interpret_parse show_bindings bindings_p "let x y = if y > 0 then y else 0";
  [%expect
    {|
    (ELet ("x",
       (EFun ("y",
          (EIfThenElse ((EBinop (Gt, (EVar "y"), (EConst (CInt 0)))), (EVar "y"),
             (EConst (CInt 0))))
          ))
       )) |}]
;;

let%expect_test _ =
  interpret_parse
    show_statements
    statements_p
    "let rec fib n = if n < 1 then 1 else fib (n - 2) + fib (n - 1)";
  [%expect
    {|
    [(ELetRec ("fib",
        (EFun ("n",
           (EIfThenElse ((EBinop (Lt, (EVar "n"), (EConst (CInt 1)))),
              (EConst (CInt 1)),
              (EBinop (Add,
                 (EApp ((EVar "fib"),
                    (EBinop (Sub, (EVar "n"), (EConst (CInt 2)))))),
                 (EApp ((EVar "fib"),
                    (EBinop (Sub, (EVar "n"), (EConst (CInt 1))))))
                 ))
              ))
           ))
        ))
      ] |}]
;;

let%expect_test _ =
  interpret_parse
    show_statements
    statements_p
    "let square = fun x -> x * x  let square5 = square 5";
  [%expect
    {|
    [(ELet ("square", (EFun ("x", (EBinop (Mul, (EVar "x"), (EVar "x")))))));
      (ELet ("square5", (EApp ((EVar "square"), (EConst (CInt 5))))))]|}]
;;

let%expect_test _ =
  interpret_parse
    show_statements
    statements_p
    "let fac n =\n\
    \    let rec fact n acc = if n < 1 then acc else fact (n-1) (acc * n) in\n\
    \    fact n 1\n\
    \    let fac5 = fac 5";
  [%expect
    {|
    [(ELet ("fac",
        (EFun ("n",
           (ELetRecIn ("fact",
              (EFun ("n",
                 (EFun ("acc",
                    (EIfThenElse ((EBinop (Lt, (EVar "n"), (EConst (CInt 1)))),
                       (EVar "acc"),
                       (EApp (
                          (EApp ((EVar "fact"),
                             (EBinop (Sub, (EVar "n"), (EConst (CInt 1)))))),
                          (EBinop (Mul, (EVar "acc"), (EVar "n")))))
                       ))
                    ))
                 )),
              (EApp ((EApp ((EVar "fact"), (EVar "n"))), (EConst (CInt 1))))))
           ))
        ));
      (ELet ("fac5", (EApp ((EVar "fac"), (EConst (CInt 5))))))] |}]
;;

let%expect_test _ =
  interpret_parse show_statements statements_p "let rec fix f = f (fix f)";
  [%expect
    {|
    [(ELetRec ("fix",
        (EFun ("f", (EApp ((EVar "f"), (EApp ((EVar "fix"), (EVar "f")))))))))
      ]
   |}]
;;

let%expect_test _ =
  interpret_parse show_statements statements_p "let rec fix f eta = f (fix f) eta";
  [%expect
    {|
    [(ELetRec ("fix",
        (EFun ("f",
           (EFun ("eta",
              (EApp ((EApp ((EVar "f"), (EApp ((EVar "fix"), (EVar "f"))))),
                 (EVar "eta")))
              ))
           ))
        ))
      ]
   |}]
;;

let%expect_test _ =
  interpret_parse
    show_statements
    statements_p
    "let fix f = (fun x -> f (x x)) (fun y -> f (y y))";
  [%expect
    {|
    [(ELet ("fix",
        (EFun ("f",
           (EApp (
              (EFun ("x", (EApp ((EVar "f"), (EApp ((EVar "x"), (EVar "x"))))))),
              (EFun ("y", (EApp ((EVar "f"), (EApp ((EVar "y"), (EVar "y")))))))
              ))
           ))
        ))
      ]
   |}]
;;
