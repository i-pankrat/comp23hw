open Ast
open Angstrom

let parse_string p s = Angstrom.parse_string ~consume:Consume.All p s

let empty = function
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
  | "let" | "rec" | "in" | "fun" -> true
  | _ -> false
;;

let empty = take_while empty
let wspaces_p p = empty *> p <* empty
let wspaces_char ch = wspaces_p @@ char ch
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
  then fail "The first character must be a character"
  else return v
;;

let sign_p =
  wspaces_char '-' *> return (-1)
  <|> wspaces_char '+' *> return 1
  <|> wspaces_str "" *> return 1
;;

let cint_p =
  wspaces_p
  @@ lift2 (fun sign num -> cint (int_of_string num * sign)) sign_p (take_while1 is_digit)
;;

let cbool_p =
  wspaces_p
  @@ lift (fun b -> cbool @@ bool_of_string b) (wspaces_str "false" <|> wspaces_str "true")
;;

let chainl1 e op =
  let rec go acc = lift2 (fun f x -> f acc x) op e >>= go <|> return acc in
  e >>= fun init -> go init
;;

type edispatch =
  { evar : edispatch -> expr t
  ; econst : edispatch -> expr t
  ; econdition : edispatch -> expr t
  ; elet : edispatch -> expr t
  ; eletin : edispatch -> expr t
  ; eletrec : edispatch -> expr t
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
  let neq_p = helper (string "<>") NEq in
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

let eapp_p expr_p =
  empty
  *> lift2
       (fun expr arg ->
         let eapp = List.fold_left (fun a b -> EApp (a, b)) expr arg in
         eapp)
       expr_p
       (many (empty *> expr_p))
;;

let fun_args_p = many (parens_or_not evar_p)
let fun_args_p1 = many1 (parens_or_not evar_p)
let efun args body = List.fold_right (fun arg acc -> EFun (arg, acc)) args body

let econd pif pexpr =
  empty
  *> lift3
       (fun i t e -> EIfThenElse (i, t, e))
       (wspaces_str "if" *> pif)
       (wspaces_str "then" *> pexpr)
       (wspaces_str "else" *> pexpr)
;;

let efun_p expr_p =
  empty
  *> lift2
       (fun args expr -> efun args expr)
       (wspaces_str "fun" *> fun_args_p1)
       (wspaces_str "->" *> expr_p)
;;

let rec_p = wspaces_str "let" *> option None ((fun r -> Some r) <$> wspaces_str "rec")

let elet_fun pexpr =
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
       (wspaces_str "=" *> pexpr)
;;

let elet_fun_in pexpr =
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
       (wspaces_str "=" *> pexpr)
       (wspaces_str "in" *> pexpr)
;;

let pack =
  let econst _ = econst_p in
  let evar _ = evar_p in
  let lets pack = pack.elet pack <|> pack.eletrec pack in
  let letsin pack = pack.eletin pack <|> pack.eletrecin pack in
  let expr pack =
    letsin pack
    <|> lets pack
    <|> pack.econdition pack
    <|> pack.eapply pack
    <|> pack.efun pack
  in
  let econdition pack =
    fix
    @@ fun _ ->
    let econd_parser =
      parens_or_not @@ pack.econdition pack <|> pack.eapply pack <|> pack.efun pack
    in
    econd econd_parser (pack.expr pack)
  in
  let ebinop pack =
    fix
    @@ fun _ ->
    let ebinop_parse =
      parens @@ pack.econdition pack
      <|> parens @@ pack.ebinop pack
      <|> parens @@ pack.eapply pack
      <|> pack.econst pack
      <|> pack.evar pack
    in
    parens_or_not @@ ebinop_p ebinop_parse
  in
  let efun pack = parens_or_not @@ fix @@ fun _ -> efun_p @@ pack.expr pack in
  let eapply pack =
    fix
    @@ fun _ ->
    let eapply_parse =
      pack.ebinop pack
      <|> parens @@ pack.efun pack
      <|> parens @@ pack.econdition pack
      <|> parens @@ pack.eapply pack
    in
    eapp_p eapply_parse
  in
  let lets_parsers pack =
    pack.eapply pack <|> pack.efun pack <|> pack.econdition pack <|> letsin pack
  in
  let elet pack = fix @@ fun _ -> elet_fun @@ lets_parsers pack in
  let eletin pack = fix @@ fun _ -> elet_fun_in @@ lets_parsers pack in
  let eletrec pack = fix @@ fun _ -> elet_fun @@ lets_parsers pack in
  let eletrecin pack = fix @@ fun _ -> elet_fun_in @@ lets_parsers pack in
  { evar
  ; econst
  ; econdition
  ; elet
  ; eletin
  ; eletrec
  ; eletrecin
  ; ebinop
  ; efun
  ; eapply
  ; expr
  }
;;

let pexpr = pack.expr pack
let pstatements = sep_by (wspaces_str ";;") pexpr
let parse program = parse_string pstatements (String.trim program)

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
  interpret_parse show_expr pexpr "1 + 5";
  [%expect {|
        (EBinop (Add, (EConst (CInt 1)), (EConst (CInt 5)))) |}]
;;

let%expect_test _ =
  interpret_parse show_expr pexpr "x + 5";
  [%expect {| (EBinop (Add, (EVar "x"), (EConst (CInt 5)))) |}]
;;

let%expect_test _ =
  interpret_parse show_expr pexpr "(x + 5)";
  [%expect {| (EBinop (Add, (EVar "x"), (EConst (CInt 5)))) |}]
;;

let%expect_test _ =
  interpret_parse show_expr pexpr "(x + 5) * 4";
  [%expect
    {|
    (EBinop (Mul, (EBinop (Add, (EVar "x"), (EConst (CInt 5)))),
       (EConst (CInt 4)))) |}]
;;

let%expect_test _ =
  interpret_parse show_expr pexpr "(a + b) * (c - d) * (e / d)";
  [%expect
    {|
        (EBinop (Mul,
           (EBinop (Mul, (EBinop (Add, (EVar "a"), (EVar "b"))),
              (EBinop (Sub, (EVar "c"), (EVar "d"))))),
           (EBinop (Div, (EVar "e"), (EVar "d"))))) |}]
;;

let%expect_test _ =
  interpret_parse show_expr pexpr "a > 1";
  [%expect {| (EBinop (Gt, (EVar "a"), (EConst (CInt 1)))) |}]
;;

let%expect_test _ =
  interpret_parse show_expr pexpr "a >= 1";
  [%expect {| (EBinop (Gte, (EVar "a"), (EConst (CInt 1)))) |}]
;;

let%expect_test _ =
  interpret_parse show_expr pexpr "a < 1";
  [%expect {| (EBinop (Lt, (EVar "a"), (EConst (CInt 1)))) |}]
;;

let%expect_test _ =
  interpret_parse show_expr pexpr "a <= 1";
  [%expect {| (EBinop (Lte, (EVar "a"), (EConst (CInt 1)))) |}]
;;

let%expect_test _ =
  interpret_parse show_expr pexpr "a ^ b";
  [%expect {| (EBinop (Xor, (EVar "a"), (EVar "b"))) |}]
;;

let%expect_test _ =
  interpret_parse show_expr pexpr "a <= 1 && a <> b";
  [%expect
    {|
    (EBinop (And, (EBinop (Lte, (EVar "a"), (EConst (CInt 1)))),
       (EBinop (NEq, (EVar "a"), (EVar "b"))))) |}]
;;

let%expect_test _ =
  interpret_parse show_expr pexpr "a <= 1 || a = b";
  [%expect
    {|
    (EBinop (Or, (EBinop (Lte, (EVar "a"), (EConst (CInt 1)))),
       (EBinop (Eq, (EVar "a"), (EVar "b"))))) |}]
;;

let%expect_test _ =
  interpret_parse show_expr pexpr "f x";
  [%expect {| (EApp ((EVar "f"), (EVar "x"))) |}]
;;

let%expect_test _ =
  interpret_parse show_expr pexpr "let x y = y";
  [%expect {| (ELet ("x", (EFun ((EVar "y"), (EVar "y"))))) |}]
;;

let%expect_test _ =
  interpret_parse show_expr pexpr "in a then b else c";
  [%expect {| Error: : char '(' |}]
;;

let%expect_test _ =
  interpret_parse show_expr pexpr "if a > 1 then true else false";
  [%expect
    {|
    (EApp (
       (EApp (
          (EApp (
             (EApp (
                (EApp ((EVar "if"), (EBinop (Gt, (EVar "a"), (EConst (CInt 1))))
                   )),
                (EVar "then"))),
             (EConst (CBool true)))),
          (EVar "else"))),
       (EConst (CBool false)))) |}]
;;

let%expect_test _ =
  interpret_parse show_expr pexpr "fun x y z -> x + y - z";
  [%expect
    {|
    (EFun ((EVar "x"),
       (EFun ((EVar "y"),
          (EFun ((EVar "z"),
             (EBinop (Sub, (EBinop (Add, (EVar "x"), (EVar "y"))), (EVar "z")))))
          ))
       )) |}]
;;

let%expect_test _ =
  interpret_parse show_expr pexpr "let x y = y * y";
  [%expect
    {| (ELet ("x", (EFun ((EVar "y"), (EBinop (Mul, (EVar "y"), (EVar "y"))))))) |}]
;;

let%expect_test _ =
  interpret_parse show_expr pexpr "let x = fun y -> y * y";
  [%expect
    {| (ELet ("x", (EFun ((EVar "y"), (EBinop (Mul, (EVar "y"), (EVar "y"))))))) |}]
;;

let%expect_test _ =
  interpret_parse show_expr pexpr "let apply f x = f x";
  [%expect
    {|
      (ELet ("apply",
         (EFun ((EVar "f"), (EFun ((EVar "x"), (EApp ((EVar "f"), (EVar "x")))))))
         )) |}]
;;

let%expect_test _ =
  interpret_parse show_expr pexpr "let x y = if y > 0 then y else 0 ";
  [%expect
    {|
    (ELet ("x",
       (EFun ((EVar "y"),
          (EApp (
             (EApp (
                (EApp (
                   (EApp (
                      (EApp ((EVar "if"),
                         (EBinop (Gt, (EVar "y"), (EConst (CInt 0)))))),
                      (EVar "then"))),
                   (EVar "y"))),
                (EVar "else"))),
             (EConst (CInt 0))))
          ))
       )) |}]
;;

let%expect_test _ =
  interpret_parse
    show_expr
    pexpr
    "let fib n = if n < 1 then 1 else fib (n - 2) + fib (n - 1)";
  [%expect
    {|
    (ELet ("fib",
       (EFun ((EVar "n"),
          (EApp (
             (EApp (
                (EApp (
                   (EApp (
                      (EApp (
                         (EApp (
                            (EApp ((EVar "if"),
                               (EBinop (Lt, (EVar "n"), (EConst (CInt 1)))))),
                            (EVar "then"))),
                         (EConst (CInt 1)))),
                      (EVar "else"))),
                   (EVar "fib"))),
                (EBinop (Add, (EBinop (Sub, (EVar "n"), (EConst (CInt 2)))),
                   (EVar "fib")))
                )),
             (EBinop (Sub, (EVar "n"), (EConst (CInt 1))))))
          ))
       )) |}]
;;

let%expect_test _ =
  interpret_parse
    show_expr
    pexpr
    "let fac n =\n\
    \    let rec fact n acc = if n < 1 then acc else fact (n-1) (acc * n) in\n\
    \    fact n 1";
  [%expect
    {|
        (ELet ("fac",
           (EFun ((EVar "n"),
              (ELetRecIn ("fact",
                 (EFun ((EVar "n"),
                    (EFun ((EVar "acc"),
                       (EApp (
                          (EApp (
                             (EApp (
                                (EApp (
                                   (EApp (
                                      (EApp (
                                         (EApp ((EVar "if"),
                                            (EBinop (Lt, (EVar "n"),
                                               (EConst (CInt 1))))
                                            )),
                                         (EVar "then"))),
                                      (EVar "acc"))),
                                   (EVar "else"))),
                                (EVar "fact"))),
                             (EBinop (Sub, (EVar "n"), (EConst (CInt 1)))))),
                          (EBinop (Mul, (EVar "acc"), (EVar "n")))))
                       ))
                    )),
                 (EApp ((EApp ((EVar "fact"), (EVar "n"))), (EConst (CInt 1))))))
              ))
           )) |}]
;;

(* Should not be parsed *)
let%expect_test _ =
  interpret_parse show_expr pexpr "let test = 5 5";
  [%expect {|  |}]
;;

let%expect_test _ =
  interpret_parse show_expr pexpr "let rec test = 5 5";
  [%expect {|  |}]
;;
