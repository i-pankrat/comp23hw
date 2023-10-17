(** Copyright 2023-2024, Ilya Pankratov, Maxim Drumov *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open Jaml_lib.Parser
open Jaml_lib.Ast

let parse_string p s = Angstrom.parse_string ~consume:Angstrom.Consume.All p s

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
