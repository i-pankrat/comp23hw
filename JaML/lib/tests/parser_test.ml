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
  [%expect {| (ELet ((PVar "x"), (EFun ((PVar "y"), (EVar "y"))))) |}]
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
    (EFun ((PVar "x"),
       (EFun ((PVar "y"),
          (EFun ((PVar "z"),
             (EBinop (Sub, (EBinop (Add, (EVar "x"), (EVar "y"))), (EVar "z")))))
          ))
       )) |}]
;;

let%expect_test _ =
  interpret_parse
    show_expr
    expr_p
    "fun (x, y, z) -> (x + y - z, (if x > y then z else x + y))";
  [%expect
    {|
    (EFun ((PTuple [(PVar "x"); (PVar "y"); (PVar "z")]),
       (ETuple
          [(EBinop (Sub, (EBinop (Add, (EVar "x"), (EVar "y"))), (EVar "z")));
            (EIfThenElse ((EBinop (Gt, (EVar "x"), (EVar "y"))), (EVar "z"),
               (EBinop (Add, (EVar "x"), (EVar "y")))))
            ])
       )) |}]
;;

let%expect_test _ =
  interpret_parse show_bindings bindings_p "let x y = y * y";
  [%expect
    {|
    (ELet ((PVar "x"),
       (EFun ((PVar "y"), (EBinop (Mul, (EVar "y"), (EVar "y"))))))) |}]
;;

let%expect_test _ =
  interpret_parse show_bindings bindings_p "let x y = y * y";
  [%expect
    {|
    (ELet ((PVar "x"),
       (EFun ((PVar "y"), (EBinop (Mul, (EVar "y"), (EVar "y"))))))) |}]
;;

let%expect_test _ =
  interpret_parse show_bindings bindings_p "let x = fun y -> y * y";
  [%expect
    {|
    (ELet ((PVar "x"),
       (EFun ((PVar "y"), (EBinop (Mul, (EVar "y"), (EVar "y"))))))) |}]
;;

let%expect_test _ =
  interpret_parse show_pattern patt_p "a, (x, y), c";
  [%expect {| (PTuple [(PVar "a"); (PTuple [(PVar "x"); (PVar "y")]); (PVar "c")]) |}]
;;

let%expect_test _ =
  interpret_parse show_pattern patt_p "_,_";
  [%expect {| (PTuple [PWildcard; PWildcard]) |}]
;;

let%expect_test _ =
  interpret_parse show_pattern patt_p "((a, (b, c),(d, (e, g),(h, j))),(i, k))";
  [%expect
    {|
    (PTuple
       [(PTuple
           [(PVar "a"); (PTuple [(PVar "b"); (PVar "c")]);
             (PTuple
                [(PVar "d"); (PTuple [(PVar "e"); (PVar "g")]);
                  (PTuple [(PVar "h"); (PVar "j")])])
             ]);
         (PTuple [(PVar "i"); (PVar "k")])])
   |}]
;;

let%expect_test _ =
  interpret_parse show_statements statements_p "let f (a, b, _, d) = (a, b, d)";
  [%expect
    {|
    [(ELet ((PVar "f"),
        (EFun ((PTuple [(PVar "a"); (PVar "b"); PWildcard; (PVar "d")]),
           (ETuple [(EVar "a"); (EVar "b"); (EVar "d")])))
        ))
      ]
   |}]
;;

let%expect_test _ =
  interpret_parse
    show_statements
    statements_p
    "let apply_tuple_to_function func a b c = func (a, b, c)";
  [%expect
    {|
    [(ELet ((PVar "apply_tuple_to_function"),
        (EFun ((PVar "func"),
           (EFun ((PVar "a"),
              (EFun ((PVar "b"),
                 (EFun ((PVar "c"),
                    (EApp ((EVar "func"),
                       (ETuple [(EVar "a"); (EVar "b"); (EVar "c")])))
                    ))
                 ))
              ))
           ))
        ))
      ]
   |}]
;;

let%expect_test _ =
  interpret_parse show_statements statements_p "let const _ = 1";
  [%expect {| [(ELet ((PVar "const"), (EFun (PWildcard, (EConst (CInt 1))))))] |}]
;;

let%expect_test _ =
  interpret_parse
    show_statements
    statements_p
    "let strange_tuple ((a, (b, c),(d, (e, g),(h, j))),(i, k)) = ((a, b, c, d), (e, g, \
     h, j), (i, k))";
  [%expect
    {|
    [(ELet ((PVar "strange_tuple"),
        (EFun (
           (PTuple
              [(PTuple
                  [(PVar "a"); (PTuple [(PVar "b"); (PVar "c")]);
                    (PTuple
                       [(PVar "d"); (PTuple [(PVar "e"); (PVar "g")]);
                         (PTuple [(PVar "h"); (PVar "j")])])
                    ]);
                (PTuple [(PVar "i"); (PVar "k")])]),
           (ETuple
              [(ETuple [(EVar "a"); (EVar "b"); (EVar "c"); (EVar "d")]);
                (ETuple [(EVar "e"); (EVar "g"); (EVar "h"); (EVar "j")]);
                (ETuple [(EVar "i"); (EVar "k")])])
           ))
        ))
      ]
   |}]
;;

let%expect_test _ =
  interpret_parse show_statements statements_p "let fst_sum (a, _) (c, _) = a + c";
  [%expect
    {|
    [(ELet ((PVar "fst_sum"),
        (EFun ((PTuple [(PVar "a"); PWildcard]),
           (EFun ((PTuple [(PVar "c"); PWildcard]),
              (EBinop (Add, (EVar "a"), (EVar "c")))))
           ))
        ))
      ]
   |}]
;;

let%expect_test _ =
  interpret_parse
    show_statements
    statements_p
    "let f (((((((((((a,b))))), ((((((c, d)))))))))))) = a + b + c + d";
  [%expect
    {|
    [(ELet ((PVar "f"),
        (EFun (
           (PTuple
              [(PTuple [(PVar "a"); (PVar "b")]);
                (PTuple [(PVar "c"); (PVar "d")])]),
           (EBinop (Add,
              (EBinop (Add, (EBinop (Add, (EVar "a"), (EVar "b"))), (EVar "c"))),
              (EVar "d")))
           ))
        ))
      ]
   |}]
;;

let%expect_test _ =
  interpret_parse show_statements statements_p "let test a = let (f, s) = a in f + s";
  [%expect
    {|
    [(ELet ((PVar "test"),
        (EFun ((PVar "a"),
           (ELetIn ((PTuple [(PVar "f"); (PVar "s")]), (EVar "a"),
              (EBinop (Add, (EVar "f"), (EVar "s")))))
           ))
        ))
      ]
   |}]
;;

let%expect_test _ =
  interpret_parse
    show_statements
    statements_p
    "let tuple_sum fst_tuple snd_fst snd_snd = \n\
    \    let snd_tuple = (snd_fst, snd_snd) in\n\
    \    let (fst_fst, fst_snd) = fst_tuple in\n\
    \    (fst_fst + snd_fst, fst_snd + snd_snd)\n\
    \    ";
  [%expect
    {|
    [(ELet ((PVar "tuple_sum"),
        (EFun ((PVar "fst_tuple"),
           (EFun ((PVar "snd_fst"),
              (EFun ((PVar "snd_snd"),
                 (ELetIn ((PVar "snd_tuple"),
                    (ETuple [(EVar "snd_fst"); (EVar "snd_snd")]),
                    (ELetIn ((PTuple [(PVar "fst_fst"); (PVar "fst_snd")]),
                       (EVar "fst_tuple"),
                       (ETuple
                          [(EBinop (Add, (EVar "fst_fst"), (EVar "snd_fst")));
                            (EBinop (Add, (EVar "fst_snd"), (EVar "snd_snd")))])
                       ))
                    ))
                 ))
              ))
           ))
        ))
      ]
   |}]
;;

let%expect_test _ =
  interpret_parse show_statements statements_p "let f (a, b) = a + b";
  [%expect
    {|
    [(ELet ((PVar "f"),
        (EFun ((PTuple [(PVar "a"); (PVar "b")]),
           (EBinop (Add, (EVar "a"), (EVar "b")))))
        ))
      ]
   |}]
;;

let%expect_test _ =
  interpret_parse show_bindings bindings_p "let apply f x = f x";
  [%expect
    {|
      (ELet ((PVar "apply"),
         (EFun ((PVar "f"), (EFun ((PVar "x"), (EApp ((EVar "f"), (EVar "x")))))))
         )) |}]
;;

let%expect_test _ =
  interpret_parse show_bindings bindings_p "let x y = if y > 0 then y else 0";
  [%expect
    {|
    (ELet ((PVar "x"),
       (EFun ((PVar "y"),
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
        (EFun ((PVar "n"),
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
    [(ELet ((PVar "square"),
        (EFun ((PVar "x"), (EBinop (Mul, (EVar "x"), (EVar "x")))))));
      (ELet ((PVar "square5"), (EApp ((EVar "square"), (EConst (CInt 5))))))]|}]
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
    [(ELet ((PVar "fac"),
        (EFun ((PVar "n"),
           (ELetRecIn ("fact",
              (EFun ((PVar "n"),
                 (EFun ((PVar "acc"),
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
      (ELet ((PVar "fac5"), (EApp ((EVar "fac"), (EConst (CInt 5))))))] |}]
;;

let%expect_test _ =
  interpret_parse show_statements statements_p "let rec fix f = f (fix f)";
  [%expect
    {|
    [(ELetRec ("fix",
        (EFun ((PVar "f"), (EApp ((EVar "f"), (EApp ((EVar "fix"), (EVar "f")))))
           ))
        ))
      ]
   |}]
;;

let%expect_test _ =
  interpret_parse show_statements statements_p "let rec fix f eta = f (fix f) eta";
  [%expect
    {|
    [(ELetRec ("fix",
        (EFun ((PVar "f"),
           (EFun ((PVar "eta"),
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
    [(ELet ((PVar "fix"),
        (EFun ((PVar "f"),
           (EApp (
              (EFun ((PVar "x"),
                 (EApp ((EVar "f"), (EApp ((EVar "x"), (EVar "x"))))))),
              (EFun ((PVar "y"),
                 (EApp ((EVar "f"), (EApp ((EVar "y"), (EVar "y")))))))
              ))
           ))
        ))
      ]
   |}]
;;
