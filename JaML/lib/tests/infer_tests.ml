(** Copyright 2023-2024, Ilya Pankratov, Maxim Drumov *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open Jaml_lib.Inferencer

let run_infer_expr =
  let open Jaml_lib.Pprinttypedtree in
  function
  | Result.Error e -> Stdlib.Format.printf "%a%!" pp_error e
  | Result.Ok (_, te) -> Stdlib.Format.printf "%a%!" pp_texpr te
;;

let run_infer_statements =
  let open Jaml_lib.Pprinttypedtree in
  function
  | Result.Error e -> Stdlib.Format.printf "%a%!" pp_error e
  | Result.Ok te ->
    let pp_statements = pp_statements ";\n" Complete in
    Stdlib.Format.printf "%a%!" pp_statements te
;;

(** Infer tests *)

(** Constants  tests *)

let%expect_test _ =
  let open Jaml_lib.Ast in
  let _ =
    let e = EConst (CInt 4) in
    infer_expr e |> run_infer_expr
  in
  [%expect {| (TConst((CInt 4): int)) |}]
;;

let%expect_test _ =
  let open Jaml_lib.Ast in
  let _ =
    let e = EConst (CBool false) in
    infer_expr e |> run_infer_expr
  in
  [%expect {| (TConst((CBool false): bool)) |}]
;;

(** Variable tests *)

let%expect_test _ =
  let open Jaml_lib.Ast in
  let _ =
    let e = EVar "x" in
    infer_expr e |> run_infer_expr
  in
  [%expect {| Typechecker error: undefined variable 'x'  |}]
;;

(** Tests for integer binary operations *)

let%expect_test _ =
  let open Jaml_lib.Ast in
  let _ =
    let e = EBinop (Add, EConst (CInt 1), EConst (CInt 1)) in
    infer_expr e |> run_infer_expr
  in
  [%expect
    {|
    (Add: (int -> (int -> int)) (
        (TConst((CInt 1): int)),
        (TConst((CInt 1): int))
    ))
|}]
;;

let%expect_test _ =
  let open Jaml_lib.Ast in
  let _ =
    let e = EBinop (Sub, EConst (CInt 1), EConst (CInt 1)) in
    infer_expr e |> run_infer_expr
  in
  [%expect
    {|
    (Sub: (int -> (int -> int)) (
        (TConst((CInt 1): int)),
        (TConst((CInt 1): int))
    ))
|}]
;;

let%expect_test _ =
  let open Jaml_lib.Ast in
  let _ =
    let e = EBinop (Div, EConst (CInt 1), EConst (CInt 1)) in
    infer_expr e |> run_infer_expr
  in
  [%expect
    {|
    (Div: (int -> (int -> int)) (
        (TConst((CInt 1): int)),
        (TConst((CInt 1): int))
    ))
|}]
;;

let%expect_test _ =
  let open Jaml_lib.Ast in
  let _ =
    let e = EBinop (Mul, EConst (CInt 1), EConst (CInt 1)) in
    infer_expr e |> run_infer_expr
  in
  [%expect
    {|
    (Mul: (int -> (int -> int)) (
        (TConst((CInt 1): int)),
        (TConst((CInt 1): int))
    ))
|}]
;;

let%expect_test _ =
  let open Jaml_lib.Ast in
  let _ =
    let e = EBinop (Eq, EConst (CInt (-1)), EConst (CInt 100)) in
    infer_expr e |> run_infer_expr
  in
  [%expect
    {|
    (Eq: (int -> (int -> bool)) (
        (TConst((CInt -1): int)),
        (TConst((CInt 100): int))
    ))
|}]
;;

let%expect_test _ =
  let open Jaml_lib.Ast in
  let _ =
    let e = EBinop (Neq, EConst (CInt (-1)), EConst (CInt 100)) in
    infer_expr e |> run_infer_expr
  in
  [%expect
    {|
    (Neq: (int -> (int -> bool)) (
        (TConst((CInt -1): int)),
        (TConst((CInt 100): int))
    ))
|}]
;;

let%expect_test _ =
  let open Jaml_lib.Ast in
  let _ =
    let e = EBinop (Gt, EConst (CInt (-1)), EConst (CInt 100)) in
    infer_expr e |> run_infer_expr
  in
  [%expect
    {|
    (Gt: (int -> (int -> bool)) (
        (TConst((CInt -1): int)),
        (TConst((CInt 100): int))
    ))
|}]
;;

let%expect_test _ =
  let open Jaml_lib.Ast in
  let _ =
    let e = EBinop (Lt, EConst (CInt (-1)), EConst (CInt 100)) in
    infer_expr e |> run_infer_expr
  in
  [%expect
    {|
    (Lt: (int -> (int -> bool)) (
        (TConst((CInt -1): int)),
        (TConst((CInt 100): int))
    ))
|}]
;;

let%expect_test _ =
  let open Jaml_lib.Ast in
  let _ =
    let e = EBinop (Gte, EConst (CInt (-1)), EConst (CInt 100)) in
    infer_expr e |> run_infer_expr
  in
  [%expect
    {|
    (Gte: (int -> (int -> bool)) (
        (TConst((CInt -1): int)),
        (TConst((CInt 100): int))
    ))
|}]
;;

let%expect_test _ =
  let open Jaml_lib.Ast in
  let _ =
    let e = EBinop (Lte, EConst (CInt (-1)), EConst (CInt 100)) in
    infer_expr e |> run_infer_expr
  in
  [%expect
    {|
    (Lte: (int -> (int -> bool)) (
        (TConst((CInt -1): int)),
        (TConst((CInt 100): int))
    ))
|}]
;;

(** Tests for boolean binary operations *)

let%expect_test _ =
  let open Jaml_lib.Ast in
  let _ =
    let e = EBinop (Xor, EConst (CBool true), EConst (CBool false)) in
    infer_expr e |> run_infer_expr
  in
  [%expect
    {|
    (Xor: (bool -> (bool -> bool)) (
        (TConst((CBool true): bool)),
        (TConst((CBool false): bool))
    ))
|}]
;;

let%expect_test _ =
  let open Jaml_lib.Ast in
  let _ =
    let e = EBinop (And, EConst (CBool true), EConst (CBool false)) in
    infer_expr e |> run_infer_expr
  in
  [%expect
    {|
    (And: (bool -> (bool -> bool)) (
        (TConst((CBool true): bool)),
        (TConst((CBool false): bool))
    ))
|}]
;;

let%expect_test _ =
  let open Jaml_lib.Ast in
  let _ =
    let e = EBinop (Or, EConst (CBool true), EConst (CBool false)) in
    infer_expr e |> run_infer_expr
  in
  [%expect
    {|
    (Or: (bool -> (bool -> bool)) (
        (TConst((CBool true): bool)),
        (TConst((CBool false): bool))
    ))
|}]
;;

let%expect_test _ =
  let open Jaml_lib.Ast in
  let _ =
    let e = EBinop (Eq, EConst (CBool true), EConst (CBool false)) in
    infer_expr e |> run_infer_expr
  in
  [%expect
    {|
    (Eq: (bool -> (bool -> bool)) (
        (TConst((CBool true): bool)),
        (TConst((CBool false): bool))
    ))
|}]
;;

let%expect_test _ =
  let open Jaml_lib.Ast in
  let _ =
    let e = EBinop (Neq, EConst (CBool true), EConst (CBool false)) in
    infer_expr e |> run_infer_expr
  in
  [%expect
    {|
    (Neq: (bool -> (bool -> bool)) (
        (TConst((CBool true): bool)),
        (TConst((CBool false): bool))
    ))
|}]
;;

let%expect_test _ =
  let open Jaml_lib.Ast in
  let _ =
    let e = EBinop (Gt, EConst (CBool true), EConst (CBool false)) in
    infer_expr e |> run_infer_expr
  in
  [%expect
    {|
    (Gt: (bool -> (bool -> bool)) (
        (TConst((CBool true): bool)),
        (TConst((CBool false): bool))
    ))
|}]
;;

let%expect_test _ =
  let open Jaml_lib.Ast in
  let _ =
    let e = EBinop (Gt, EConst (CBool true), EConst (CBool false)) in
    infer_expr e |> run_infer_expr
  in
  [%expect
    {|
    (Gt: (bool -> (bool -> bool)) (
        (TConst((CBool true): bool)),
        (TConst((CBool false): bool))
    ))
|}]
;;

let%expect_test _ =
  let open Jaml_lib.Ast in
  let _ =
    let e = EBinop (Lt, EConst (CBool true), EConst (CBool false)) in
    infer_expr e |> run_infer_expr
  in
  [%expect
    {|
    (Lt: (bool -> (bool -> bool)) (
        (TConst((CBool true): bool)),
        (TConst((CBool false): bool))
    ))
|}]
;;

let%expect_test _ =
  let open Jaml_lib.Ast in
  let _ =
    let e = EBinop (Gte, EConst (CBool true), EConst (CBool false)) in
    infer_expr e |> run_infer_expr
  in
  [%expect
    {|
    (Gte: (bool -> (bool -> bool)) (
        (TConst((CBool true): bool)),
        (TConst((CBool false): bool))
    ))
|}]
;;

let%expect_test _ =
  let open Jaml_lib.Ast in
  let _ =
    let e = EBinop (Lte, EConst (CBool true), EConst (CBool false)) in
    infer_expr e |> run_infer_expr
  in
  [%expect
    {|
    (Lte: (bool -> (bool -> bool)) (
        (TConst((CBool true): bool)),
        (TConst((CBool false): bool))
    ))
|}]
;;

(** Tests for wrong typed binary operations *)

let%expect_test _ =
  let open Jaml_lib.Ast in
  let _ =
    let e = EBinop (Add, EConst (CInt 42), EConst (CBool false)) in
    infer_expr e |> run_infer_expr
  in
  [%expect {|
    Typechecker error: unification failed on bool and int
|}]
;;

let%expect_test _ =
  let open Jaml_lib.Ast in
  let _ =
    let e = EBinop (Sub, EConst (CInt 42), EConst (CBool false)) in
    infer_expr e |> run_infer_expr
  in
  [%expect {|
    Typechecker error: unification failed on bool and int
|}]
;;

let%expect_test _ =
  let open Jaml_lib.Ast in
  let _ =
    let e = EBinop (Div, EConst (CInt 42), EConst (CBool false)) in
    infer_expr e |> run_infer_expr
  in
  [%expect {|
    Typechecker error: unification failed on bool and int
|}]
;;

let%expect_test _ =
  let open Jaml_lib.Ast in
  let _ =
    let e = EBinop (Mul, EConst (CInt 42), EConst (CBool false)) in
    infer_expr e |> run_infer_expr
  in
  [%expect {|
    Typechecker error: unification failed on bool and int
|}]
;;

let%expect_test _ =
  let open Jaml_lib.Ast in
  let _ =
    let e = EBinop (Xor, EConst (CInt 42), EConst (CBool false)) in
    infer_expr e |> run_infer_expr
  in
  [%expect {|
    Typechecker error: unification failed on int and bool
|}]
;;

let%expect_test _ =
  let open Jaml_lib.Ast in
  let _ =
    let e = EBinop (And, EConst (CInt 42), EConst (CBool false)) in
    infer_expr e |> run_infer_expr
  in
  [%expect {|
    Typechecker error: unification failed on int and bool
|}]
;;

let%expect_test _ =
  let open Jaml_lib.Ast in
  let _ =
    let e = EBinop (Or, EConst (CInt 42), EConst (CBool false)) in
    infer_expr e |> run_infer_expr
  in
  [%expect {|
    Typechecker error: unification failed on int and bool
|}]
;;

let%expect_test _ =
  let open Jaml_lib.Ast in
  let _ =
    let e = EBinop (Eq, EConst (CInt 42), EConst (CBool false)) in
    infer_expr e |> run_infer_expr
  in
  [%expect {|
    Typechecker error: unification failed on int and bool
|}]
;;

let%expect_test _ =
  let open Jaml_lib.Ast in
  let _ =
    let e = EBinop (Neq, EConst (CInt 42), EConst (CBool false)) in
    infer_expr e |> run_infer_expr
  in
  [%expect {|
    Typechecker error: unification failed on int and bool
|}]
;;

let%expect_test _ =
  let open Jaml_lib.Ast in
  let _ =
    let e = EBinop (Gt, EConst (CInt 42), EConst (CBool false)) in
    infer_expr e |> run_infer_expr
  in
  [%expect {|
    Typechecker error: unification failed on int and bool
|}]
;;

let%expect_test _ =
  let open Jaml_lib.Ast in
  let _ =
    let e = EBinop (Lt, EConst (CInt 42), EConst (CBool false)) in
    infer_expr e |> run_infer_expr
  in
  [%expect {|
    Typechecker error: unification failed on int and bool
|}]
;;

let%expect_test _ =
  let open Jaml_lib.Ast in
  let _ =
    let e = EBinop (Gte, EConst (CInt 42), EConst (CBool false)) in
    infer_expr e |> run_infer_expr
  in
  [%expect {|
    Typechecker error: unification failed on int and bool
|}]
;;

let%expect_test _ =
  let open Jaml_lib.Ast in
  let _ =
    let e = EBinop (Lte, EConst (CInt 42), EConst (CBool false)) in
    infer_expr e |> run_infer_expr
  in
  [%expect {|
    Typechecker error: unification failed on int and bool
|}]
;;

(** Test for typing anonymous function *)

let%expect_test _ =
  let open Jaml_lib.Ast in
  let _ =
    let e = EFun ("x", EVar "x") in
    infer_expr e |> run_infer_expr
  in
  [%expect {|
    (TFun: ('a -> 'a) (
        (x: 'a),
        (x: 'a)
    ))
|}]
;;

let%expect_test _ =
  let open Jaml_lib.Ast in
  let _ =
    let e = EFun ("x", EConst (CInt 1)) in
    infer_expr e |> run_infer_expr
  in
  [%expect
    {|
    (TFun: ('a -> int) (
        (x: 'a),
        (TConst((CInt 1): int))
    ))
|}]
;;

let%expect_test _ =
  let open Jaml_lib.Ast in
  let _ =
    let e = EFun ("x", EConst (CBool false)) in
    infer_expr e |> run_infer_expr
  in
  [%expect
    {|
    (TFun: ('a -> bool) (
        (x: 'a),
        (TConst((CBool false): bool))
    ))
|}]
;;

(** Tests for typing condition statement *)

let%expect_test _ =
  let open Jaml_lib.Ast in
  let _ =
    let e = EIfThenElse (EConst (CBool true), EConst (CInt 4), EConst (CInt 5)) in
    infer_expr e |> run_infer_expr
  in
  [%expect
    {|
    (TIfThenElse: int
        ((TConst((CBool true): bool)),
        (TConst((CInt 4): int)),
        (TConst((CInt 5): int))
    ))
|}]
;;

let%expect_test _ =
  let open Jaml_lib.Ast in
  let _ =
    let e =
      EIfThenElse (EConst (CBool true), EConst (CBool true), EConst (CBool false))
    in
    infer_expr e |> run_infer_expr
  in
  [%expect
    {|
    (TIfThenElse: bool
        ((TConst((CBool true): bool)),
        (TConst((CBool true): bool)),
        (TConst((CBool false): bool))
    ))
|}]
;;

let%expect_test _ =
  let open Jaml_lib.Ast in
  let _ =
    let e = EIfThenElse (EConst (CInt 3), EConst (CBool true), EConst (CBool false)) in
    infer_expr e |> run_infer_expr
  in
  [%expect {|
    Typechecker error: unification failed on int and bool
|}]
;;

let%expect_test _ =
  let open Jaml_lib.Ast in
  let _ =
    let e = EIfThenElse (EConst (CBool true), EConst (CBool true), EConst (CInt 3)) in
    infer_expr e |> run_infer_expr
  in
  [%expect {|
    Typechecker error: unification failed on bool and int
|}]
;;

(** Let in tests *)

let%expect_test _ =
  let open Jaml_lib.Ast in
  let _ =
    let e = ELetIn ("id", EFun ("x", EVar "x"), EApp (EVar "id", EConst (CInt 1))) in
    infer_expr e |> run_infer_expr
  in
  [%expect
    {|
    (TLetIn(
        id: ('a -> 'a),
        (TFun: ('a -> 'a) (
            (x: 'a),
            (x: 'a)
        )),
        (TApp: int (
            (id: (int -> int)),
            (TConst((CInt 1): int))
        ))
    ))
|}]
;;

(** Statements tests *)

let%expect_test _ =
  let open Jaml_lib.Ast in
  let _ =
    let e = [ ELet ("result", EConst (CBool true)) ] in
    infer_statements e |> run_infer_statements
  in
  [%expect
    {|
    (TLet(
        result: bool,
        (TConst((CBool true): bool))
    ))
|}]
;;

let%expect_test _ =
  let open Jaml_lib.Ast in
  let _ =
    let e = [ ELet ("result", EConst (CInt 4)) ] in
    infer_statements e |> run_infer_statements
  in
  [%expect {|
    (TLet(
        result: int,
        (TConst((CInt 4): int))
    ))
|}]
;;

let%expect_test _ =
  let open Jaml_lib.Ast in
  let _ =
    let e = [ ELet ("result", EFun ("x", EVar "x")) ] in
    infer_statements e |> run_infer_statements
  in
  [%expect
    {|
    (TLet(
        result: ('a -> 'a),
        (TFun: ('a -> 'a) (
            (x: 'a),
            (x: 'a)
        ))
    ))
|}]
;;

let%expect_test _ =
  let open Jaml_lib.Ast in
  let _ =
    let e = [ ELet ("result", EFun ("x", EConst (CInt 5))) ] in
    infer_statements e |> run_infer_statements
  in
  [%expect
    {|
    (TLet(
        result: ('a -> int),
        (TFun: ('a -> int) (
            (x: 'a),
            (TConst((CInt 5): int))
        ))
    ))
|}]
;;

let%expect_test _ =
  let open Jaml_lib.Ast in
  let _ =
    let e =
      [ ELet
          ( "sum4"
          , EFun
              ( "x"
              , EFun
                  ( "y"
                  , EFun
                      ( "z"
                      , EFun
                          ( "w"
                          , EBinop
                              ( Add
                              , EBinop (Add, EVar "x", EVar "y")
                              , EBinop (Add, EVar "z", EVar "w") ) ) ) ) ) )
      ]
    in
    infer_statements e |> run_infer_statements
  in
  [%expect
    {|
    (TLet(
        sum4: (int -> (int -> (int -> (int -> int)))),
        (TFun: (int -> (int -> (int -> (int -> int)))) (
            (x: int),
            (TFun: (int -> (int -> (int -> int))) (
                (y: int),
                (TFun: (int -> (int -> int)) (
                    (z: int),
                    (TFun: (int -> int) (
                        (w: int),
                        (Add: (int -> (int -> int)) (
                            (Add: (int -> (int -> int)) (
                                (x: int),
                                (y: int)
                            )),
                            (Add: (int -> (int -> int)) (
                                (z: int),
                                (w: int)
                            ))
                        ))
                    ))
                ))
            ))
        ))
    ))
|}]
;;

let%expect_test _ =
  let open Jaml_lib.Ast in
  let _ =
    let e = [ ELet ("apply", EFun ("f", EFun ("a", EApp (EVar "f", EVar "a")))) ] in
    infer_statements e |> run_infer_statements
  in
  [%expect
    {|
    (TLet(
        apply: (('a -> 'b) -> ('a -> 'b)),
        (TFun: (('a -> 'b) -> ('a -> 'b)) (
            (f: ('a -> 'b)),
            (TFun: ('a -> 'b) (
                (a: 'a),
                (TApp: 'b (
                    (f: ('a -> 'b)),
                    (a: 'a)
                ))
            ))
        ))
    ))

|}]
;;

let%expect_test _ =
  let open Jaml_lib.Ast in
  let _ =
    let e =
      [ ELet
          ( "apply5"
          , EFun
              ( "function"
              , EFun
                  ( "a"
                  , EFun
                      ( "b"
                      , EFun
                          ( "c"
                          , EFun
                              ( "d"
                              , EFun
                                  ( "e"
                                  , EApp
                                      ( EApp
                                          ( EApp
                                              ( EApp
                                                  ( EApp (EVar "function", EVar "a")
                                                  , EVar "b" )
                                              , EVar "c" )
                                          , EVar "d" )
                                      , EVar "e" ) ) ) ) ) ) ) )
      ]
    in
    infer_statements e |> run_infer_statements
  in
  [%expect
    {|
    (TLet(
        apply5: (('a -> ('b -> ('c -> ('d -> ('e -> 'f))))) -> ('a -> ('b -> ('c -> ('d -> ('e -> 'f)))))),
        (TFun: (('a -> ('b -> ('c -> ('d -> ('e -> 'f))))) -> ('a -> ('b -> ('c -> ('d -> ('e -> 'f)))))) (
            (function: ('a -> ('b -> ('c -> ('d -> ('e -> 'f)))))),
            (TFun: ('a -> ('b -> ('c -> ('d -> ('e -> 'f))))) (
                (a: 'a),
                (TFun: ('b -> ('c -> ('d -> ('e -> 'f)))) (
                    (b: 'b),
                    (TFun: ('c -> ('d -> ('e -> 'f))) (
                        (c: 'c),
                        (TFun: ('d -> ('e -> 'f)) (
                            (d: 'd),
                            (TFun: ('e -> 'f) (
                                (e: 'e),
                                (TApp: 'f (
                                    (TApp: ('e -> 'f) (
                                        (TApp: ('d -> ('e -> 'f)) (
                                            (TApp: ('c -> ('d -> ('e -> 'f))) (
                                                (TApp: ('b -> ('c -> ('d -> ('e -> 'f)))) (
                                                    (function: ('a -> ('b -> ('c -> ('d -> ('e -> 'f)))))),
                                                    (a: 'a)
                                                )),
                                                (b: 'b)
                                            )),
                                            (c: 'c)
                                        )),
                                        (d: 'd)
                                    )),
                                    (e: 'e)
                                ))
                            ))
                        ))
                    ))
                ))
            ))
        ))
    ))
|}]
;;

(** Let rec in tests *)

let%expect_test _ =
  let open Jaml_lib.Ast in
  let _ =
    let e =
      [ ELet
          ( "sumn"
          , EFun
              ( "x"
              , ELetRecIn
                  ( "helper"
                  , EFun
                      ( "x"
                      , EIfThenElse
                          ( EBinop (Eq, EVar "x", EConst (CInt 1))
                          , EConst (CInt 1)
                          , EBinop (Add, EVar "x", EBinop (Sub, EVar "x", EConst (CInt 1)))
                          ) )
                  , EApp (EVar "helper", EVar "x") ) ) )
      ]
    in
    infer_statements e |> run_infer_statements
  in
  [%expect
    {|
    (TLet(
        sumn: (int -> int),
        (TFun: (int -> int) (
            (x: int),
            (TLetRecIn(
                helper: (int -> int),
                (TFun: (int -> int) (
                    (x: int),
                    (TIfThenElse: int
                        ((Eq: (int -> (int -> bool)) (
                            (x: int),
                            (TConst((CInt 1): int))
                        )),
                        (TConst((CInt 1): int)),
                        (Add: (int -> (int -> int)) (
                            (x: int),
                            (Sub: (int -> (int -> int)) (
                                (x: int),
                                (TConst((CInt 1): int))
                            ))
                        ))
                    ))
                )),
                (TApp: int (
                    (helper: (int -> int)),
                    (x: int)
                ))
            ))
        ))
    ))
|}]
;;

(** Factorial and fibonacci tests *)

let%expect_test _ =
  let open Jaml_lib.Ast in
  let _ =
    let e =
      [ ELetRec
          ( "fact"
          , EFun
              ( "n"
              , EIfThenElse
                  ( EBinop (Eq, EVar "n", EConst (CInt 1))
                  , EConst (CInt 1)
                  , EBinop
                      ( Mul
                      , EApp (EVar "fact", EVar "n")
                      , EApp (EVar "fact", EBinop (Sub, EVar "n", EConst (CInt 1))) ) ) )
          )
      ]
    in
    infer_statements e |> run_infer_statements
  in
  [%expect
    {|
    (TLetRec(
        fact: (int -> int),
        (TFun: (int -> int) (
            (n: int),
            (TIfThenElse: int
                ((Eq: (int -> (int -> bool)) (
                    (n: int),
                    (TConst((CInt 1): int))
                )),
                (TConst((CInt 1): int)),
                (Mul: (int -> (int -> int)) (
                    (TApp: int (
                        (fact: (int -> int)),
                        (n: int)
                    )),
                    (TApp: int (
                        (fact: (int -> int)),
                        (Sub: (int -> (int -> int)) (
                            (n: int),
                            (TConst((CInt 1): int))
                        ))
                    ))
                ))
            ))
        ))
    ))
|}]
;;

let%expect_test _ =
  let open Jaml_lib.Ast in
  let _ =
    let e =
      [ ELet
          ( "fac"
          , EFun
              ( "n"
              , ELetRecIn
                  ( "fact"
                  , EFun
                      ( "n"
                      , EFun
                          ( "acc"
                          , EIfThenElse
                              ( EBinop (Lt, EVar "n", EConst (CInt 1))
                              , EVar "acc"
                              , EApp
                                  ( EApp
                                      ( EVar "fact"
                                      , EBinop (Sub, EVar "n", EConst (CInt 1)) )
                                  , EBinop (Mul, EVar "acc", EVar "n") ) ) ) )
                  , EApp (EApp (EVar "fact", EVar "n"), EConst (CInt 1)) ) ) )
      ]
    in
    infer_statements e |> run_infer_statements
  in
  [%expect
    {|
    (TLet(
        fac: (int -> int),
        (TFun: (int -> int) (
            (n: int),
            (TLetRecIn(
                fact: (int -> (int -> int)),
                (TFun: (int -> (int -> int)) (
                    (n: int),
                    (TFun: (int -> int) (
                        (acc: int),
                        (TIfThenElse: int
                            ((Lt: (int -> (int -> bool)) (
                                (n: int),
                                (TConst((CInt 1): int))
                            )),
                            (acc: int),
                            (TApp: int (
                                (TApp: (int -> int) (
                                    (fact: (int -> (int -> int))),
                                    (Sub: (int -> (int -> int)) (
                                        (n: int),
                                        (TConst((CInt 1): int))
                                    ))
                                )),
                                (Mul: (int -> (int -> int)) (
                                    (acc: int),
                                    (n: int)
                                ))
                            ))
                        ))
                    ))
                )),
                (TApp: int (
                    (TApp: (int -> int) (
                        (fact: (int -> (int -> int))),
                        (n: int)
                    )),
                    (TConst((CInt 1): int))
                ))
            ))
        ))
    ))
|}]
;;

let%expect_test _ =
  let open Jaml_lib.Ast in
  let _ =
    let e =
      [ ELetRec ("fix", EFun ("f", EApp (EVar "f", EApp (EVar "fix", EVar "f")))) ]
    in
    infer_statements e |> run_infer_statements
  in
  [%expect
    {|
    (TLetRec(
        fix: (('a -> 'a) -> 'a),
        (TFun: (('a -> 'a) -> 'a) (
            (f: ('a -> 'a)),
            (TApp: 'a (
                (f: ('a -> 'a)),
                (TApp: 'a (
                    (fix: (('a -> 'a) -> 'a)),
                    (f: ('a -> 'a))
                ))
            ))
        ))
    ))
|}]
;;

let%expect_test _ =
  let open Jaml_lib.Ast in
  let _ =
    let e =
      [ ELetRec
          ( "fix"
          , EFun
              ( "f"
              , EFun
                  ("eta", EApp (EApp (EVar "f", EApp (EVar "fix", EVar "f")), EVar "eta"))
              ) )
      ]
    in
    infer_statements e |> run_infer_statements
  in
  [%expect
    {|
    (TLetRec(
        fix: ((('a -> 'b) -> ('a -> 'b)) -> ('a -> 'b)),
        (TFun: ((('a -> 'b) -> ('a -> 'b)) -> ('a -> 'b)) (
            (f: (('a -> 'b) -> ('a -> 'b))),
            (TFun: ('a -> 'b) (
                (eta: 'a),
                (TApp: 'b (
                    (TApp: ('a -> 'b) (
                        (f: (('a -> 'b) -> ('a -> 'b))),
                        (TApp: ('a -> 'b) (
                            (fix: ((('a -> 'b) -> ('a -> 'b)) -> ('a -> 'b))),
                            (f: (('a -> 'b) -> ('a -> 'b)))
                        ))
                    )),
                    (eta: 'a)
                ))
            ))
        ))
    ))
|}]
;;
