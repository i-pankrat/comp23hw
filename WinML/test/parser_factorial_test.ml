(** Copyright 2023-2024, Vyacheslav Buchin and Artur Gagin *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open Winml.Parse

let rec print_list = function
  | [] -> ()
  | e :: l ->
    print_string e;
    print_string " ";
    print_list l
;;

let print_fac_test_expr = function
  | Winml.Parsed_tree.IfThenElse
      ( BinOp (Eq, Var "n", Const (CInt 1))
      , Const (CInt 1)
      , BinOp
          (Mul, Var "n", Application (Var "fac", BinOp (Sub, Var "n", Const (CInt 1)), []))
      ) ->
    Printf.printf
      "IfThenElse(BinOp(Eq, Var 'n', Const(CInt 1)), Const(CInt 1), BinOp(Mul, Var 'n', \
       Application(Var 'fac', BinOp(Sub, Var 'n', Const(CInt 1)), []))"
  | _ -> print_string "Not what we are expected."
;;

let () =
  match grammar_from_string "let rec fac n = if n == 1 then 1 else n * fac (n - 1)" with
  | Ok (Winml.Parsed_tree.Function _) -> Printf.printf "Not implemented"
  | Ok (Winml.Parsed_tree.RecFunction p) ->
    let s, l, expr = p in
    print_string "RecFunction(";
    Printf.printf "%s," s;
    print_string " [ ";
    print_list l;
    print_string "], ";
    print_fac_test_expr expr;
    print_string ")"
  | Error e -> Printf.printf "%s!" e
;;
