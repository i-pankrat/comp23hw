(** Copyright 2023-2024, Ilya Pankratov, Maxim Drumov *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

type const =
  | CInt of int (** 1 2 3 *)
  | CBool of bool (** true, false *)
[@@deriving show { with_path = false }]

(** Binary operations type *)
type bin_op =
  | Add
  | Sub
  | Div
  | Mul
  | Xor
  | And
  | Or
  | Eq
  | Neq
  | Gt
  | Lt
  | Gte
  | Lte
[@@deriving show { with_path = false }]

(** Expression type *)
type expr =
  | EConst of const (** An expression for the constant *)
  | EVar of string (** An expression for the variables *)
  | EBinop of bin_op * expr * expr
  (** An expression for the binary operations: +, -, *, / ... *)
  | EApp of expr * expr (** An expression for the function application to the arguments *)
  | EIfThenElse of expr * expr * expr
  (** An expression for condition statement: if expr then expr else expr *)
  | ELetIn of string * expr * expr
  (** An expression for let in declaration: let id = expr in expr *)
  | ELetRecIn of string * expr * expr
  (** An expression for let rec in declaration: let rec id = expr in expr *)
  | EFun of string * expr (** An expression for function: fun id -> expr *)
[@@deriving show { with_path = false }]

(** Binding type *)
type bindings =
  | ELet of string * expr (** An expression for let declaration: let id = expr *)
  | ELetRec of string * expr
  (** An expression for let rec declaration: let rec id = expr *)
[@@deriving show { with_path = false }]

(** Statements type *)
type statements = bindings list [@@deriving show { with_path = false }]
