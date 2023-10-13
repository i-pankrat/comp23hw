type const =
  | CInt of int
  | CBool of bool
[@@deriving show { with_path = false }]

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

type expr =
  | EConst of const
  | EVar of string
  | EBinop of bin_op * expr * expr
  | EApp of expr * expr
  | EIfThenElse of expr * expr * expr
  | ELet of string * expr
  | ELetRec of string * expr
  | ELetIn of string * expr * expr
  | ELetRecIn of string * expr * expr
  | EFun of string * expr
[@@deriving show { with_path = false }]
