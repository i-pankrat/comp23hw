type const =
  | CInt of int
  | CBool of bool

type bin_op =
  | Plus of const * const
  | Minus of const * const
  | Slash of const * const
  | Aster of const * const
  | Xor of const * const
  | And of const * const
  | Or of const * const
  | Equal of const * const
  | NotEqual of const * const
  | Gt of const * const
  | Lt of const * const
  | Gte of const * const
  | Lte of const * const

type expr =
  | Const of const
  | Binop of bin_op
  | App of expr * expr
  | Let of string * expr * expr
  | LetRec of string * expr * expr
  | LetIn of string * expr * expr
  | LetRecIn of string * expr * expr
  | Fun of expr * expr
