open Typetree

type tboolean =
  | BoolVar of string
  | BoolConst of bool

type tnumber =
  | IntVar of string
  | IntConst of int

type ground_value =
  | Tboolean of tboolean
  | TInteger of tnumber

type poly_value = { name : string }

type tvalue =
  | GValue of ground_value
  | PValue of poly_value

type 'a poly_binop =
  | NotEqual of 'a * 'a
  | Gt of 'a * 'a
  | Lt of 'a * 'a
  | Gte of 'a * 'a
  | Lte of 'a * 'a

type int_binop =
  | Plus of tnumber * tnumber
  | Minus of tnumber * tnumber
  | Slash of tnumber * tnumber
  | Aster of tnumber * tnumber
  | OrderBinop of tnumber poly_binop

type bool_binop =
  | Xor of tboolean * tboolean
  | And of tboolean * tboolean
  | Or of tboolean * tboolean
  | OrderBinop of bool_binop poly_binop

type tbinop =
  | BoolBinop of bool_binop
  | IntBinop of int_binop

type let_binding_info =
  { name : string
  ; let_type : ty (* TODO: type for types *)
  }

type texpr =
  | Tconst of tvalue
  | TVar of poly_value
  | TBinop of tbinop
  | TApp of texpr * texpr
  | TLen of let_binding_info * texpr
  | TLetRec of let_binding_info * texpr
  | TLetIn of let_binding_info * texpr * texpr
  | TLetRecIn of let_binding_info * texpr * texpr
  | TFun of tvalue * texpr
