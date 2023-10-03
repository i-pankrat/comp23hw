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

(* It would be great to remove copy paste with Equal, NotEqual, ...*)
type int_binop =
  | Plus of tnumber * tnumber
  | Minus of tnumber * tnumber
  | Slash of tnumber * tnumber
  | Aster of tnumber * tnumber
  | Equal of tnumber * tnumber
  | NotEqual of tnumber * tnumber
  | Gt of tnumber * tnumber
  | Lt of tnumber * tnumber
  | Gte of tnumber * tnumber
  | Lte of tnumber * tnumber

type bool_binop =
  | Xor of tboolean * tboolean
  | And of tboolean * tboolean
  | Or of tboolean * tboolean
  | Equal of tboolean * tboolean
  | NotEqual of tboolean * tboolean
  | Gt of tboolean * tboolean
  | Lt of tboolean * tboolean
  | Gte of tboolean * tboolean
  | Lte of tboolean * tboolean

(* TODO: For now it's not ready to implement good typed binary operations
   because we can express Equal of TBooleand * TInteger *)
type tbinop =
  | BoolBinop of bool_binop
  | IntBinop of int_binop

type let_binding_info =
  { name : string
  ; let_type : string (* TODO: type for types *)
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
