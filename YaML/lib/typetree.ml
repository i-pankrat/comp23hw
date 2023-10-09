type ty =
  | Tyvar of int
  | Prim of string (* Constsnts: bool, int ...*)
  | Arrow of ty * ty

(* Constructors  of ground types*)

let tybool = Prim "bool"
let tyint = Prim "int"
