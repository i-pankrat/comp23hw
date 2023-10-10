type ty =
  | Tyvar of int
  | Prim of string (* Constsnts: bool, int ...*)
  | Arrow of ty * ty
[@@deriving show { with_path = false }]

(* Constructors  of ground types*)
let tybool = Prim "bool"
let tyint = Prim "int"

(* Constructors *)
let arrow l r = Arrow (l, r)
let var_typ x = Tyvar x
