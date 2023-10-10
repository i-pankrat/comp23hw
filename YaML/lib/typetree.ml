type prim =
  | Int
  | Bool
[@@deriving show { with_path = false }, eq]

type ty =
  | Tyvar of int
  | Prim of prim
  | Arrow of ty * ty
[@@deriving show { with_path = false }]

(* Constructors for ground types *)
let tyint = Prim Int
let tybool = Prim Bool

(* Constructors *)
let arrow l r = Arrow (l, r)
let var_typ x = Tyvar x
