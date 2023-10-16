type immexpr =
  | ImmNum of int
  | ImmString of string
  | ImmChar of char
  | ImmBool of bool
  | ImmUnit
  | ImmId of string

type cexpr =
  | CAdd of immexpr * immexpr
  | Mul of immexpr * immexpr
  | Div (** / *)
  | Eq (** = *)
  | NEq (** != *)
  | GT (** > *)
  | GTE (** >= *)
  | LT (** < *)
  | LTE (** <= *)
  | AND (** && *)
  | OR (** || *)