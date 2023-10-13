(** Get subs from type. Using for beautiful printing. *)
val get_ty_subs
  :  (int, int, 'a) Base.Map.t
  -> int
  -> Typetree.ty
  -> (int, int, 'a) Base.Map.t * int

(** Print type using some substitution for types *)
val pp_ty_with_subs
  :  (int, int, 'a) Base.Map.t option
  -> Format.formatter
  -> Typetree.ty
  -> unit

(** Print type *)
val pp_ty : Format.formatter -> Typetree.ty -> unit
