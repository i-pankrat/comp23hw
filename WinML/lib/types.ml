(** Copyright 2023-2024, Vyacheslav Buchin and Artur Gagin *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

type typ =
  | TVar of int
  | TInt
  | TBool
  | TArrow of typ * typ
[@@deriving show]

module TypeVars = Set.Make (Int)

module Subst : sig
  type t

  val empty : t
  val find_opt : int -> t -> typ option
  val remove : int -> t -> t
  val compose : (t -> typ -> typ) -> t -> t -> t
  val singleton : int -> typ -> t
  val from_seq : (int * typ) Seq.t -> t
  val pp : Format.formatter -> t -> unit
end = struct
  module M = Map.Make (Int)

  type t = typ M.t

  let empty = M.empty
  let find_opt = M.find_opt
  let remove = M.remove
  let singleton = M.singleton
  let compose apply s1 s2 = M.map (apply s1) s2 |> M.union (fun _ t _ -> Some t) s1
  let from_seq s = M.add_seq s empty
  let pp ppf s = M.iter (fun k v -> Format.fprintf ppf "%d -> %s@\n" k (show_typ v)) s
end

module type Parameterized = sig
  type t

  val ftv : t -> TypeVars.t
  val apply : Subst.t -> t -> t
end

module Typ : sig
  include Parameterized

  val from_typ : typ -> t
  val to_typ : t -> typ
end = struct
  type t = typ

  let rec ftv = function
    | TVar i -> TypeVars.singleton i
    | TArrow (a, b) ->
      let a_vars = ftv a in
      let b_vars = ftv b in
      TypeVars.union a_vars b_vars
    | TInt | TBool -> TypeVars.empty
  ;;

  let rec apply s = function
    | TVar i as v -> Subst.find_opt i s |> Option.value ~default:v
    | TArrow (a, b) ->
      let a_applied = apply s a in
      let b_applied = apply s b in
      TArrow (a_applied, b_applied)
    | t -> t
  ;;

  let from_typ t = t
  let to_typ = from_typ
end