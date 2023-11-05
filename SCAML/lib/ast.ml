(** Copyright 2023-2024, Grigory Aseev and Matvey Kalashnikov *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

type id = string [@@deriving eq, show { with_path = false }]

type bin_op =
  | Add (** + *)
  | Sub (** - *)
  | Mul (** * *)
  | Div (** / *)
  | Mod (** % *)
  | Less (** < *)
  | Leq (** <= *)
  | Gre (** > *)
  | Geq (** >= *)
  | Eq (** = *)
  | Neq (** <> *)
  | And (** && *)
  | Or (** || *)
[@@deriving show { with_path = false }]

type const =
  | CBool of bool (** true *)
  | CInt of int (** 777 *)
  | CUnit (** () *)
[@@deriving show { with_path = false }]

type pattern =
  | PWild (** _ *)
  | PConst of const (** 1 *)
  | PVar of id (** abc *)
[@@deriving show { with_path = false }]

type expr =
  | EConst of const (** 1 *)
  | EBinOp of bin_op * expr * expr (** 1 + 1 *)
  | EVar of id (**  abc *)
  | EIf of expr * expr * expr (** if x > 0 then x else 0 *)
  | EFun of pattern * expr (** fun x -> x * 2 *)
  | ELetIn of bool * id * expr * expr (** let [rec] f x = e in e' *)
  | EApp of expr * expr (** f x *)
[@@deriving show { with_path = false }]

type binding = ELet of bool * id * expr (** let [rec] f x = e *)
[@@deriving show { with_path = false }]

(** e1 ;; e2 ;; ... ;; en;; *)
type program = binding list [@@deriving show { with_path = false }]
