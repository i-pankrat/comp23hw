(** Copyright 2023-2024, Ilya Pankratov, Maxim Drumov *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

type immexpr =
  | ImmNum of int
  | ImmBool of bool
  | ImmId of string
[@@deriving show { with_path = false }]

type cexpr =
  | CPlus of immexpr * immexpr
  | CMinus of immexpr * immexpr
  | CDivide of immexpr * immexpr
  | CMultiply of immexpr * immexpr
  | CXor of immexpr * immexpr
  | CAnd of immexpr * immexpr
  | COr of immexpr * immexpr
  | CEq of immexpr * immexpr
  | CNeq of immexpr * immexpr
  | CGt of immexpr * immexpr
  | CLt of immexpr * immexpr
  | CGte of immexpr * immexpr
  | CLte of immexpr * immexpr
  | CApp of immexpr * immexpr
  | CImmExpr of immexpr
[@@deriving show { with_path = false }]

type aexpr =
  | ALet of string * cexpr * aexpr
  | ALetRec of string * cexpr * aexpr
  | AIfThenElse of aexpr * aexpr * aexpr
  | ACEexpr of cexpr
[@@deriving show { with_path = false }]

type anfexpr =
  | AnfLet of string * string list * aexpr
  | AnfLetRec of string * string list * aexpr
[@@deriving show { with_path = false }]

type anfstatements = anfexpr list [@@deriving show { with_path = false }]
