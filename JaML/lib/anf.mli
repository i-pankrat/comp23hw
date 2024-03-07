(** Copyright 2023-2024, Ilya Pankratov, Maxim Drumov *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

type arg =
  | Used of string (** Represent usual argument *)
  | Unused (** Represent unused argument (wildcard for example) *)

type immexpr =
  | ImmNum of int (** ..., -1, 0, 1, ... *)
  | ImmBool of bool (** true, false *)
  | ImmId of string (** identifiers or variables *)

type cexpr =
  | CBinOp of Ast.bin_op * immexpr * immexpr (** Binary operation *)
  | CApp of immexpr * immexpr list (** Apply function to its arguments *)
  | CTuple of immexpr list (** (1, 2, a, b) *)
  | CTake of immexpr * int (** Take(tuple, 0) *)
  | CMakeClosure of (immexpr * int * int * immexpr list)
  (** Used in partial application *)
  | CAddArgsToClosure of immexpr * int * immexpr list (** Add arguments to closure *)
  | CIfThenElse of immexpr * aexpr * aexpr (** if immexpr then aexpr2 else aexpr3 *)
  | CImmExpr of immexpr (** immexpr *)

and aexpr =
  | ALet of string * cexpr * aexpr (** let name = cexpr in aexpr *)
  | ACEexpr of cexpr (** cexpr *)

(** Anf binding type (top level declarations) *)
type anfexpr =
  | AnfLetFun of string * arg list * aexpr
  (** let name arg1, arg2, ..., argn = aexpr. It's possible to have zero args *)

(** Statements type (list of top level declarations) *)
type anfstatements = anfexpr list
