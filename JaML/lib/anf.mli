(** Copyright 2023-2024, Ilya Pankratov, Maxim Drumov *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

type immexpr =
  | ImmNum of int (** ..., -1, 0, 1, ... *)
  | ImmBool of bool (** true, false *)
  | ImmId of string (** identifiers or variables *)
  | ImmTuple of immexpr list (** (1, 2, a, b) *)

type cexpr =
  | CBinOp of Ast.bin_op * immexpr * immexpr (** Binary operation *)
  | CApp of immexpr * immexpr list (** Apply function to its arguments *)
  | CTake of immexpr * int (** Take(tuple, 0) *)
  | CMakeClosure of immexpr * int * int * immexpr list
  | CImmExpr of immexpr (** immexpr *)

type aexpr =
  | ALet of string * cexpr * aexpr (** let name = cexpr in aexpr *)
  | AIfThenElse of cexpr * aexpr * aexpr (** if aexpr1 then aexpr2 else aexpe3 *)
  | ACEexpr of cexpr (** cexpr *)

(** Anf binding type (top level declarations) *)
type anfexpr =
  | AnfLetVar of string * aexpr (** let name = aexpr *)
  | AnfLetFun of string * string list * aexpr
  (** let name arg1, arg2, ..., argn = aexpr. Invariant: there's more than one argument *)
  | AnfLetRec of string * string list * aexpr
  (** let rec name arg1, arg2, ..., argn = aexpr. Invariant: there's more than one argument *)

(** Statements type (list of top level declarations) *)
type anfstatements = anfexpr list
