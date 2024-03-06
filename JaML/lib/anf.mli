(** Copyright 2023-2024, Ilya Pankratov, Maxim Drumov *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

type immexpr =
  | ImmNum of int (** ..., -1, 0, 1, ... *)
  | ImmBool of bool (** true, false *)
  | ImmId of string (** identifiers or variables *)
  | ImmVariable of string (** Represents global variables: let ImmVariable = 2 *)
  | PassFunctionAsArgument of string (** Represent function passed as a argument *)

type cexpr =
  | CBinOp of Ast.bin_op * immexpr * immexpr (** Binary operation *)
  | CApp of immexpr * immexpr list (** Apply function to its arguments *)
  | CTuple of immexpr list (** (1, 2, a, b) *)
  | CTake of immexpr * int (** Take(tuple, 0) *)
  | CMakeClosure of immexpr * immexpr list (** Used in partial application *)
  | CAddArgsToClosure of immexpr * immexpr list (** Add arguments to closure *)
  | CIfThenElse of immexpr * aexpr * aexpr (** if immexpr then aexpr2 else aexpr3 *)
  | CImmExpr of immexpr (** immexpr *)

and aexpr =
  | ALet of string * cexpr * aexpr (** let name = cexpr in aexpr *)
  | ACEexpr of cexpr (** cexpr *)

(** Anf binding type (top level declarations) *)
type anfexpr =
  | AnfLetFun of string * string list * aexpr
  (** let name arg1, arg2, ..., argn = aexpr. It's possible to have zero args *)

(** Statements type (list of top level declarations) *)
type anfstatements = anfexpr list
