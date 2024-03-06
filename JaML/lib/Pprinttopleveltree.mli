(** Copyright 2024, Ilya Pankratov, Maxim Drumov *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open LL_ast
open Stdlib.Format

val pp_llbinding_wt : formatter -> llbinding -> unit
val pp_llstatements_without_types : formatter -> llbinding list -> unit
