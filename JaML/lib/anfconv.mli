(** Copyright 2023-2024, Ilya Pankratov, Maxim Drumov *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

(** Performs conversion to anf *)
val anf : LL_ast.llbinding list -> Anf.anfexpr list
