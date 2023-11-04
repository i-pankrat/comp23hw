(** Copyright 2023-2024, Ilya Pankratov, Maxim Drumov *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

(** Lambda lifting of statements *)
val lambda_lift : Typedtree.tbinding list -> Toplevel.llbinding list
