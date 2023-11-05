(** Copyright 2022-2023, Rustam Shangareev and Danil Yevdokimov*)

(** SPDX-License-Identifier: LGPL-2.1 *)

open ZRusML_lib.Code_inferencer

let _ =
  let prog = Stdio.In_channel.input_all Caml.stdin in
  Format.printf "%a_______\n" inference prog
;;
