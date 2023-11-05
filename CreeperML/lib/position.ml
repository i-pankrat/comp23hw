(** Copyright 2023-2024, Arthur Alekseev and Starcev Matvey *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

module Position = struct
  type t = Lexing.position
  type loc = { start_p : t; end_p : t }

  let show_lex_position : t -> string =
   fun { pos_fname = fname; pos_lnum = lnum; pos_bol = bol; pos_cnum = cnum } ->
    Printf.sprintf "[%s: %d %d %d]" fname lnum bol cnum

  let show_loc loc =
    let start_p = show_lex_position loc.start_p in
    let end_p = show_lex_position loc.end_p in
    Printf.sprintf "{start_p: %s; end_p: %s}" start_p end_p

  let pp_loc fmt loc = show_loc loc |> Format.pp_print_string fmt

  type 'a position = { value : 'a; pos : loc }
  [@@deriving show { with_path = false }]

  let value { value = v; pos = _ } = v
  let position { value = _; pos = p } = p
  let start_p { start_p = s; end_p = _ } = s
  let end_p { start_p = _; end_p = e } = e
  let start_position p = position p |> start_p
  let end_position p = position p |> end_p
  let with_position start_p end_p v = { value = v; pos = { start_p; end_p } }
end
