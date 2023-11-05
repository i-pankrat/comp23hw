(** Copyright 2023-2024, Arthur Alekseev and Starcev Matvey *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

module Position : sig
  (* Lexing.position *)
  type t = Lexing.position

  (* position of start and end symbol in code *)
  type loc = { start_p : t; end_p : t }

  val show_loc : loc -> string
  val pp_loc : Format.formatter -> loc -> unit

  (* something with it position in code *)
  type 'a position = { value : 'a; pos : loc }

  val show_position : (Format.formatter -> 'a -> unit) -> 'a position -> string

  val pp_position :
    (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a position -> unit

  (* get value *)
  val value : 'a position -> 'a

  (* get position *)
  val position : 'a position -> loc

  (* get position of start symbol *)
  val start_position : 'a position -> t

  (* get position of end symbol *)
  val end_position : 'a position -> t

  (* add position to value *)
  val with_position : t -> t -> 'a -> 'a position
end
