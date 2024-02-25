(** Copyright 2024, Ilya Pankratov, Maxim Drumov *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

module VariableNameGeneratorMonad : sig
  type 'a t

  val return : 'a -> 'a t
  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
  val fresh : string -> string t
  val run : 'a t -> 'a
  val monad_fold : init:'a -> f:('a -> 'b -> 'a t) -> 'b list -> 'a t
end
