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

module StateResultOrResultStateMonad : sig
  type ('a, 'error) t

  val return : 'a -> ('a, 'error) t
  val fail : 'error -> ('a, 'error) t
  val bind : ('a, 'error) t -> ('a -> ('b, 'error) t) -> ('b, 'error) t
  val ( let* ) : ('a, 'error) t -> ('a -> ('b, 'error) t) -> ('b, 'error) t
  val fresh : string -> (string, 'error) t
  val run : ('a, 'error) t -> ('a, 'error) result
  val monad_fold : init:'a -> f:('a -> 'b -> ('a, 'error) t) -> 'b list -> ('a, 'error) t
  val monad_map : 'a list -> f:('a -> ('b, 'c) t) -> ('b list, 'c) t
end
