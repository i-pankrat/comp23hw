(** Copyright 2024, Ilya Pankratov, Maxim Drumov *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open Base

module VariableNameGeneratorMonad = struct
  type 'a t = int -> 'a * int

  let return a : 'a t = fun state -> a, state

  let bind (t : 'a t) ~(f : 'a -> 'b t) : 'b t =
    fun state ->
    let a, new_state = t state in
    let b, final_state = f a new_state in
    b, final_state
  ;;

  let ( let* ) x f = bind x ~f
  let fresh name : string t = fun state -> name ^ string_of_int state, state + 1
  let run (m : 'a t) = fst (m 1)

  let monad_fold ~init ~f l : 'a t =
    List.fold
      l
      ~f:(fun acc x ->
        let* acc = acc in
        f acc x)
      ~init:(return init)
  ;;
end
