(** Copyright 2024, Ilya Pankratov, Maxim Drumov *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open Base
open Result

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

module StateResultOrResultStateMonad = struct
  type ('a, 'error) t = int -> ('a, 'error) Result.t * int

  let return x : ('a, 'error) t = fun state -> Result.return x, state
  let fail : 'error -> ('a, 'error) t = fun e state -> Result.fail e, state

  let bind (t : ('a, 'error) t) (f : 'a -> ('b, 'error) t) : ('b, 'error) t =
    fun state ->
    let a, state = t state in
    match a with
    | Result.Error x -> Error x, state
    | Ok x -> f x state
  ;;

  let ( let* ) x f = bind x f

  let fresh name : (string, 'error) t =
    fun state -> Result.Ok (name ^ string_of_int state), state + 1
  ;;

  let run (m : ('a, 'error) t) = fst (m 1)

  let monad_fold ~init ~f l : ('a, 'error) t =
    List.fold
      l
      ~f:(fun acc x ->
        let* acc = acc in
        f acc x)
      ~init:(return init)
  ;;

  let monad_map l ~f =
    let rec helper acc = function
      | [] -> return (List.rev acc)
      | hd :: tl ->
        let* res = f hd in
        helper (res :: acc) tl
    in
    helper [] l
  ;;
end
