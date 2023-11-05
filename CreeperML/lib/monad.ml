(** Copyright 2023-2024, Arthur Alekseev and Starcev Matvey *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)
(*
   module Error = struct
     type t =
       | Unreacheble of string
       |
   end *)

module Result = struct
  type 'a t = ('a, string) Result.t

  let return x = Ok x
  let error msg = Error msg
  let ( >>= ) = Result.bind
  let ( let* ) = ( >>= )

  let ( *> ) : 'a t -> 'b t -> 'b t =
   fun x y -> match x with Ok _ -> y | Error msg -> error msg

  let ( >>| ) : 'a t -> ('a -> 'b) -> 'b t =
   fun x f -> match x with Ok x -> Ok (f x) | Error msg -> error msg
end
