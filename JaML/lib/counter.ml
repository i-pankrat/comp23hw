(** Copyright 2023-2024, Ilya Pankratov, Maxim Drumov *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

module Counter = struct
  let count = ref 0

  let count_next () =
    incr count;
    !count
  ;;

  let genid x =
    incr count;
    x ^ string_of_int !count
  ;;
end
