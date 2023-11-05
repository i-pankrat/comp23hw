(** Copyright 2023-2024, Arthur Alekseev and Starcev Matvey *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

let lr =
  let open CreeperML.Type_ast.InferTypeUtils in
  let int_const = t_ground t_int |> with_lvls 0 0 in
  let bool_const = t_ground t_bool |> with_lvls 0 0 in
  let arr = t_arrow int_const bool_const |> with_lvls 0 0 in
  let lr = t_arrow int_const arr |> with_lvls 0 0 in
  ("<=", lr)

let mi =
  let open CreeperML.Type_ast.InferTypeUtils in
  let int_const = t_ground t_int |> with_lvls 0 0 in
  let arr = t_arrow int_const int_const |> with_lvls 0 0 in
  let mi = t_arrow int_const arr |> with_lvls 0 0 in
  ("-", mi)

let ml =
  let open CreeperML.Type_ast.InferTypeUtils in
  let int_const = t_ground t_int |> with_lvls 0 0 in
  let arr = t_arrow int_const int_const |> with_lvls 0 0 in
  let ml = t_arrow int_const arr |> with_lvls 0 0 in
  ("*", ml)

let () =
  match
    CreeperML.Parser_interface.ParserInterface.from_string
      (* {|let f x = let g y = x - y in g|} *)
      {|let rec fac n = if n <= 0 then 1 else fac (n - 1)|}
  with
  | Ok p -> (
      (* List.iter
         (fun l ->
           CreeperML.Parser_ast.ParserAst.show_loc_let_binding l
           |> Printf.printf "%s")
         p; *)
      let p = CreeperML.Infer.Infer.top_infer [ lr; mi; ml ] p in
      match p with
      | Ok e ->
          CreeperML.Type_ast.TypeAst.show_typ_program
            CreeperML.Type_ast.TypeAst.pp_ty e
          |> Printf.printf "%s"
      | Error err -> print_endline err)
  | Error err -> print_endline err
