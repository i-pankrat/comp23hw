open CreeperML.Parser_interface.ParserInterface
open CreeperML.Infer.Infer
open CreeperML.Type_ast.InferTypeUtils
open CreeperML.Type_ast.TypeAstUtils
open CreeperML.Type_ast.TypeAst

let int_const = t_ground t_int |> with_lvls 0 0
let bool_const = t_ground t_bool |> with_lvls 0 0

(* int -> (int -> bool) *)
let lr =
  let arr = t_arrow int_const bool_const |> with_lvls 0 0 in
  let lr = t_arrow int_const arr |> with_lvls 0 0 in
  ("<=", lr)

(* int -> (int -> int) *)
let minus =
  let arr = t_arrow int_const int_const |> with_lvls 0 0 in
  let minus = t_arrow int_const arr |> with_lvls 0 0 in
  ("-", minus)

let mul = ("*", snd minus)
let env = [ lr; minus; mul ]

let () =
  match from_channel stdin with
  | Error msg -> Printf.printf "%s" msg
  | Ok p -> (
      match top_infer env p with
      | Error msg -> Printf.printf "%s" msg
      | Ok p ->
          List.hd p |> fun { rec_f = _; l_v; body = _ } ->
          ty_typ l_v |> pp_ty Format.std_formatter)
