open CreeperML.Parser_interface.ParserInterface
open CreeperML.Parser_ast.ParserAst

let () =
  match from_channel stdin with
  | Error msg -> Printf.printf "%s" msg
  | Ok p -> pp_program Format.std_formatter p
