open Yaml_lib
open Format

module YamlCLIArgs = struct
  type t =
    { infer_type : bool
    ; filename : string
    }

  let usage = "yaml [-i <only-infer-type>] [-f <file>]\nCompile program from stdin"

  let parse () =
    let infer = ref false in
    let file = ref "" in
    let specs =
      [ ( "-i"
        , Arg.Set infer
        , "Infer only the types for the input, do not use the compiler." )
      ; "-f", Set_string file, "Read program from specified file, not from the stdin."
      ]
    in
    let anon _ = () in
    Arg.parse specs anon usage;
    { infer_type = !infer; filename = !file }
  ;;
end

let pp_statements = Pprinttypedtree.pp_statements "\n" Pprinttypedtree.Brief

let infer_types input =
  let parsed = Parser.parse input in
  match parsed with
  | Ok statements ->
    (match Inferencer.infer statements with
     | Ok typed_statements -> printf "%a!" pp_statements typed_statements
     | Error err -> printf "%a!" Inferencer.pp_error err)
  | Error err -> printf "%a!" Parser.pp_error err
;;

let read_whole_file filename = In_channel.with_open_bin filename In_channel.input_all

let () =
  match YamlCLIArgs.parse () with
  | { YamlCLIArgs.infer_type; filename } when infer_type && filename == "" ->
    let input = read_line () in
    infer_types input
  | { YamlCLIArgs.infer_type; filename } when infer_type && filename != "" ->
    if Sys.file_exists filename
    then (
      let input = read_whole_file filename in
      infer_types input)
    else printf "File %s does not exist" filename
  | _ -> printf "Сompilation is not currently implemented"
;;
