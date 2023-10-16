open Yaml_lib
open Format
open Result

module YamlCLIArgs = struct
  type t =
    { infer_type : bool
    ; filename : string
    }

  let usage = "yaml -i -f <file>\nCompile program from stdin"

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

type errors = FileDoesNotExist of string

let pp_error ppf = function
  | FileDoesNotExist file -> fprintf ppf "file '%s' does not exist" file
;;

let pp_statements = Pprinttypedtree.pp_statements "\n" Pprinttypedtree.Brief

let infer_types input =
  let parsed = Parser.parse input in
  match parsed with
  | Ok statements ->
    (match Inferencer.infer statements with
     | Ok typed_statements -> printf "%a\n" pp_statements typed_statements
     | Error err -> printf "%a\n" Inferencer.pp_error err)
  | Error err -> printf "%a\n" Parser.pp_error err
;;

let read_input filename =
  if filename != ""
  then
    if Sys.file_exists filename
    then Ok (In_channel.with_open_bin filename In_channel.input_all)
    else Error (FileDoesNotExist filename)
  else Ok (In_channel.input_all In_channel.stdin)
;;

let () =
  let args = YamlCLIArgs.parse () in
  match read_input args.filename with
  | Ok input when input != "" && args.infer_type -> infer_types input
  | Ok input when input != "" && not args.infer_type ->
    printf "compilation does not support yet"
  | Ok _ -> printf "empty input"
  | Error err -> printf "%a\n" pp_error err
;;
