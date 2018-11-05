(* File MiniOOL.ml *)
open Parsing;;
try
  let lexbuf = Lexing.from_channel stdin
  and err_str = "\027[31;1mError\027[0m: " in
  while true do
    (try
      print_string "# "; flush stdout;
      MiniOOL_parser.prog MiniOOL_lex.token lexbuf
    with
    | Failure msg -> print_endline (err_str ^ msg)
    | MiniOOL_parser.Error ->
      Printf.fprintf stderr
        "%sSyntax error (syntax error at offset %d)\n%!" err_str
        (Lexing.lexeme_start lexbuf)
    | MiniOOL_lex.Error msg ->
      Printf.fprintf stderr "%sLexing error (%s)\n%!" err_str msg);
    Lexing.flush_input lexbuf;
    clear_parser ();
  done
with MiniOOL_lex.Eof -> print_string "\nEnd of file reached\nExiting\n"; ()
;;
