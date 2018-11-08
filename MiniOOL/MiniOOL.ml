(*
Main cmd-line interface for MiniOOL
Writen by Zachary Ferguson
*)
open Parsing;;

print_endline "MiniOOL (Fall 2018)\nHonors Programming Languages\nCreated by Zachary Ferguson";;

try
  let lexbuf = Lexing.from_channel stdin (* Parse from the standard input *)
  and err_str = "\027[31;1mError\027[0m: " in (* Colored "Error: " *)
  while true do (* Loop until end of file (^D) *)
    (try
       print_string "# "; flush stdout; (* Print the prompt # *)
       (* Parse the standard input *)
       Parser.prog Lexer.token lexbuf
     with (* Handle errors incountered *)
     | Failure msg -> Printf.fprintf stderr "%s%s\n%!" err_str msg
     | Parser.Error ->
       Printf.fprintf stderr
         "%sSyntax error (syntax error at offset %d)\n%!" err_str
         (Lexing.lexeme_start lexbuf)
     | Lexer.Error msg ->
       Printf.fprintf stderr "%sLexing error (%s)\n%!" err_str msg);
    Lexing.flush_input lexbuf; (* Clear the input buffer *)
    clear_parser ();
  done
with Lexer.Eof -> print_string "\nEnd of file reached\nExiting\n"; ();;