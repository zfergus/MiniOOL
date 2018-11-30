(** Main cmd-line interface for MiniOOL
    @author Zachary Ferguson *)
open Parsing;;

let show_logo = ref true;;

(** Parse command-line arguments. *)
Arg.parse
  [("--verbose",
    Arg.Unit (fun () -> Flags.verbose := true),
    "Verbosly print steps of interpretation");
   ("--no-logo",
    Arg.Unit (fun () -> show_logo := false),
    "Do not display the logo and information at start up");]
  (fun s -> ())
  "\nUsage: MiniOOL [--verbose] [--quite] [-help|--help]";;

(** Optionally print logo and information. *)
if !show_logo then print_endline Utils.logo;;

Random.self_init();;

(** Map Ctrl-C to an exception. *)
exception SigIntError;;
Sys.set_signal Sys.sigint
  (Sys.Signal_handle (fun _signum -> raise SigIntError));;

try
  let lexbuf = Lexing.from_channel stdin (** Parse from the standard input *)
  and err_str = "\027[31;1mError\027[0m: " in (* Colored "Error: " *)
  while true do (** Loop until end of file (Ctrl-D) *)
    (try
       Printf.printf "# %!"; (* Print the prompt # *)
       (* Parse the standard input *)
       Parser.prog Lexer.token lexbuf
     with (* Handle errors incountered *)
     | Failure msg -> Printf.fprintf stderr "%s%s\n%!" err_str msg
     | StaticSemantics.VariableOutOfScope msg ->
       Printf.fprintf stderr "%s Variable out of scope (%s)\n%!" err_str msg
     | Parser.Error ->
       Printf.fprintf stderr
         "%sSyntax error (syntax error at offset %d)\n%!" err_str
         (Lexing.lexeme_start lexbuf)
     | Lexer.Error msg ->
       Printf.fprintf stderr "%sLexing error (%s)\n%!" err_str msg
     | Division_by_zero -> Printf.fprintf stderr "%s%s\n%!" err_str
                             "Division by zero"
     | Stack_overflow -> Printf.fprintf stderr "%s%s\n%!" err_str
                           "Stack overflow"
     | SigIntError -> Printf.fprintf stderr "\nInterrupted\n%!");
    Lexing.flush_input lexbuf; (* Clear the input buffer *)
    clear_parser ();
  done
with Lexer.Eof -> print_endline "\nExiting";;
