(** Main cmd-line interface for MiniOOL
    @author Zachary Ferguson *)
open Parsing;;

(** Parse command-line arguments. *)
Arg.parse
  [("--verbose",
    Arg.Unit (fun () -> Flags.verbose := true),
    "Verbosly print steps of interpretation.");
   ("--quite",
    Arg.Unit (fun () -> Flags.quite := true;),
    "Only print the input and results.");]
  (fun s -> ())
  "\nUsage: MiniOOL [--verbose] [--quite] [-help|--help]";;

(** Optionally print logo and information. *)
if not !(Flags.quite) then
  print_endline "
\027[36;1m ╭─╮╭─╮╭─╮      ╭─╮\027[0m\027[31;1m╭────╮\027[0m\027[32;1m╭────╮\027[0m\027[35;1m╭─╮  \027[0m │ Type \"\\help\" for help
\027[36;1m │    │╰─╯╭────╮╰─╯\027[0m\027[31;1m│ ╭╮ │\027[0m\027[32;1m│ ╭╮ │\027[0m\027[35;1m│ │  \027[0m │ Type \"\\exit\" or press Ctrl-D to exit
\027[36;1m │ ╭╮ │╭─╮│ ╭╮ │╭─╮\027[0m\027[31;1m│ ││ │\027[0m\027[32;1m│ ││ │\027[0m\027[35;1m│ │  \027[0m │ Honors Programming Languages
\027[36;1m │ ││ ││ ││ ││ ││ │\027[0m\027[31;1m│ ╰╯ │\027[0m\027[32;1m│ ╰╯ │\027[0m\027[35;1m│ ╰─╮\027[0m │ Version Fall 2018
\027[36;1m ╰─╯╰─╯╰─╯╰─╯╰─╯╰─╯\027[0m\027[31;1m╰────╯\027[0m\027[32;1m╰────╯\027[0m\027[35;1m╰───╯\027[0m │ Created by Zachary Ferguson
";;

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
