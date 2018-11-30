(** Utilities for for MiniOOL
    @author Zachary Ferguson *)


(** Prints a warning message with the yellow "Wraning:" text prepended.
    @param msg Warning message string to print. *)
let print_warning msg =
  Printf.fprintf stderr "\027[33;1mWarning\027[0m: %s\n%!" msg;;


(** Colorful logo of MiniOOL including some help and version information. *)
let logo = "
\027[36;1m ╭─╮╭─╮╭─╮      ╭─╮\027[0m\027[31;1m╭────╮\027[0m\027[32;1m╭────╮\027[0m\027[35;1m╭─╮  \027[0m │ Type \"\\help\" for help
\027[36;1m │    │╰─╯╭────╮╰─╯\027[0m\027[31;1m│ ╭╮ │\027[0m\027[32;1m│ ╭╮ │\027[0m\027[35;1m│ │  \027[0m │ Type \"\\exit\" or press Ctrl-D to exit
\027[36;1m │ ╭╮ │╭─╮│ ╭╮ │╭─╮\027[0m\027[31;1m│ ││ │\027[0m\027[32;1m│ ││ │\027[0m\027[35;1m│ │  \027[0m │ Honors Programming Languages
\027[36;1m │ ││ ││ ││ ││ ││ │\027[0m\027[31;1m│ ╰╯ │\027[0m\027[32;1m│ ╰╯ │\027[0m\027[35;1m│ ╰─╮\027[0m │ Version Fall 2018
\027[36;1m ╰─╯╰─╯╰─╯╰─╯╰─╯╰─╯\027[0m\027[31;1m╰────╯\027[0m\027[32;1m╰────╯\027[0m\027[35;1m╰───╯\027[0m │ Created by Zachary Ferguson
";;

let help ="
Commands:
 • var x                   (declare a variable x)
 • e₀(e₁)                  (call procedure evaluated from e₀ with an argument
                            of e₁ evaluated, procedures do not return a value
                            explicitly and are not expressions)
 • malloc(x)               (allocate x on the heap, making it an object with
                            unlimited fields)
 • x = e                   (assign the value of the expression e to x)
 • e₀.e₁ = e₂              (assign the value of the expression e₂ to the field
                            e₁ of variable e₀)
 • skip                    (do nothing)
 • {C₀; C₁; ...}           (execute commands sequentially)
 •   while b C             (while b evaluates to true execute the command C)
   | while b do C
 •   if b C₀ else C₁       (if b evaluates to true execute the command C₀ else
                            execute C₁)
 • | if b then C₀ else C₁
 • {C₀ ||| C₁}             (execute commands C₀ and C₁ in parallel)
 • atom(C)                 (execute commands C without executing any other
                            command)

Expressions:
 • (['A'-'Z'])(['a'-'z'] | ['A'-'Z'] | ['0'-'9'] | '_')* (fields)
 • [0-9]+                                                (integers)
 •   e₀ + e₁ | e₀ - e₁ | e₀ * e₁ | e₀ / e₁ | e₀ % e₁     (arithmetic operations)
   | e₀ mod e₁ | -e₀
 • null                                                  (null location)
 • (['a'-'z'])(['a'-'z'] | ['A'-'Z'] | ['0'-'9'] | '_')* (variables)
 • e₀.e₁                                                 (field access)
 • proc y: C                                             (procedure declaration)

Boolean Expressions:
 • true | false                                    (boolean literals)
 •   e₀ == e₁ | e₀ != e₁ | e₀ <> e₁ | e₀ < e₁      (comparison operations)
   | e₀ <= e₁ | e₀ > e₁ | e₀ >= e₁
 •   not b | !b | b₀ && b₁ | b₀ and b₁ | b₀ || b₁  (boolean operations)
   | b₀ or b₁

Interpreter Directives:
 • \\exit | \\quit  (exit MiniOOL)
 • \\help:          (display helpful information about and how to use MiniOOL)
 • \\clear:         (clear the stack and heap of all values (this will also
                     reset the unique naming of variables))
 • \\verbose:       (verbosely display the steps of interpretation including
                     the abstract syntax tree (same as the command line option
                     --verbose))
 • \\quite:         (the opposite of \\verbose, turn off verbose information)
"
