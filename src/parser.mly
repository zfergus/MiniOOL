(** Parser for MiniOOL. Takes the tokens from the lexer and builds an abstract
    syntax tree for of the input program. Runs the input program by checking
    the scope, uniquely renaming vairables, and evaluating the commands.
    @author Zachary Ferguson *)

%{ (* header *)
open AbstractSyntaxTree;;
open ProgramString;;
open AbstractSyntaxTreeString;;
open StaticSemantics;;
open OperationalSemantics;;

type directive = string;;

let scope = ref (Hashtbl.create 10);;
let stack = ref (Hashtbl.create 10);;
let heap = ref (Hashtbl.create 10);;

(** Run the given program by checking the scope, uniquely renaming vairables,
    and evaluating the commands.
    @param ast The abstract syntax tree of the program to run. *)
let run_program ast =
  (* Make copies incase of failure. *)
  let scope' = copy_scope !scope and stack' = Hashtbl.copy !stack and heap' = Hashtbl.copy !heap in
  (* Check the scope of the varable and uniquely rename them. *)
  check_cmds_in_scope scope' ast;
  (* Echo the input with uniquely renamed variables. *)
  Printf.printf "Input: %s\n%!" (string_of_cmds ast);
  (* Should the abstract syntax tree be printed? *)
  if !(Flags.verbose) then
    ((* Printf.printf "\nScope:\n%s\n%!" (string_of_scope scope); *)
      Printf.printf "\nAbstract Syntax Tree:\n%s\n\n%!" (tree_string_of_cmds ast ""));
  (* Evaluate the input. *)
  eval_cmds ast stack' heap';
  (* Print the values on the top of the stack. *)
  Printf.printf "Values:\n%s%!" (string_of_vals stack' heap');
  (* Update the global variables with the new versions if there were no
     failures. *)
  scope := scope'; stack := stack'; heap := heap';;


(** Evaluate the given interpreter directive.
    @param dir A string for the directive to run. *)
let do_directive dir =
  match dir with
  (** "clear" -> clearing the scope, stack, and heap *)
  | "clear" -> Hashtbl.clear !scope; Hashtbl.clear !stack; Hashtbl.clear !heap;
    Printf.printf "Stack and heap cleared\n%!"
  (** "help" -> printing the help string for MiniOOL *)
  | "help" -> print_endline (Utils.logo ^ Utils.help)
  (** "exit" | "quit" -> exit MiniOOL *)
  | "exit" | "quit" -> raise Lexer.Eof
  | "verbose" -> Flags.verbose := true
  | "quite" -> Flags.verbose := false
  (** _ -> fail because of an unknown directive *)
  | _ -> failwith (Printf.sprintf "Unknown directive \"%s\"" dir);;
%} (* declarations *)

(* lexer tokens *)
%token EOL PROC SEMICOLON COLON DOT ASSIGN NULL
%token IS_EQUAL IS_NOT_EQUAL IS_LESS IS_LESS_EQUAL IS_GREATER IS_GREATER_EQUAL
%token NOT AND OR
%token PLUS MINUS TIMES DIV MOD LPAREN RPAREN
%token VAR MALLOC SKIP RBRACE LBRACE WHILE DO IF THEN ELSE PARALLEL ATOM
%token BACKSLASH
%token <string> VARIABLE, FIELD
%token <int> NUM
%token <bool> BOOL

(* the entry point *)
%start <unit> prog

(* lowest precedence  *)
%right ASSIGN
%left OR
%left AND
%nonassoc NOT
%left IS_LESS IS_LESS_EQUAL IS_GREATER IS_GREATER_EQUAL IS_NOT_EQUAL IS_EQUAL
%left PLUS MINUS
%left TIMES DIV MOD
%nonassoc UMINUS
%left DOT
(* highest precedence *)

%% (* rules *)

(* The input is either a sequence of commands or a single directive. *)
prog:
    ast = cmds EOL  {run_program ast}
  | dir = directive EOL {do_directive dir}

(* A cmds is a sequence of commands seperated by a SEMICOLON of empty. *)
cmds:
    c1 = cmd SEMICOLON c2 = cmds {c1 :: c2}
  | c = cmd                      {[c]}
  |                              {[]}

(* A cmd is a single command as defined in the
   "Syntax ans Semantics of MiniOOL" *)
cmd:
    VAR x = VARIABLE                                {Declare (ref x)}
  | p = expr LPAREN y = expr RPAREN                 {ProceduceCall (p, y)}
  | MALLOC LPAREN x=VARIABLE RPAREN                 {Malloc (ref x)}
  | x = VARIABLE ASSIGN e = expr                    {Assign ((ref x), e)}
  | e1 = expr DOT e2 = expr ASSIGN e3 = expr        {FieldAssign (e1, e2, e3)}
  | SKIP                                            {Skip}
  | LBRACE cs = cmds RBRACE                         {CmdSequence cs}
  (* While loops can optionally have a "do" after the conditional. *)
  | WHILE b = bool_expr c = cmd                     {While (b, c)}
  | WHILE b = bool_expr DO c = cmd                  {While (b, c)}
  (* IfElse statements can optionally have a "then" after the conditional. *)
  | IF b = bool_expr c1 = cmd ELSE c2 = cmd         {IfElse (b, c1, c2)}
  | IF b = bool_expr THEN c1 = cmd ELSE c2 = cmd    {IfElse (b, c1, c2)}
  | LBRACE c1 = cmd PARALLEL c2 = cmd RBRACE        {Parallel (c1, c2)}
  | ATOM LPAREN cs = cmds RPAREN                    {Atom (CmdSequence cs)}

(* A bool_expr is a boolean expression as defined in the
   "Syntax ans Semantics of MiniOOL" *)
bool_expr:
    b = BOOL                             {Bool b}
  | LPAREN b = bool_expr RPAREN          {b}
  (* Store the operators as a function: ( op ). *)
  | e1 = expr IS_EQUAL e2 = expr         {BinaryComparisonOperator (( = ),  e1, e2)}
  | e1 = expr IS_NOT_EQUAL e2 = expr     {BinaryComparisonOperator (( <> ), e1, e2)}
  | e1 = expr IS_LESS e2 = expr          {BinaryComparisonOperator (( < ),  e1, e2)}
  | e1 = expr IS_LESS_EQUAL e2 = expr    {BinaryComparisonOperator (( <= ), e1, e2)}
  | e1 = expr IS_GREATER e2 = expr       {BinaryComparisonOperator (( > ),  e1, e2)}
  | e1 = expr IS_GREATER_EQUAL e2 = expr {BinaryComparisonOperator (( >= ), e1, e2)}
  | NOT b = bool_expr                    {UnaryLogicOperator (( not ), b)}
  | b1 = bool_expr AND b2 = bool_expr    {BinaryLogicOperator (( && ), b1, b2)}
  | b1 = bool_expr OR  b2 = bool_expr    {BinaryLogicOperator (( || ), b1, b2)}

(* A expr is an expression as defined in the
   "Syntax ans Semantics of MiniOOL" *)
expr:
    f = FIELD                        {Field f}
  | n = NUM                          {Num n}
  (* Store the operators as a function: ( op ). *)
  | e1 = expr PLUS  e2 = expr        {BinaryArithmeticOperator (( + ), e1, e2)}
  | e1 = expr MINUS e2 = expr        {BinaryArithmeticOperator (( - ), e1, e2)}
  | e1 = expr TIMES e2 = expr        {BinaryArithmeticOperator (( * ), e1, e2)}
  | e1 = expr DIV   e2 = expr        {BinaryArithmeticOperator (( / ), e1, e2)}
  | e1 = expr MOD   e2 = expr        {BinaryArithmeticOperator (( mod ), e1, e2)}
  | MINUS e = expr                   {UnaryArithmeticOperator  (( ~- ), e)} %prec UMINUS
  | LPAREN e = expr RPAREN           {e}
  | NULL                             {Null}
  | x = VARIABLE                     {Variable (ref x)}
  | e1 = expr DOT e2 = expr          {FieldAccess (e1, e2)}
  | PROC y = VARIABLE COLON c = cmd  {Procedure ((ref y), c)}

directive:
    BACKSLASH dir = VARIABLE       {dir}
%% (* trailer *)
