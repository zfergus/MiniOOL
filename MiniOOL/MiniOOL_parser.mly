/* File calculatorMENHIR.mly */

%{ (* header *)
type ident = string;;
type cmds = cmd list
and expr =
    Num of int | ArithmeticBinaryOperator of (int -> int -> int) * expr * expr |
    ArithmeticUnaryOperator of (int -> int) * expr | Null |
    FieldAccess of expr * ident | Procedure of ident * cmd |
    Ident of ident
and bool_expr =
    Bool of bool |
    ComparisonBinaryOperator of (int -> int -> bool) * expr * expr |
    BoolUnaryOperator of (bool -> bool) * bool_expr |
    BoolBinaryOperator of (bool -> bool -> bool) * bool_expr * bool_expr
and cmd =
    Declare of ident | ProceduceCall of expr * expr | Malloc of string |
    Assign of ident * expr | FieldAssign of ident * ident * expr | Skip |
    Cmds of cmds | While of bool_expr * cmd | IfElse of bool_expr * cmd * cmd |
    Parallel of cmd * cmd | Atom of cmd;;

type prog = cmds;;

let rec arithmetic_op_to_string e = match e with
    | ArithmeticBinaryOperator (op, a, b) -> (match (op 20 10) with
        | 10  -> " - "
        | 30  -> " + "
        | 200 -> " * "
        | 2   -> " / ";
        | _   -> " op ")
    | ArithmeticUnaryOperator (op, e) -> "-"
    | _ -> " op "

and bool_op_to_string e = match e with
    | ComparisonBinaryOperator (op, a, b) -> (match (op 2 1) with
        | true -> (match (op 1 2) with
            | true -> " != "
            | false -> (match (op 1 1) with
                | true -> " >= "
                | false -> " > "))
        | false -> (match (op 1 2) with
            | true -> (match (op 1 1) with
                | true -> " <= "
                | false -> " < ")
            | false -> " == "))
    | BoolBinaryOperator (op, a, b) -> (match (op true false) with
        | false -> " && "
        | true  -> " || ")
    | BoolUnaryOperator (op, e) -> "not"
    | _ -> " op "

and bool_expr_to_string b = match b with
    | Bool b -> if b then "true" else "false"
    | _ -> ("(" ^
        (match b with
            | ComparisonBinaryOperator (op, e1, e2) ->
                ((expr_to_string e1) ^ (bool_op_to_string b) ^ (expr_to_string e2))
            | BoolUnaryOperator (op, b1) ->
                ((bool_op_to_string b) ^ (bool_expr_to_string b1))
            | BoolBinaryOperator (op, b1, b2) ->
                ((bool_expr_to_string b1) ^ (bool_op_to_string b) ^ (bool_expr_to_string b2))
            | _ -> "")
        ^ ")")

and expr_to_string e = match e with
    | Num n -> string_of_int n
    | Null -> "null"
    | Ident x -> x
    | _ -> ("(" ^
        (match e with
            | ArithmeticBinaryOperator (op, e1, e2) ->
                ((expr_to_string e1) ^ (arithmetic_op_to_string e) ^ (expr_to_string e2))
            | ArithmeticUnaryOperator (op, e1) ->
                ((arithmetic_op_to_string e) ^ (expr_to_string e1))
            | FieldAccess (x, f) -> ((expr_to_string x) ^ "." ^ f)
            | Procedure (y, c) ->
                ("proc " ^ y ^ ": " ^ (cmd_to_string c))
            | _ -> "")
        ^ ")")

and cmd_to_string c =
    "{" ^
    (match c with
        | Declare x -> ("var " ^ x)
        | ProceduceCall (p, y) ->
            ((expr_to_string p) ^ "(" ^ (expr_to_string y) ^ ")")
        | Malloc x -> ("malloc(" ^ x ^ ")")
        | Assign (x, e) -> (x ^ " = " ^ (expr_to_string e))
        | FieldAssign (x, f, e) ->
            (x ^ "." ^ f ^ " = " ^ (expr_to_string e))
        | Skip -> "skip"
        | Cmds cs -> (cmds_to_string cs)
        | While (b, c) ->
            ("while " ^ (bool_expr_to_string b) ^ " " ^ (cmd_to_string c))
        | IfElse (b, c1, c2) ->
            ("if " ^ (bool_expr_to_string b) ^
             " " ^ (cmd_to_string c1) ^
             " else " ^ (cmd_to_string c2))
        | Parallel (c1, c2) ->
            ((cmd_to_string c1) ^ " ||| " ^ (cmd_to_string c2))
        | Atom c -> ("atom(" ^ (cmd_to_string c) ^ ")"))
    ^ "}"

and cmds_to_string cmds = match cmds with
    | [] -> "";
    | h :: t -> ((cmd_to_string h) ^ ";" ^ (cmds_to_string t))

and print_ast ast = print_endline (cmds_to_string ast);;

%} /* declarations */

/* lexer tokens */
%token EOL PROC SEMICOLON COLON DOT ASSIGN NULL
%token IS_EQUAL IS_NOT_EQUAL IS_LESS IS_LESS_EQUAL IS_GREATER IS_GREATER_EQUAL
%token NOT AND OR
%token PLUS MINUS TIMES DIV LPAREN RPAREN
%token VAR MALLOC SKIP RBRACE LBRACE WHILE IF ELSE PARALLEL ATOM
%token <string> IDENT
%token <int> NUM
%token <bool> BOOL
/* the entry point */
%start prog
%type <unit> prog
/* lowest precedence  */
/* %left PARALLEL */
%left OR
%left AND
%nonassoc NOT
%left IS_LESS IS_LESS_EQUAL IS_GREATER IS_GREATER_EQUAL IS_NOT_EQUAL IS_EQUAL
%left PLUS MINUS
%left TIMES DIV
%nonassoc UMINUS
/* %nonassoc PROC_CALL */
/* %left FIELD_REF */
/* %nonassoc ATOM */
/* highest precedence */

%% /* rules */

prog:
    ast = cmds EOL  {print_string "AST built:\n"; print_ast ast; flush stdout; ()}

cmds:
    c1 = cmd SEMICOLON c2 = cmds {c1 :: c2}
  | c = cmd SEMICOLON            {[c]}
  | c = cmd                      {[c]}
  |                              {[]}

cmd:
    VAR x = IDENT                                   {Declare x}
  /* | e = expr {$syntaxerror "slkdfnsklfdn"} */
  /* | e = expr                                        {failwith ("Syntax error (" ^ (expr_to_string e) ^ " is an expression not a command)")} */
  /* | b = bool_expr                                   {failwith ("Syntax error (" ^ (bool_expr_to_string b) ^ " is a boolean expression not a command)")} */
  /* | p = expr LPAREN y = expr RPAREN %prec PROC_CALL {ProceduceCall (p, y)} */
  | p = expr LPAREN y = expr RPAREN                 {ProceduceCall (p, y)}
  | MALLOC LPAREN x=IDENT RPAREN                    {Malloc x}
  | x = IDENT ASSIGN e = expr                       {Assign (x, e)}
  | x = IDENT DOT f = IDENT ASSIGN e = expr         {FieldAssign (x, f, e)}
  | SKIP                                            {Skip}
  | LBRACE c = cmds RBRACE                          {Cmds c}
  | WHILE b = bool_expr c = cmd                     {While (b, c)}
  | IF b = bool_expr c1 = cmd ELSE c2 = cmd         {IfElse (b, c1, c2)}
  | LBRACE c1 = cmd PARALLEL c2 = cmd RBRACE        {Parallel (c1, c2)}
  | ATOM LPAREN c = cmd RPAREN                      {Atom c}

bool_expr:
    b = BOOL                             {Bool b}
  | LPAREN b = bool_expr RPAREN          {b}
  | e1 = expr IS_EQUAL e2 = expr         {ComparisonBinaryOperator (( = ),  e1, e2)}
  | e1 = expr IS_NOT_EQUAL e2 = expr     {ComparisonBinaryOperator (( <> ), e1, e2)}
  | e1 = expr IS_LESS e2 = expr          {ComparisonBinaryOperator (( < ),  e1, e2)}
  | e1 = expr IS_LESS_EQUAL e2 = expr    {ComparisonBinaryOperator (( <= ), e1, e2)}
  | e1 = expr IS_GREATER e2 = expr       {ComparisonBinaryOperator (( > ),  e1, e2)}
  | e1 = expr IS_GREATER_EQUAL e2 = expr {ComparisonBinaryOperator (( >= ), e1, e2)}
  | NOT b = bool_expr                    {BoolUnaryOperator (not, b)}
  | b1 = bool_expr AND b2 = bool_expr    {BoolBinaryOperator ((&&), b1, b2)}
  | b1 = bool_expr OR  b2 = bool_expr    {BoolBinaryOperator ((||), b1, b2)}

expr:
    n = NUM                       {Num n}
  | e1 = expr PLUS  e2 = expr     {ArithmeticBinaryOperator (( + ), e1, e2)}
  | e1 = expr MINUS e2 = expr     {ArithmeticBinaryOperator (( - ), e1, e2)}
  | e1 = expr TIMES e2 = expr     {ArithmeticBinaryOperator (( * ), e1, e2)}
  | e1 = expr DIV   e2 = expr     {ArithmeticBinaryOperator (( / ), e1, e2)}
  | MINUS e = expr %prec UMINUS   {ArithmeticUnaryOperator  (( ~- ), e)}
  | LPAREN e = expr RPAREN        {e}
  | NULL                          {Null}
  | x = expr DOT f = IDENT        {FieldAccess (x, f)}
  /* | x = expr DOT f = IDENT %prec FIELD_REF {FieldAccess (x, f)} */
  /* | x = expr DOT f = expr  %prec FIELD_REF {failwith ("Syntax error (" ^ (expr_to_string f) ^ " is not an field)")} */
  | x = IDENT                     {Ident x}
  | PROC y = IDENT COLON c = cmd  {Procedure (y, c)}
  /* | p = expr LPAREN y = expr RPAREN {MiniOOL_parser.Error "Syntax error (proceduce calls are commands not expressions)"} */

%% (* trailer *)
