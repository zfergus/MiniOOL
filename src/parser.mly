(** Parser for MiniOOL
    @author Zachary Ferguson *)

%{ (* header *)
open AbstractSyntaxTree;;
open ProgramString;;
open AbstractSyntaxTreeString;;
open StaticSemantics;;
%} (* declarations *)

(* lexer tokens *)
%token EOL PROC SEMICOLON COLON DOT ASSIGN NULL
%token IS_EQUAL IS_NOT_EQUAL IS_LESS IS_LESS_EQUAL IS_GREATER IS_GREATER_EQUAL
%token NOT AND OR
%token PLUS MINUS TIMES DIV MOD LPAREN RPAREN
%token VAR MALLOC SKIP RBRACE LBRACE WHILE IF THEN ELSE PARALLEL ATOM
%token <string> IDENT
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
/* %left DOT */
(* highest precedence *)

%% (* rules *)

prog:
    ast = cmds EOL  {let scope = Hashtbl.create 10 in
                     check_cmds_in_scope scope ast;
                     if !(Flags.verbose) then
                        (print_scope scope; print_endline (cmds_to_string ast);
                        Printf.printf "Abstract Syntax Tree:\n%s\n" (tree_string_of_cmds ast ""))
                     else ()}

cmds:
    c1 = cmd SEMICOLON c2 = cmds {c1 :: c2}
  | c = cmd                      {[c]}
  |                              {[]}

cmd:
    VAR x = IDENT                                   {Declare (ref x)}
  (* | e = expr                                        {failwith ("Syntax error (" ^ (expr_to_string e) ^ " is an expression not a command)")} *)
  (* | b = bool_expr                                   {failwith ("Syntax error (" ^ (bool_expr_to_string b) ^ " is a boolean expression not a command)")} *)
  | p = expr LPAREN y = expr RPAREN                 {ProceduceCall (p, y)}
  | MALLOC LPAREN x=IDENT RPAREN                    {MallocVar (ref x)}
  | MALLOC LPAREN xf=field RPAREN                   {MallocField xf}
  | VAR x = IDENT ASSIGN e = expr                   {CmdSequence [(Declare (ref x)); (Assign ((ref x), e))]}
  | x = IDENT ASSIGN e = expr                       {Assign ((ref x), e)}
  | xf = field ASSIGN e = expr                      {FieldAssign (xf, e)}
  | SKIP                                            {Skip}
  | LBRACE c = cmds RBRACE                          {CmdSequence c}
  | WHILE b = bool_expr c = cmd                     {While (b, c)}
  | IF b = bool_expr c1 = cmd ELSE c2 = cmd         {IfElse (b, c1, c2)}
  | IF b = bool_expr THEN c1 = cmd ELSE c2 = cmd    {IfElse (b, c1, c2)}
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
  | e1 = expr MOD   e2 = expr     {ArithmeticBinaryOperator (( mod ), e1, e2)}
  | MINUS e = expr                {ArithmeticUnaryOperator  (( ~- ), e)} %prec UMINUS
  | LPAREN e = expr RPAREN        {e}
  | NULL                          {Null}
  | xf = field                    {FieldAccess xf}
  | x = IDENT                     {Ident (ref x)}
  | PROC y = IDENT COLON c = cmd  {Procedure ((ref y), c)}

field:
      xf = field DOT f = IDENT {FieldExpr (xf, f)}
    | x = IDENT DOT f = IDENT {Field ((ref x), f)}

%% (* trailer *)
