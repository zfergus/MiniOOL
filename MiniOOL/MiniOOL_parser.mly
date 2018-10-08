/* File calculatorMENHIR.mly */

%{ (* header *)

type symbTable = (string * int) list ;;

let sb = ref([] : symbTable) ;;

let getvalue x =
   if (List.mem_assoc x !sb) then
     (List.assoc x !sb)
   else
     0;;

let rec except x l = match l with
  []   -> []
| h::t -> if (h = x) then t
            else h::(except x t)

let setvalue x v =
  (print_string (x ^ " = "); print_int (v);
   print_string ";\n"; flush stdout;
   if (List.mem_assoc x !sb) then
     sb := (x, v) :: (except (x, (List.assoc x !sb)) !sb)
   else
     sb := (x, v) :: !sb
  );;

%} /* declarations */

%token EOL SEMICOLON ASSIGN PLUS /* lexer tokens */
%token MINUS TIMES DIV LPAREN RPAREN
%token < string > IDENT
%token < int > NUM
%start prog               /* the entry point */
%type <unit> prog
%type <int> cmds
%type <int> cmd
%type <int> declaration
%type <int> procedure_call
%type <int> malloc
%type <int> assign
%type <int> field_assign
%type <int> sequential_control
%type <int> parallelism
%type <bool> bool_expr
%type <int> expr
%left OR           /* lowest precedence  */
%left AND
%nonassoc NOT
%left IS_LESS IS_LESS_EQUAL IS_GREATER IS_GREATER_EQUAL IS_NOT_EQUAL IS_EQUAL
%left PLUS MINUS
%left TIMES DIV
%nonassoc UMINUS
%left FIELD_REF
%nonassoc PROC_CALL /* highest precedence */

%% /* rules */

prog:
    cmds EOL  { print_int $1 ; print_newline(); flush stdout; () }

cmds:
    cmd SEMICOLON l = cmds   { l }
  | c = cmd                  { c }

cmd:
    c = declaration     { c }
  | c = procedure_call  { c }
  | c = malloc          { c }
  | c = assign          { c }
  | c = field_assign    { c }
  | c = expr            { c }

declaration:
    VAR IDENT { (setvalue x 0) ; 0 }

procedure_call:
    /* IDENT may need to be replaced with expr */
    p = expr RPAREN y = expr LPAREN %prec PROC_CALL {(call_proc p y)}

malloc:
    MALLOC RPAREN x=IDENT LPAREN { (heap_setvalue x 0) }

assign:
    x = IDENT ASSIGN e = expr  { (setvalue x e) ; e }

field_assign:
    x = IDENT DOT f = IDENT ASSIGN e = expr { (setfield x f e); e}

sequential_control:
    SKIP {0}
  | RBRACE c = cmds LBRACE { c }
    /* TODO: Need to finish while and if else */
  | WHILE b = bool_expr c = cmd { c }
  | IF b = bool_expr c1 = cmd ELSE c2 = cmd { c1 }

parallelism:
    RBRACE c1 = cmd PARALLEL c2 = cmd LBRACE { c1 }
  | ATOM RPAREN c = cmd LPAREN {(atom c)}

bool_expr:
    TRUE                                 {true}
  | FALSE                                {false}
  | LPAREN b = bool_expr RPAREN          { b }
  | e1 = expr IS_EQUAL e2 = expr         { e1 = e2  }
  | e1 = expr IS_NOT_EQUAL e2 = expr     { e1 <> e2 }
  | e1 = expr IS_LESS e2 = expr          { e1 < e2  }
  | e1 = expr IS_LESS_EQUAL e2 = expr    { e1 <= e2 }
  | e1 = expr IS_GREATER e2 = expr       { e1 > e2  }
  | e1 = expr IS_GREATER_EQUAL e2 = expr { e1 >= e2 }
  | NOT b = bool_expr                    {not b}
  | b1 = bool_expr AND b2 = bool_expr    {b1 && b2}
  | b1 = bool_expr OR  b2 = bool_expr    {b1 || b2}

expr:
    v = NUM                       { v }
  | e1 = expr PLUS  e2 = expr     { e1 + e2 }
  | e1 = expr MINUS e2 = expr     { e1 - e2 }
  | e1 = expr TIMES e2 = expr     { e1 * e2 }
  | e1 = expr DIV   e2 = expr     { e1 / e2 }
  | MINUS e = expr %prec UMINUS   { -e }
  | LPAREN e = expr RPAREN        { e }
  | NULL                          {0} /* TODO: Declare null type */
  | x = IDENT DOT f = IDENT % pred FIELD_REF { (getfieldvalue x f) }
  | x = IDENT                     { (getvalue x) }
  | PROC y = IDENT COLON c = cmd  {(declare_proc y c); 0}

%% (* trailer *)
