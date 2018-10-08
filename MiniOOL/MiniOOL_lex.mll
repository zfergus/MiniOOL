(* File miniooLEX.mll *)
{
open MiniOOL_parser;; (* Type token defined in MiniOOL_parser.mli *)
exception Eof;;
}
rule token = parse
    [' ' '\t'] { token lexbuf } (* skip blanks and tabs *)
  | ['\n']     { EOL }
  | (['a'-'z'] | ['A'-'Z'])(['a'-'z'] | ['A'-'Z'] | ['0'-'9'] | '_')* as idt
               { IDENT idt }
  | "proc" { PROC_DEC }
  | ['0'-'9']+ as num
               { NUM (int_of_string num) }
  | "true" | "false" as b
               {BOOL (bool_of_string b)}
  | ';'        { SEMICOLON }
  | ':'        { COLON }
  | '.'        { DOT }
  | '='        { ASSIGN }
  (* boolean operators *)
  | "=="       { IS_EQUAL }
  | "!="       { IS_NOT_EQUAL }
  | '<'        { IS_LESS }
  | '<' '='    { IS_LESS_EQUAL }
  | '>'        { IS_GREATER }
  | '>' '='    { IS_GREATER_EQUAL }
  | "not"      {NOT}
  | "&&"       {AND}
  | "||"       {OR}
  (* algebraic operators *)
  | '+'        { PLUS }
  | '-'        { MINUS }
  | '*'        { TIMES }
  | '/'        { DIV }
  | '('        { LPAREN }
  | ')'        { RPAREN }
  | "var"      { VAR_DEC }
  | "malloc"   { MALLOC }
  | eof        { raise Eof }
  | "skip"     {SKIP}
  | '{'        {RBRACE}
  | '}'        {LBRACE}
  | "while"    {WHILE}
  | "if"       {IF}
  | "else"     {ELSE}
  | "|||"      {PARALLEL}
  | "atom"     {ATOM}
