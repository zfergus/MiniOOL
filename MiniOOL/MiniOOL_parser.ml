
module MenhirBasics = struct
  
  exception Error
  
  type token = 
    | WHILE
    | VAR
    | TIMES
    | SKIP
    | SEMICOLON
    | RPAREN
    | RBRACE
    | PROC
    | PLUS
    | PARALLEL
    | OR
    | NUM of (
# 119 "MiniOOL_parser.mly"
       (int)
# 22 "MiniOOL_parser.ml"
  )
    | NULL
    | NOT
    | MINUS
    | MALLOC
    | LPAREN
    | LBRACE
    | IS_NOT_EQUAL
    | IS_LESS_EQUAL
    | IS_LESS
    | IS_GREATER_EQUAL
    | IS_GREATER
    | IS_EQUAL
    | IF
    | IDENT of (
# 118 "MiniOOL_parser.mly"
       (string)
# 40 "MiniOOL_parser.ml"
  )
    | EOL
    | ELSE
    | DOT
    | DIV
    | COLON
    | BOOL of (
# 120 "MiniOOL_parser.mly"
       (bool)
# 50 "MiniOOL_parser.ml"
  )
    | ATOM
    | ASSIGN
    | AND
  
end

include MenhirBasics

let _eRR =
  MenhirBasics.Error

type _menhir_env = {
  _menhir_lexer: Lexing.lexbuf -> token;
  _menhir_lexbuf: Lexing.lexbuf;
  _menhir_token: token;
  mutable _menhir_error: bool
}

and _menhir_state = 
  | MenhirState85
  | MenhirState81
  | MenhirState78
  | MenhirState73
  | MenhirState67
  | MenhirState65
  | MenhirState62
  | MenhirState60
  | MenhirState56
  | MenhirState53
  | MenhirState52
  | MenhirState50
  | MenhirState48
  | MenhirState46
  | MenhirState44
  | MenhirState42
  | MenhirState40
  | MenhirState38
  | MenhirState36
  | MenhirState33
  | MenhirState32
  | MenhirState31
  | MenhirState30
  | MenhirState23
  | MenhirState21
  | MenhirState19
  | MenhirState14
  | MenhirState11
  | MenhirState10
  | MenhirState4
  | MenhirState1
  | MenhirState0

# 3 "MiniOOL_parser.mly"
   (* header *)
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


# 213 "MiniOOL_parser.ml"

let rec _menhir_run50 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_bool_expr -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL _v ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _v
    | IDENT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _v
    | LPAREN ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | MINUS ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | NOT ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | NULL ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | NUM _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _v
    | PROC ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState50

and _menhir_run53 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_bool_expr -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL _v ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _v
    | IDENT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _v
    | LPAREN ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | MINUS ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | NOT ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | NULL ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | NUM _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _v
    | PROC ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState53

and _menhir_run36 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _v
    | LPAREN ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | MINUS ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | NULL ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | NUM _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _v
    | PROC ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState36

and _menhir_run38 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
    | LPAREN ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | MINUS ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | NULL ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | NUM _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
    | PROC ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState38

and _menhir_run40 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
    | LPAREN ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | MINUS ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | NULL ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | NUM _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
    | PROC ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState40

and _menhir_run42 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _v
    | LPAREN ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | MINUS ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | NULL ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | NUM _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _v
    | PROC ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState42

and _menhir_run44 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v
    | LPAREN ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | MINUS ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | NULL ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | NUM _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v
    | PROC ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState44

and _menhir_run46 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _v
    | LPAREN ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | MINUS ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | NULL ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | NUM _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _v
    | PROC ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState46

and _menhir_run14 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState14 _v
    | LPAREN ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState14
    | MINUS ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState14
    | NULL ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState14
    | NUM _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState14 _v
    | PROC ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState14
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState14

and _menhir_run18 : _menhir_env -> ('ttv_tail * _menhir_state) * _menhir_state * 'tv_expr -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ('freshtv325 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
    ((let ((_menhir_stack, _menhir_s), _, (e : 'tv_expr)) = _menhir_stack in
    let _3 = () in
    let _1 = () in
    let _v : 'tv_expr = 
# 186 "MiniOOL_parser.mly"
                                  (e)
# 434 "MiniOOL_parser.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv326)

and _menhir_run19 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState19 _v
    | LPAREN ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | MINUS ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | NULL ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | NUM _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState19 _v
    | PROC ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState19

and _menhir_run23 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState23 _v
    | LPAREN ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState23
    | MINUS ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState23
    | NULL ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState23
    | NUM _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState23 _v
    | PROC ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState23
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState23

and _menhir_run16 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENT _v ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv321 * _menhir_state * 'tv_expr)) = Obj.magic _menhir_stack in
        let (_v : (
# 118 "MiniOOL_parser.mly"
       (string)
# 493 "MiniOOL_parser.ml"
        )) = _v in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv319 * _menhir_state * 'tv_expr)) = Obj.magic _menhir_stack in
        let ((f : (
# 118 "MiniOOL_parser.mly"
       (string)
# 501 "MiniOOL_parser.ml"
        )) : (
# 118 "MiniOOL_parser.mly"
       (string)
# 505 "MiniOOL_parser.ml"
        )) = _v in
        ((let (_menhir_stack, _menhir_s, (x : 'tv_expr)) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_expr = 
# 188 "MiniOOL_parser.mly"
                                  (FieldAccess (x, f))
# 512 "MiniOOL_parser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv320)) : 'freshtv322)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv323 * _menhir_state * 'tv_expr)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv324)

and _menhir_run21 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState21 _v
    | LPAREN ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState21
    | MINUS ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState21
    | NULL ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState21
    | NUM _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState21 _v
    | PROC ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState21
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState21

and _menhir_fail : unit -> 'a =
  fun () ->
    Printf.fprintf Pervasives.stderr "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_reduce25 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_cmd -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, (c : 'tv_cmd)) = _menhir_stack in
    let _v : 'tv_cmds = 
# 146 "MiniOOL_parser.mly"
                                 ([c])
# 556 "MiniOOL_parser.ml"
     in
    _menhir_goto_cmds _menhir_env _menhir_stack _menhir_s _v

and _menhir_run78 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_cmd -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ATOM ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState78
    | IDENT _v ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _v
    | IF ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState78
    | LBRACE ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState78
    | LPAREN ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState78
    | MALLOC ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState78
    | MINUS ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState78
    | NULL ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState78
    | NUM _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _v
    | PROC ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState78
    | SKIP ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState78
    | VAR ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState78
    | WHILE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState78
    | EOL | RBRACE ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv317 * _menhir_state * 'tv_cmd)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, (c : 'tv_cmd)) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_cmds = 
# 145 "MiniOOL_parser.mly"
                                 ([c])
# 599 "MiniOOL_parser.ml"
         in
        _menhir_goto_cmds _menhir_env _menhir_stack _menhir_s _v) : 'freshtv318)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState78

and _menhir_goto_bool_expr : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_bool_expr -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState33 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv299 * _menhir_state) * _menhir_state * 'tv_bool_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | AND ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState48
        | OR ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState48
        | RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv297 * _menhir_state) * _menhir_state * 'tv_bool_expr) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState48 in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv295 * _menhir_state) * _menhir_state * 'tv_bool_expr) = Obj.magic _menhir_stack in
            let (_ : _menhir_state) = _menhir_s in
            ((let ((_menhir_stack, _menhir_s), _, (b : 'tv_bool_expr)) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : 'tv_bool_expr = 
# 168 "MiniOOL_parser.mly"
                                         (b)
# 635 "MiniOOL_parser.ml"
             in
            _menhir_goto_bool_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv296)) : 'freshtv298)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState48) : 'freshtv300)
    | MenhirState50 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv303 * _menhir_state * 'tv_bool_expr) * _menhir_state) * _menhir_state * 'tv_bool_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | AND ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState52
        | ATOM | IDENT _ | IF | LBRACE | LPAREN | MALLOC | MINUS | NULL | NUM _ | OR | PROC | RPAREN | SKIP | VAR | WHILE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv301 * _menhir_state * 'tv_bool_expr) * _menhir_state) * _menhir_state * 'tv_bool_expr) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s, (b1 : 'tv_bool_expr)), _), _, (b2 : 'tv_bool_expr)) = _menhir_stack in
            let _2 = () in
            let _v : 'tv_bool_expr = 
# 177 "MiniOOL_parser.mly"
                                         (BoolBinaryOperator ((||), b1, b2))
# 658 "MiniOOL_parser.ml"
             in
            _menhir_goto_bool_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv302)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState52) : 'freshtv304)
    | MenhirState53 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv307 * _menhir_state * 'tv_bool_expr) * _menhir_state) * _menhir_state * 'tv_bool_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv305 * _menhir_state * 'tv_bool_expr) * _menhir_state) * _menhir_state * 'tv_bool_expr) = Obj.magic _menhir_stack in
        ((let (((_menhir_stack, _menhir_s, (b1 : 'tv_bool_expr)), _), _, (b2 : 'tv_bool_expr)) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_bool_expr = 
# 176 "MiniOOL_parser.mly"
                                         (BoolBinaryOperator ((&&), b1, b2))
# 675 "MiniOOL_parser.ml"
         in
        _menhir_goto_bool_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv306)) : 'freshtv308)
    | MenhirState32 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv311 * _menhir_state) * _menhir_state * 'tv_bool_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv309 * _menhir_state) * _menhir_state * 'tv_bool_expr) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _, (b : 'tv_bool_expr)) = _menhir_stack in
        let _1 = () in
        let _v : 'tv_bool_expr = 
# 175 "MiniOOL_parser.mly"
                                         (BoolUnaryOperator (not, b))
# 688 "MiniOOL_parser.ml"
         in
        _menhir_goto_bool_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv310)) : 'freshtv312)
    | MenhirState31 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv313 * _menhir_state) * _menhir_state * 'tv_bool_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | AND ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState56
        | ATOM ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState56
        | IDENT _v ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _v
        | IF ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState56
        | LBRACE ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState56
        | LPAREN ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState56
        | MALLOC ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState56
        | MINUS ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState56
        | NULL ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState56
        | NUM _v ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _v
        | OR ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState56
        | PROC ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState56
        | SKIP ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState56
        | VAR ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState56
        | WHILE ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState56
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState56) : 'freshtv314)
    | MenhirState1 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv315 * _menhir_state) * _menhir_state * 'tv_bool_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | AND ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | ATOM ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | IDENT _v ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _v
        | IF ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | LBRACE ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | LPAREN ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | MALLOC ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | MINUS ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | NULL ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | NUM _v ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _v
        | OR ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | PROC ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | SKIP ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | VAR ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | WHILE ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState85) : 'freshtv316)
    | _ ->
        _menhir_fail ()

and _menhir_goto_cmds : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_cmds -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState30 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv275 * _menhir_state) * _menhir_state * 'tv_cmds) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RBRACE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv271 * _menhir_state) * _menhir_state * 'tv_cmds) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv269 * _menhir_state) * _menhir_state * 'tv_cmds) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _, (c : 'tv_cmds)) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : 'tv_cmd = 
# 160 "MiniOOL_parser.mly"
                                                    (Cmds c)
# 796 "MiniOOL_parser.ml"
             in
            _menhir_goto_cmd _menhir_env _menhir_stack _menhir_s _v) : 'freshtv270)) : 'freshtv272)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv273 * _menhir_state) * _menhir_state * 'tv_cmds) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv274)) : 'freshtv276)
    | MenhirState78 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv279 * _menhir_state * 'tv_cmd)) * _menhir_state * 'tv_cmds) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv277 * _menhir_state * 'tv_cmd)) * _menhir_state * 'tv_cmds) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (c1 : 'tv_cmd)), _, (c2 : 'tv_cmds)) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_cmds = 
# 144 "MiniOOL_parser.mly"
                                 (c1 :: c2)
# 816 "MiniOOL_parser.ml"
         in
        _menhir_goto_cmds _menhir_env _menhir_stack _menhir_s _v) : 'freshtv278)) : 'freshtv280)
    | MenhirState0 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv293 * _menhir_state * 'tv_cmds) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | EOL ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv289 * _menhir_state * 'tv_cmds) = Obj.magic _menhir_stack in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv287 * _menhir_state * 'tv_cmds) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, (ast : 'tv_cmds)) = _menhir_stack in
            let _2 = () in
            let _v : (
# 123 "MiniOOL_parser.mly"
      (unit)
# 835 "MiniOOL_parser.ml"
            ) = 
# 141 "MiniOOL_parser.mly"
                    (print_string "AST built:\n"; print_ast ast; flush stdout; ())
# 839 "MiniOOL_parser.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv285) = _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : (
# 123 "MiniOOL_parser.mly"
      (unit)
# 847 "MiniOOL_parser.ml"
            )) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv283) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : (
# 123 "MiniOOL_parser.mly"
      (unit)
# 855 "MiniOOL_parser.ml"
            )) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv281) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let ((_1 : (
# 123 "MiniOOL_parser.mly"
      (unit)
# 863 "MiniOOL_parser.ml"
            )) : (
# 123 "MiniOOL_parser.mly"
      (unit)
# 867 "MiniOOL_parser.ml"
            )) = _v in
            (Obj.magic _1 : 'freshtv282)) : 'freshtv284)) : 'freshtv286)) : 'freshtv288)) : 'freshtv290)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv291 * _menhir_state * 'tv_cmds) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv292)) : 'freshtv294)
    | _ ->
        _menhir_fail ()

and _menhir_goto_expr : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_expr -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState11 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv167 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIV ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack)
        | RPAREN ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv165 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv166)) : 'freshtv168)
    | MenhirState14 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv173 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DOT ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack)
        | AND | ATOM | DIV | ELSE | EOL | IDENT _ | IF | IS_EQUAL | IS_GREATER | IS_GREATER_EQUAL | IS_LESS | IS_LESS_EQUAL | IS_NOT_EQUAL | LBRACE | LPAREN | MALLOC | MINUS | NULL | NUM _ | OR | PARALLEL | PLUS | PROC | RBRACE | RPAREN | SEMICOLON | SKIP | TIMES | VAR | WHILE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv169 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (e1 : 'tv_expr)), _, (e2 : 'tv_expr)) = _menhir_stack in
            let _2 = () in
            let _v : 'tv_expr = 
# 183 "MiniOOL_parser.mly"
                                  (ArithmeticBinaryOperator (( * ), e1, e2))
# 925 "MiniOOL_parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv170)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv171 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv172)) : 'freshtv174)
    | MenhirState19 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv179 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIV ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | AND | ATOM | ELSE | EOL | IDENT _ | IF | IS_EQUAL | IS_GREATER | IS_GREATER_EQUAL | IS_LESS | IS_LESS_EQUAL | IS_NOT_EQUAL | LBRACE | LPAREN | MALLOC | MINUS | NULL | NUM _ | OR | PARALLEL | PLUS | PROC | RBRACE | RPAREN | SEMICOLON | SKIP | VAR | WHILE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv175 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (e1 : 'tv_expr)), _, (e2 : 'tv_expr)) = _menhir_stack in
            let _2 = () in
            let _v : 'tv_expr = 
# 181 "MiniOOL_parser.mly"
                                  (ArithmeticBinaryOperator (( + ), e1, e2))
# 955 "MiniOOL_parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv176)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv177 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv178)) : 'freshtv180)
    | MenhirState21 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv185 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DOT ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack)
        | AND | ATOM | DIV | ELSE | EOL | IDENT _ | IF | IS_EQUAL | IS_GREATER | IS_GREATER_EQUAL | IS_LESS | IS_LESS_EQUAL | IS_NOT_EQUAL | LBRACE | LPAREN | MALLOC | MINUS | NULL | NUM _ | OR | PARALLEL | PLUS | PROC | RBRACE | RPAREN | SEMICOLON | SKIP | TIMES | VAR | WHILE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv181 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (e1 : 'tv_expr)), _, (e2 : 'tv_expr)) = _menhir_stack in
            let _2 = () in
            let _v : 'tv_expr = 
# 184 "MiniOOL_parser.mly"
                                  (ArithmeticBinaryOperator (( / ), e1, e2))
# 981 "MiniOOL_parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv182)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv183 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv184)) : 'freshtv186)
    | MenhirState23 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv191 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIV ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | AND | ATOM | ELSE | EOL | IDENT _ | IF | IS_EQUAL | IS_GREATER | IS_GREATER_EQUAL | IS_LESS | IS_LESS_EQUAL | IS_NOT_EQUAL | LBRACE | LPAREN | MALLOC | MINUS | NULL | NUM _ | OR | PARALLEL | PLUS | PROC | RBRACE | RPAREN | SEMICOLON | SKIP | VAR | WHILE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv187 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (e1 : 'tv_expr)), _, (e2 : 'tv_expr)) = _menhir_stack in
            let _2 = () in
            let _v : 'tv_expr = 
# 182 "MiniOOL_parser.mly"
                                  (ArithmeticBinaryOperator (( - ), e1, e2))
# 1011 "MiniOOL_parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv188)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv189 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv190)) : 'freshtv192)
    | MenhirState10 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv197 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DOT ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack)
        | AND | ATOM | DIV | ELSE | EOL | IDENT _ | IF | IS_EQUAL | IS_GREATER | IS_GREATER_EQUAL | IS_LESS | IS_LESS_EQUAL | IS_NOT_EQUAL | LBRACE | LPAREN | MALLOC | MINUS | NULL | NUM _ | OR | PARALLEL | PLUS | PROC | RBRACE | RPAREN | SEMICOLON | SKIP | TIMES | VAR | WHILE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv193 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _, (e : 'tv_expr)) = _menhir_stack in
            let _1 = () in
            let _v : 'tv_expr = 
# 185 "MiniOOL_parser.mly"
                                  (ArithmeticUnaryOperator  (( ~- ), e))
# 1037 "MiniOOL_parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv194)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv195 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv196)) : 'freshtv198)
    | MenhirState33 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv201 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIV ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack)
        | IS_EQUAL ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | IS_GREATER ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | IS_GREATER_EQUAL ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | IS_LESS ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | IS_LESS_EQUAL ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | IS_NOT_EQUAL ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack)
        | RPAREN ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv199 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv200)) : 'freshtv202)
    | MenhirState36 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv207 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIV ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | AND | ATOM | IDENT _ | IF | LBRACE | LPAREN | MALLOC | NULL | NUM _ | OR | PROC | RPAREN | SKIP | VAR | WHILE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv203 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (e1 : 'tv_expr)), _, (e2 : 'tv_expr)) = _menhir_stack in
            let _2 = () in
            let _v : 'tv_bool_expr = 
# 170 "MiniOOL_parser.mly"
                                         (ComparisonBinaryOperator (( <> ), e1, e2))
# 1108 "MiniOOL_parser.ml"
             in
            _menhir_goto_bool_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv204)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv205 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv206)) : 'freshtv208)
    | MenhirState38 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv213 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIV ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | AND | ATOM | IDENT _ | IF | LBRACE | LPAREN | MALLOC | NULL | NUM _ | OR | PROC | RPAREN | SKIP | VAR | WHILE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv209 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (e1 : 'tv_expr)), _, (e2 : 'tv_expr)) = _menhir_stack in
            let _2 = () in
            let _v : 'tv_bool_expr = 
# 172 "MiniOOL_parser.mly"
                                         (ComparisonBinaryOperator (( <= ), e1, e2))
# 1142 "MiniOOL_parser.ml"
             in
            _menhir_goto_bool_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv210)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv211 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv212)) : 'freshtv214)
    | MenhirState40 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv219 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIV ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | AND | ATOM | IDENT _ | IF | LBRACE | LPAREN | MALLOC | NULL | NUM _ | OR | PROC | RPAREN | SKIP | VAR | WHILE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv215 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (e1 : 'tv_expr)), _, (e2 : 'tv_expr)) = _menhir_stack in
            let _2 = () in
            let _v : 'tv_bool_expr = 
# 171 "MiniOOL_parser.mly"
                                         (ComparisonBinaryOperator (( < ),  e1, e2))
# 1176 "MiniOOL_parser.ml"
             in
            _menhir_goto_bool_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv216)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv217 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv218)) : 'freshtv220)
    | MenhirState42 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv225 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIV ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | AND | ATOM | IDENT _ | IF | LBRACE | LPAREN | MALLOC | NULL | NUM _ | OR | PROC | RPAREN | SKIP | VAR | WHILE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv221 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (e1 : 'tv_expr)), _, (e2 : 'tv_expr)) = _menhir_stack in
            let _2 = () in
            let _v : 'tv_bool_expr = 
# 174 "MiniOOL_parser.mly"
                                         (ComparisonBinaryOperator (( >= ), e1, e2))
# 1210 "MiniOOL_parser.ml"
             in
            _menhir_goto_bool_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv222)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv223 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv224)) : 'freshtv226)
    | MenhirState44 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv231 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIV ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | AND | ATOM | IDENT _ | IF | LBRACE | LPAREN | MALLOC | NULL | NUM _ | OR | PROC | RPAREN | SKIP | VAR | WHILE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv227 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (e1 : 'tv_expr)), _, (e2 : 'tv_expr)) = _menhir_stack in
            let _2 = () in
            let _v : 'tv_bool_expr = 
# 173 "MiniOOL_parser.mly"
                                         (ComparisonBinaryOperator (( > ),  e1, e2))
# 1244 "MiniOOL_parser.ml"
             in
            _menhir_goto_bool_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv228)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv229 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv230)) : 'freshtv232)
    | MenhirState46 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv237 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIV ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | AND | ATOM | IDENT _ | IF | LBRACE | LPAREN | MALLOC | NULL | NUM _ | OR | PROC | RPAREN | SKIP | VAR | WHILE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv233 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (e1 : 'tv_expr)), _, (e2 : 'tv_expr)) = _menhir_stack in
            let _2 = () in
            let _v : 'tv_bool_expr = 
# 169 "MiniOOL_parser.mly"
                                         (ComparisonBinaryOperator (( = ),  e1, e2))
# 1278 "MiniOOL_parser.ml"
             in
            _menhir_goto_bool_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv234)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv235 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv236)) : 'freshtv238)
    | MenhirState1 | MenhirState31 | MenhirState32 | MenhirState53 | MenhirState50 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv241 * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIV ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack)
        | IS_EQUAL ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | IS_GREATER ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | IS_GREATER_EQUAL ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | IS_LESS ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | IS_LESS_EQUAL ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | IS_NOT_EQUAL ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv239 * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv240)) : 'freshtv242)
    | MenhirState60 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv247 * _menhir_state * (
# 118 "MiniOOL_parser.mly"
       (string)
# 1328 "MiniOOL_parser.ml"
        ))) * (
# 118 "MiniOOL_parser.mly"
       (string)
# 1332 "MiniOOL_parser.ml"
        ))) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIV ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | AND | ATOM | ELSE | EOL | IDENT _ | IF | IS_EQUAL | IS_GREATER | IS_GREATER_EQUAL | IS_LESS | IS_LESS_EQUAL | IS_NOT_EQUAL | LBRACE | LPAREN | MALLOC | NULL | NUM _ | OR | PARALLEL | PROC | RBRACE | RPAREN | SEMICOLON | SKIP | VAR | WHILE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv243 * _menhir_state * (
# 118 "MiniOOL_parser.mly"
       (string)
# 1352 "MiniOOL_parser.ml"
            ))) * (
# 118 "MiniOOL_parser.mly"
       (string)
# 1356 "MiniOOL_parser.ml"
            ))) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s, (x : (
# 118 "MiniOOL_parser.mly"
       (string)
# 1361 "MiniOOL_parser.ml"
            ))), (f : (
# 118 "MiniOOL_parser.mly"
       (string)
# 1365 "MiniOOL_parser.ml"
            ))), _, (e : 'tv_expr)) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _v : 'tv_cmd = 
# 158 "MiniOOL_parser.mly"
                                                    (FieldAssign (x, f, e))
# 1372 "MiniOOL_parser.ml"
             in
            _menhir_goto_cmd _menhir_env _menhir_stack _menhir_s _v) : 'freshtv244)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv245 * _menhir_state * (
# 118 "MiniOOL_parser.mly"
       (string)
# 1382 "MiniOOL_parser.ml"
            ))) * (
# 118 "MiniOOL_parser.mly"
       (string)
# 1386 "MiniOOL_parser.ml"
            ))) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv246)) : 'freshtv248)
    | MenhirState62 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv253 * _menhir_state * (
# 118 "MiniOOL_parser.mly"
       (string)
# 1395 "MiniOOL_parser.ml"
        ))) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIV ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | AND | ATOM | ELSE | EOL | IDENT _ | IF | IS_EQUAL | IS_GREATER | IS_GREATER_EQUAL | IS_LESS | IS_LESS_EQUAL | IS_NOT_EQUAL | LBRACE | LPAREN | MALLOC | NULL | NUM _ | OR | PARALLEL | PROC | RBRACE | RPAREN | SEMICOLON | SKIP | VAR | WHILE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv249 * _menhir_state * (
# 118 "MiniOOL_parser.mly"
       (string)
# 1415 "MiniOOL_parser.ml"
            ))) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (x : (
# 118 "MiniOOL_parser.mly"
       (string)
# 1420 "MiniOOL_parser.ml"
            ))), _, (e : 'tv_expr)) = _menhir_stack in
            let _2 = () in
            let _v : 'tv_cmd = 
# 157 "MiniOOL_parser.mly"
                                                    (Assign (x, e))
# 1426 "MiniOOL_parser.ml"
             in
            _menhir_goto_cmd _menhir_env _menhir_stack _menhir_s _v) : 'freshtv250)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv251 * _menhir_state * (
# 118 "MiniOOL_parser.mly"
       (string)
# 1436 "MiniOOL_parser.ml"
            ))) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv252)) : 'freshtv254)
    | MenhirState0 | MenhirState85 | MenhirState4 | MenhirState81 | MenhirState78 | MenhirState30 | MenhirState73 | MenhirState56 | MenhirState65 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv259 * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIV ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack)
        | LPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv255 * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | IDENT _v ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
            | LPAREN ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState67
            | MINUS ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState67
            | NULL ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState67
            | NUM _v ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
            | PROC ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState67
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState67) : 'freshtv256)
        | MINUS ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv257 * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv258)) : 'freshtv260)
    | MenhirState67 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv267 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIV ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack)
        | RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv263 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv261 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (p : 'tv_expr)), _, (y : 'tv_expr)) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _v : 'tv_cmd = 
# 155 "MiniOOL_parser.mly"
                                                    (ProceduceCall (p, y))
# 1511 "MiniOOL_parser.ml"
             in
            _menhir_goto_cmd _menhir_env _menhir_stack _menhir_s _v) : 'freshtv262)) : 'freshtv264)
        | TIMES ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv265 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv266)) : 'freshtv268)
    | _ ->
        _menhir_fail ()

and _menhir_goto_cmd : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_cmd -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState65 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv127 * _menhir_state)) * _menhir_state * 'tv_cmd) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv123 * _menhir_state)) * _menhir_state * 'tv_cmd) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv121 * _menhir_state)) * _menhir_state * 'tv_cmd) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _, (c : 'tv_cmd)) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : 'tv_cmd = 
# 164 "MiniOOL_parser.mly"
                                                    (Atom c)
# 1549 "MiniOOL_parser.ml"
             in
            _menhir_goto_cmd _menhir_env _menhir_stack _menhir_s _v) : 'freshtv122)) : 'freshtv124)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv125 * _menhir_state)) * _menhir_state * 'tv_cmd) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv126)) : 'freshtv128)
    | MenhirState56 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv133 * _menhir_state) * _menhir_state * 'tv_bool_expr) * _menhir_state * 'tv_cmd) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ELSE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv129 * _menhir_state) * _menhir_state * 'tv_bool_expr) * _menhir_state * 'tv_cmd) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ATOM ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState73
            | IDENT _v ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v
            | IF ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState73
            | LBRACE ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState73
            | LPAREN ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState73
            | MALLOC ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState73
            | MINUS ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState73
            | NULL ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState73
            | NUM _v ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v
            | PROC ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState73
            | SKIP ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState73
            | VAR ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState73
            | WHILE ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState73
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState73) : 'freshtv130)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv131 * _menhir_state) * _menhir_state * 'tv_bool_expr) * _menhir_state * 'tv_cmd) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv132)) : 'freshtv134)
    | MenhirState73 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv137 * _menhir_state) * _menhir_state * 'tv_bool_expr) * _menhir_state * 'tv_cmd)) * _menhir_state * 'tv_cmd) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv135 * _menhir_state) * _menhir_state * 'tv_bool_expr) * _menhir_state * 'tv_cmd)) * _menhir_state * 'tv_cmd) = Obj.magic _menhir_stack in
        ((let ((((_menhir_stack, _menhir_s), _, (b : 'tv_bool_expr)), _, (c1 : 'tv_cmd)), _, (c2 : 'tv_cmd)) = _menhir_stack in
        let _4 = () in
        let _1 = () in
        let _v : 'tv_cmd = 
# 162 "MiniOOL_parser.mly"
                                                    (IfElse (b, c1, c2))
# 1619 "MiniOOL_parser.ml"
         in
        _menhir_goto_cmd _menhir_env _menhir_stack _menhir_s _v) : 'freshtv136)) : 'freshtv138)
    | MenhirState30 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv143 * _menhir_state) * _menhir_state * 'tv_cmd) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | PARALLEL ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv139 * _menhir_state) * _menhir_state * 'tv_cmd) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ATOM ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState81
            | IDENT _v ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _v
            | IF ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState81
            | LBRACE ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState81
            | LPAREN ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState81
            | MALLOC ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState81
            | MINUS ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState81
            | NULL ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState81
            | NUM _v ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _v
            | PROC ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState81
            | SKIP ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState81
            | VAR ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState81
            | WHILE ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState81
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState81) : 'freshtv140)
        | SEMICOLON ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | RBRACE ->
            _menhir_reduce25 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv141 * _menhir_state) * _menhir_state * 'tv_cmd) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv142)) : 'freshtv144)
    | MenhirState0 | MenhirState78 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv147 * _menhir_state * 'tv_cmd) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | SEMICOLON ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | EOL | RBRACE ->
            _menhir_reduce25 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv145 * _menhir_state * 'tv_cmd) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv146)) : 'freshtv148)
    | MenhirState81 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv155 * _menhir_state) * _menhir_state * 'tv_cmd)) * _menhir_state * 'tv_cmd) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RBRACE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv151 * _menhir_state) * _menhir_state * 'tv_cmd)) * _menhir_state * 'tv_cmd) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv149 * _menhir_state) * _menhir_state * 'tv_cmd)) * _menhir_state * 'tv_cmd) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s), _, (c1 : 'tv_cmd)), _, (c2 : 'tv_cmd)) = _menhir_stack in
            let _5 = () in
            let _3 = () in
            let _1 = () in
            let _v : 'tv_cmd = 
# 163 "MiniOOL_parser.mly"
                                                    (Parallel (c1, c2))
# 1711 "MiniOOL_parser.ml"
             in
            _menhir_goto_cmd _menhir_env _menhir_stack _menhir_s _v) : 'freshtv150)) : 'freshtv152)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv153 * _menhir_state) * _menhir_state * 'tv_cmd)) * _menhir_state * 'tv_cmd) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv154)) : 'freshtv156)
    | MenhirState4 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv159 * _menhir_state) * (
# 118 "MiniOOL_parser.mly"
       (string)
# 1726 "MiniOOL_parser.ml"
        ))) * _menhir_state * 'tv_cmd) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv157 * _menhir_state) * (
# 118 "MiniOOL_parser.mly"
       (string)
# 1732 "MiniOOL_parser.ml"
        ))) * _menhir_state * 'tv_cmd) = Obj.magic _menhir_stack in
        ((let (((_menhir_stack, _menhir_s), (y : (
# 118 "MiniOOL_parser.mly"
       (string)
# 1737 "MiniOOL_parser.ml"
        ))), _, (c : 'tv_cmd)) = _menhir_stack in
        let _3 = () in
        let _1 = () in
        let _v : 'tv_expr = 
# 192 "MiniOOL_parser.mly"
                                  (Procedure (y, c))
# 1744 "MiniOOL_parser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv158)) : 'freshtv160)
    | MenhirState85 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv163 * _menhir_state) * _menhir_state * 'tv_bool_expr) * _menhir_state * 'tv_cmd) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv161 * _menhir_state) * _menhir_state * 'tv_bool_expr) * _menhir_state * 'tv_cmd) = Obj.magic _menhir_stack in
        ((let (((_menhir_stack, _menhir_s), _, (b : 'tv_bool_expr)), _, (c : 'tv_cmd)) = _menhir_stack in
        let _1 = () in
        let _v : 'tv_cmd = 
# 161 "MiniOOL_parser.mly"
                                                    (While (b, c))
# 1757 "MiniOOL_parser.ml"
         in
        _menhir_goto_cmd _menhir_env _menhir_stack _menhir_s _v) : 'freshtv162)) : 'freshtv164)
    | _ ->
        _menhir_fail ()

and _menhir_run32 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL _v ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState32 _v
    | IDENT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState32 _v
    | LPAREN ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | MINUS ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | NOT ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | NULL ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | NUM _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState32 _v
    | PROC ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState32

and _menhir_run33 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL _v ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _v
    | IDENT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _v
    | LPAREN ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState33
    | MINUS ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState33
    | NOT ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState33
    | NULL ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState33
    | NUM _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _v
    | PROC ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState33
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState33

and _menhir_run34 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 120 "MiniOOL_parser.mly"
       (bool)
# 1820 "MiniOOL_parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv119) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((b : (
# 120 "MiniOOL_parser.mly"
       (bool)
# 1830 "MiniOOL_parser.ml"
    )) : (
# 120 "MiniOOL_parser.mly"
       (bool)
# 1834 "MiniOOL_parser.ml"
    )) = _v in
    ((let _v : 'tv_bool_expr = 
# 167 "MiniOOL_parser.mly"
                                         (Bool b)
# 1839 "MiniOOL_parser.ml"
     in
    _menhir_goto_bool_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv120)

and _menhir_reduce36 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 118 "MiniOOL_parser.mly"
       (string)
# 1846 "MiniOOL_parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, (x : (
# 118 "MiniOOL_parser.mly"
       (string)
# 1852 "MiniOOL_parser.ml"
    ))) = _menhir_stack in
    let _v : 'tv_expr = 
# 191 "MiniOOL_parser.mly"
                                  (Ident x)
# 1857 "MiniOOL_parser.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_run12 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 118 "MiniOOL_parser.mly"
       (string)
# 1864 "MiniOOL_parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    _menhir_reduce36 _menhir_env (Obj.magic _menhir_stack)

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState85 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv55 * _menhir_state) * _menhir_state * 'tv_bool_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv56)
    | MenhirState81 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv57 * _menhir_state) * _menhir_state * 'tv_cmd)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv58)
    | MenhirState78 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv59 * _menhir_state * 'tv_cmd)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv60)
    | MenhirState73 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv61 * _menhir_state) * _menhir_state * 'tv_bool_expr) * _menhir_state * 'tv_cmd)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv62)
    | MenhirState67 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv63 * _menhir_state * 'tv_expr)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv64)
    | MenhirState65 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv65 * _menhir_state)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv66)
    | MenhirState62 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv67 * _menhir_state * (
# 118 "MiniOOL_parser.mly"
       (string)
# 1909 "MiniOOL_parser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv68)
    | MenhirState60 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv69 * _menhir_state * (
# 118 "MiniOOL_parser.mly"
       (string)
# 1918 "MiniOOL_parser.ml"
        ))) * (
# 118 "MiniOOL_parser.mly"
       (string)
# 1922 "MiniOOL_parser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv70)
    | MenhirState56 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv71 * _menhir_state) * _menhir_state * 'tv_bool_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv72)
    | MenhirState53 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv73 * _menhir_state * 'tv_bool_expr) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv74)
    | MenhirState52 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv75 * _menhir_state * 'tv_bool_expr) * _menhir_state) * _menhir_state * 'tv_bool_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv76)
    | MenhirState50 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv77 * _menhir_state * 'tv_bool_expr) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv78)
    | MenhirState48 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv79 * _menhir_state) * _menhir_state * 'tv_bool_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv80)
    | MenhirState46 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv81 * _menhir_state * 'tv_expr)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv82)
    | MenhirState44 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv83 * _menhir_state * 'tv_expr)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv84)
    | MenhirState42 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv85 * _menhir_state * 'tv_expr)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv86)
    | MenhirState40 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv87 * _menhir_state * 'tv_expr)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv88)
    | MenhirState38 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv89 * _menhir_state * 'tv_expr)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv90)
    | MenhirState36 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv91 * _menhir_state * 'tv_expr)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv92)
    | MenhirState33 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv93 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv94)
    | MenhirState32 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv95 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv96)
    | MenhirState31 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv97 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv98)
    | MenhirState30 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv99 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv100)
    | MenhirState23 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv101 * _menhir_state * 'tv_expr)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv102)
    | MenhirState21 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv103 * _menhir_state * 'tv_expr)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv104)
    | MenhirState19 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv105 * _menhir_state * 'tv_expr)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv106)
    | MenhirState14 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv107 * _menhir_state * 'tv_expr)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv108)
    | MenhirState11 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv109 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv110)
    | MenhirState10 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv111 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv112)
    | MenhirState4 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv113 * _menhir_state) * (
# 118 "MiniOOL_parser.mly"
       (string)
# 2036 "MiniOOL_parser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv114)
    | MenhirState1 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv115 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv116)
    | MenhirState0 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv117) = Obj.magic _menhir_stack in
        (raise _eRR : 'freshtv118)

and _menhir_reduce26 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_cmds = 
# 147 "MiniOOL_parser.mly"
                                 ([])
# 2055 "MiniOOL_parser.ml"
     in
    _menhir_goto_cmds _menhir_env _menhir_stack _menhir_s _v

and _menhir_run1 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL _v ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState1 _v
    | IDENT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState1 _v
    | LPAREN ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState1
    | MINUS ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState1
    | NOT ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState1
    | NULL ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState1
    | NUM _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState1 _v
    | PROC ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState1
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState1

and _menhir_run5 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENT _v ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv51 * _menhir_state) = Obj.magic _menhir_stack in
        let (_v : (
# 118 "MiniOOL_parser.mly"
       (string)
# 2098 "MiniOOL_parser.ml"
        )) = _v in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv49 * _menhir_state) = Obj.magic _menhir_stack in
        let ((x : (
# 118 "MiniOOL_parser.mly"
       (string)
# 2106 "MiniOOL_parser.ml"
        )) : (
# 118 "MiniOOL_parser.mly"
       (string)
# 2110 "MiniOOL_parser.ml"
        )) = _v in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        let _1 = () in
        let _v : 'tv_cmd = 
# 150 "MiniOOL_parser.mly"
                                                    (Declare x)
# 2117 "MiniOOL_parser.ml"
         in
        _menhir_goto_cmd _menhir_env _menhir_stack _menhir_s _v) : 'freshtv50)) : 'freshtv52)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv53 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv54)

and _menhir_run7 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv47) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _1 = () in
    let _v : 'tv_cmd = 
# 159 "MiniOOL_parser.mly"
                                                    (Skip)
# 2138 "MiniOOL_parser.ml"
     in
    _menhir_goto_cmd _menhir_env _menhir_stack _menhir_s _v) : 'freshtv48)

and _menhir_run2 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENT _v ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv43 * _menhir_state) = Obj.magic _menhir_stack in
        let (_v : (
# 118 "MiniOOL_parser.mly"
       (string)
# 2154 "MiniOOL_parser.ml"
        )) = _v in
        ((let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | COLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv39 * _menhir_state) * (
# 118 "MiniOOL_parser.mly"
       (string)
# 2165 "MiniOOL_parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ATOM ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState4
            | IDENT _v ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState4 _v
            | IF ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState4
            | LBRACE ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState4
            | LPAREN ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState4
            | MALLOC ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState4
            | MINUS ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState4
            | NULL ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState4
            | NUM _v ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState4 _v
            | PROC ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState4
            | SKIP ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState4
            | VAR ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState4
            | WHILE ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState4
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState4) : 'freshtv40)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv41 * _menhir_state) * (
# 118 "MiniOOL_parser.mly"
       (string)
# 2207 "MiniOOL_parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv42)) : 'freshtv44)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv45 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv46)

and _menhir_run8 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 119 "MiniOOL_parser.mly"
       (int)
# 2222 "MiniOOL_parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv37) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((n : (
# 119 "MiniOOL_parser.mly"
       (int)
# 2232 "MiniOOL_parser.ml"
    )) : (
# 119 "MiniOOL_parser.mly"
       (int)
# 2236 "MiniOOL_parser.ml"
    )) = _v in
    ((let _v : 'tv_expr = 
# 180 "MiniOOL_parser.mly"
                                  (Num n)
# 2241 "MiniOOL_parser.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv38)

and _menhir_run9 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv35) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _1 = () in
    let _v : 'tv_expr = 
# 187 "MiniOOL_parser.mly"
                                  (Null)
# 2255 "MiniOOL_parser.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv36)

and _menhir_run10 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState10 _v
    | LPAREN ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState10
    | MINUS ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState10
    | NULL ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState10
    | NUM _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState10 _v
    | PROC ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState10
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState10

and _menhir_run26 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAREN ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv31 * _menhir_state) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | IDENT _v ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv27 * _menhir_state)) = Obj.magic _menhir_stack in
            let (_v : (
# 118 "MiniOOL_parser.mly"
       (string)
# 2300 "MiniOOL_parser.ml"
            )) = _v in
            ((let _menhir_stack = (_menhir_stack, _v) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | RPAREN ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv23 * _menhir_state)) * (
# 118 "MiniOOL_parser.mly"
       (string)
# 2311 "MiniOOL_parser.ml"
                )) = Obj.magic _menhir_stack in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv21 * _menhir_state)) * (
# 118 "MiniOOL_parser.mly"
       (string)
# 2318 "MiniOOL_parser.ml"
                )) = Obj.magic _menhir_stack in
                ((let ((_menhir_stack, _menhir_s), (x : (
# 118 "MiniOOL_parser.mly"
       (string)
# 2323 "MiniOOL_parser.ml"
                ))) = _menhir_stack in
                let _4 = () in
                let _2 = () in
                let _1 = () in
                let _v : 'tv_cmd = 
# 156 "MiniOOL_parser.mly"
                                                    (Malloc x)
# 2331 "MiniOOL_parser.ml"
                 in
                _menhir_goto_cmd _menhir_env _menhir_stack _menhir_s _v) : 'freshtv22)) : 'freshtv24)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv25 * _menhir_state)) * (
# 118 "MiniOOL_parser.mly"
       (string)
# 2341 "MiniOOL_parser.ml"
                )) = Obj.magic _menhir_stack in
                ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv26)) : 'freshtv28)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv29 * _menhir_state)) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv30)) : 'freshtv32)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv33 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv34)

and _menhir_run11 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState11 _v
    | LPAREN ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState11
    | MINUS ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState11
    | NULL ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState11
    | NUM _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState11 _v
    | PROC ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState11
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState11

and _menhir_run30 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ATOM ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | IDENT _v ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _v
    | IF ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | LBRACE ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | LPAREN ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | MALLOC ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | MINUS ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | NULL ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | NUM _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _v
    | PROC ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | SKIP ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | VAR ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | WHILE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | RBRACE ->
        _menhir_reduce26 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState30

and _menhir_run31 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL _v ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _v
    | IDENT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _v
    | LPAREN ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | MINUS ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | NOT ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | NULL ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | NUM _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _v
    | PROC ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState31

and _menhir_run57 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 118 "MiniOOL_parser.mly"
       (string)
# 2452 "MiniOOL_parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ASSIGN ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv7 * _menhir_state * (
# 118 "MiniOOL_parser.mly"
       (string)
# 2464 "MiniOOL_parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | IDENT _v ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _v
        | LPAREN ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState62
        | MINUS ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState62
        | NULL ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState62
        | NUM _v ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _v
        | PROC ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState62
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState62) : 'freshtv8)
    | DOT ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv17 * _menhir_state * (
# 118 "MiniOOL_parser.mly"
       (string)
# 2490 "MiniOOL_parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | IDENT _v ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv13 * _menhir_state * (
# 118 "MiniOOL_parser.mly"
       (string)
# 2500 "MiniOOL_parser.ml"
            ))) = Obj.magic _menhir_stack in
            let (_v : (
# 118 "MiniOOL_parser.mly"
       (string)
# 2505 "MiniOOL_parser.ml"
            )) = _v in
            ((let _menhir_stack = (_menhir_stack, _v) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ASSIGN ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv9 * _menhir_state * (
# 118 "MiniOOL_parser.mly"
       (string)
# 2516 "MiniOOL_parser.ml"
                ))) * (
# 118 "MiniOOL_parser.mly"
       (string)
# 2520 "MiniOOL_parser.ml"
                )) = Obj.magic _menhir_stack in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | IDENT _v ->
                    _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _v
                | LPAREN ->
                    _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState60
                | MINUS ->
                    _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState60
                | NULL ->
                    _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState60
                | NUM _v ->
                    _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _v
                | PROC ->
                    _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState60
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState60) : 'freshtv10)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv11 * _menhir_state * (
# 118 "MiniOOL_parser.mly"
       (string)
# 2548 "MiniOOL_parser.ml"
                ))) * (
# 118 "MiniOOL_parser.mly"
       (string)
# 2552 "MiniOOL_parser.ml"
                )) = Obj.magic _menhir_stack in
                ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv12)) : 'freshtv14)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv15 * _menhir_state * (
# 118 "MiniOOL_parser.mly"
       (string)
# 2563 "MiniOOL_parser.ml"
            ))) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv16)) : 'freshtv18)
    | DIV | LPAREN | MINUS | PLUS | TIMES ->
        _menhir_reduce36 _menhir_env (Obj.magic _menhir_stack)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv19 * _menhir_state * (
# 118 "MiniOOL_parser.mly"
       (string)
# 2576 "MiniOOL_parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv20)

and _menhir_run64 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAREN ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv3 * _menhir_state) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ATOM ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | IDENT _v ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _v
        | IF ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | LBRACE ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | LPAREN ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | MALLOC ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | MINUS ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | NULL ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | NUM _v ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _v
        | PROC ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | SKIP ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | VAR ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | WHILE ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState65) : 'freshtv4)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv5 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv6)

and _menhir_discard : _menhir_env -> _menhir_env =
  fun _menhir_env ->
    let lexer = _menhir_env._menhir_lexer in
    let lexbuf = _menhir_env._menhir_lexbuf in
    let _tok = lexer lexbuf in
    {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = _tok;
      _menhir_error = false;
    }

and prog : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (
# 123 "MiniOOL_parser.mly"
      (unit)
# 2646 "MiniOOL_parser.ml"
) =
  fun lexer lexbuf ->
    let _menhir_env =
      let (lexer : Lexing.lexbuf -> token) = lexer in
      let (lexbuf : Lexing.lexbuf) = lexbuf in
      ((let _tok = Obj.magic () in
      {
        _menhir_lexer = lexer;
        _menhir_lexbuf = lexbuf;
        _menhir_token = _tok;
        _menhir_error = false;
      }) : _menhir_env)
    in
    Obj.magic (let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv1) = ((), _menhir_env._menhir_lexbuf.Lexing.lex_curr_p) in
    ((let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ATOM ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | IDENT _v ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | IF ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | LBRACE ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | LPAREN ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | MALLOC ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | MINUS ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | NULL ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | NUM _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | PROC ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | SKIP ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | VAR ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | WHILE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | EOL ->
        _menhir_reduce26 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0) : 'freshtv2))

# 195 "MiniOOL_parser.mly"
   (* trailer *)

# 2701 "MiniOOL_parser.ml"

# 233 "/Users/zachary/.opam/default/lib/menhir/standard.mly"
  

# 2706 "MiniOOL_parser.ml"
