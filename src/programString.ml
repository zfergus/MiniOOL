open AbstractSyntaxTree;;

let rec field_to_string id =
  "(" ^
  (match id with
   | Field (x, f) -> (!x ^ "." ^ f)
   | FieldExpr (id', f) -> ((field_to_string id') ^ "." ^ f))
  ^ ")";;

let arithmetic_op_to_string e padding =
  let op_str = (match e with
      | ArithmeticBinaryOperator (op, a, b) -> (match (op 25 10) with
          | 15  -> "-"
          | 35  -> "+"
          | 250 -> "*"
          | 2   -> "/";
          | 5   -> "%";
          | _   -> "op")
      | ArithmeticUnaryOperator (op, e) -> if (op 1) == -1 then "-" else "op"
      | _ -> "op") in
  padding ^ op_str ^ padding;;

let bool_op_to_string e padding =
  let op_str = (match e with
      | ComparisonBinaryOperator (op, a, b) -> (match (op 2 1) with
          | true -> (match (op 1 2) with
              | true -> "!="
              | false -> (match (op 1 1) with
                  | true -> ">="
                  | false -> ">"))
          | false -> (match (op 1 2) with
              | true -> (match (op 1 1) with
                  | true -> "<="
                  | false -> "<")
              | false -> "=="))
      | BoolBinaryOperator (op, a, b) -> (match (op true false) with
          | false -> "&&"
          | true  -> "||")
      | BoolUnaryOperator (op, e) -> "not"
      | _ -> "op") in
  padding ^ op_str ^ padding;;

let rec bool_expr_to_string b = match b with
  | Bool b -> if b then "true" else "false"
  | _ -> ("(" ^
          (match b with
           | ComparisonBinaryOperator (op, e1, e2) ->
             ((expr_to_string e1) ^ (bool_op_to_string b " ") ^ (expr_to_string e2))
           | BoolUnaryOperator (op, b1) ->
             ((bool_op_to_string b " ") ^ (bool_expr_to_string b1))
           | BoolBinaryOperator (op, b1, b2) ->
             ((bool_expr_to_string b1) ^ (bool_op_to_string b " ") ^ (bool_expr_to_string b2))
           | _ -> "")
          ^ ")")

and expr_to_string e = match e with
  | Num n -> string_of_int n
  | Null -> "null"
  | Ident x -> !x
  | _ -> ("(" ^
          (match e with
           | ArithmeticBinaryOperator (op, e1, e2) ->
             ((expr_to_string e1) ^ (arithmetic_op_to_string e " ") ^ (expr_to_string e2))
           | ArithmeticUnaryOperator (op, e1) ->
             ((arithmetic_op_to_string e " ") ^ (expr_to_string e1))
           | FieldAccess id -> (field_to_string id)
           | Procedure (y, c) ->
             ("proc " ^ !y ^ ": " ^ (cmd_to_string c))
           | _ -> "")
          ^ ")")

and cmd_to_string c =
  "{" ^
  (match c with
   | Declare x -> ("var " ^ !x)
   | ProceduceCall (p, y) ->
     ((expr_to_string p) ^ "(" ^ (expr_to_string y) ^ ")")
   | MallocVar   x  -> ("malloc(" ^ !x ^ ")")
   | MallocField xf -> ("malloc(" ^ (field_to_string xf) ^ ")")
   | Assign (x, e) -> (!x ^ " = " ^ (expr_to_string e))
   | FieldAssign (id, e) ->
     ((field_to_string id) ^ " = " ^ (expr_to_string e))
   | Skip -> "skip"
   | CmdSequence cs -> (cmds_to_string cs)
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
  | h :: t -> ((cmd_to_string h) ^ ";" ^ (cmds_to_string t));;
