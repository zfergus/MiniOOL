(** Create a string of of the abstract syntax tree types for MiniOOL.
    @author Zachary Ferguson *)

open AbstractSyntaxTree;;


(** Create a string of the field.
    @param f The field to stringify.
    @return a string representation of the field. *)
let rec field_to_string id = Printf.sprintf "(%s)" (match id with
    | Field (x, f) -> !x ^ "." ^ f
    | FieldExpr (id', f) -> (field_to_string id') ^ "." ^ f);;


(** Create a string of the arithmetic operator.
    @param e ArithmeticUnaryOperator or ArithmeticBinaryOperator expresion.
    @return a string representation of the operator. *)
let arithmetic_op_to_string e = match e with
  | ArithmeticBinaryOperator (op, a, b) -> (match (op 25 10) with
      | 15  -> "-"
      | 35  -> "+"
      | 250 -> "*"
      | 2   -> "/";
      | 5   -> "%";
      | _   -> "op")
  | ArithmeticUnaryOperator (op, e) -> if (op 1) == -1 then "-" else "op"
  | _ -> "op"


(** Create a string of the boolean operator.
    @param b ComparisonBinaryOperator, BoolUnaryOperator, BoolBinaryOperator expresion.
    @return a string representation of the operator. *)
let bool_op_to_string b = match b with
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
      | false -> "and"
      | true  -> "or")
  | BoolUnaryOperator (op, e) -> "not"
  | _ -> "op"

(** Create a string of the boolean expresion.
    @param e Boolean expresion to stringify.
    @return a string representation of the boolean expression. *)
let rec bool_expr_to_string b = match b with
  | Bool b -> if b then "true" else "false"
  | ComparisonBinaryOperator (op, e1, e2) ->
    Printf.sprintf "(%s %s %s)" (expr_to_string e1) (bool_op_to_string b)
      (expr_to_string e2)
  | BoolUnaryOperator (op, b1) ->
    Printf.sprintf "(%s %s)" (bool_op_to_string b) (bool_expr_to_string b1)
  | BoolBinaryOperator (op, b1, b2) ->
    Printf.sprintf "(%s %s %s)" (bool_expr_to_string b1) (bool_op_to_string b)
      (bool_expr_to_string b2)


(** Create a string of the expresion.
    @param e Expresion to stringify.
    @return a string representation of the expression. *)
and expr_to_string e = match e with
  | Num n -> string_of_int n
  | Null -> "null"
  | Ident x -> !x
  | ArithmeticBinaryOperator (op, e1, e2) ->
    Printf.sprintf "(%s %s %s)" (expr_to_string e1) (arithmetic_op_to_string e)
      (expr_to_string e2)
  | ArithmeticUnaryOperator (op, e1) ->
    Printf.sprintf "(%s%s)" (arithmetic_op_to_string e) (expr_to_string e1)
  | FieldAccess id -> Printf.sprintf "(%s)" (field_to_string id)
  | Procedure (y, c) ->
    Printf.sprintf "(proc %s: %s)" !y (cmd_to_string c)


(** Create a string of the command.
    @param e Command to stringify.
    @return a string representation of the command. *)
and cmd_to_string c = match c with
  | Declare x -> Printf.sprintf "{var %s}" !x
  | ProceduceCall (p, y) ->
    Printf.sprintf "{%s(%s)}" (expr_to_string p) (expr_to_string y)
  | MallocVar   x  -> Printf.sprintf "{malloc(%s)}" !x
  | MallocField xf -> Printf.sprintf "{malloc(%s)}" (field_to_string xf)
  | Assign (x, e) -> Printf.sprintf "{%s = %s}" !x (expr_to_string e)
  | FieldAssign (id, e) ->
    Printf.sprintf "{%s = %s}" (field_to_string id) (expr_to_string e)
  | Skip -> "{skip}"
  | CmdSequence cs -> Printf.sprintf "{%s}" (cmds_to_string cs)
  | While (b, c) ->
    Printf.sprintf "{while %s %s}" (bool_expr_to_string b) (cmd_to_string c)
  | IfElse (b, c1, c2) ->
    Printf.sprintf "{if %s then %s else %s}"
      (bool_expr_to_string b) (cmd_to_string c1) (cmd_to_string c2)
  | Parallel (c1, c2) ->
    Printf.sprintf "{%s ||| %s}" (cmd_to_string c1) (cmd_to_string c2)
  | Atom c -> Printf.sprintf "{atom(%s)}" (cmd_to_string c)


(** Create a string of the commands.
    @param e Commands to stringify.
    @return a string representation of the commands. *)
and cmds_to_string cmds = match cmds with
  | [] -> "";
  | h :: t -> Printf.sprintf "%s; %s" (cmd_to_string h) (cmds_to_string t);;
