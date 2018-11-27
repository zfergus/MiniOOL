(** Create a string of of the abstract syntax tree types for MiniOOL.
    @author Zachary Ferguson *)

open AbstractSyntaxTree;;


(** Create a string of the arithmetic operator.
    @param e UnaryArithmeticOperator or BinaryArithmeticOperator expresion.
    @return a string representation of the operator. *)
let string_of_arithmetic_op e = match e with
  | BinaryArithmeticOperator (op, a, b) -> (match (op 25 10) with
      | 15  -> "-"
      | 35  -> "+"
      | 250 -> "*"
      | 2   -> "/";
      | 5   -> "%";
      | _   -> "op")
  | UnaryArithmeticOperator (op, e) -> if (op 1) == -1 then "-" else "op"
  | _ -> "op"


(** Create a string of the boolean operator.
    @param b BinaryComparisonOperator, UnaryLogicOperator, BinaryLogicOperator expresion.
    @return a string representation of the operator. *)
let string_of_bool_op b = match b with
  | BinaryComparisonOperator (op, a, b) -> (match (op 2 1) with
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
  | BinaryLogicOperator (op, a, b) -> (match (op true false) with
      | false -> "and"
      | true  -> "or")
  | UnaryLogicOperator (op, e) -> "not"
  | _ -> "op"

(** Create a string of the boolean expresion.
    @param e Boolean expresion to stringify.
    @return a string representation of the boolean expression. *)
let rec string_of_bool_expr b = match b with
  | Bool b -> if b then "true" else "false"
  | BinaryComparisonOperator (op, e1, e2) ->
    Printf.sprintf "(%s %s %s)" (string_of_expr e1) (string_of_bool_op b)
      (string_of_expr e2)
  | UnaryLogicOperator (op, b1) ->
    Printf.sprintf "(%s %s)" (string_of_bool_op b) (string_of_bool_expr b1)
  | BinaryLogicOperator (op, b1, b2) ->
    Printf.sprintf "(%s %s %s)" (string_of_bool_expr b1) (string_of_bool_op b)
      (string_of_bool_expr b2)


(** Create a string of the expresion.
    @param e Expresion to stringify.
    @return a string representation of the expression. *)
and string_of_expr e = match e with
  | Field f -> f
  | Num n -> string_of_int n
  | BinaryArithmeticOperator (op, e1, e2) ->
    Printf.sprintf "(%s %s %s)" (string_of_expr e1) (string_of_arithmetic_op e)
      (string_of_expr e2)
  | UnaryArithmeticOperator (op, e1) ->
    Printf.sprintf "(%s%s)" (string_of_arithmetic_op e) (string_of_expr e1)
  | Null -> "null"
  | Variable x -> !x
  | FieldAccess (e1, e2) ->
    Printf.sprintf "(%s.%s)" (string_of_expr e1) (string_of_expr e2)
  | Procedure (y, c) ->
    Printf.sprintf "(proc %s: %s)" !y (string_of_cmd c)


(** Create a string of the command.
    @param e Command to stringify.
    @return a string representation of the command. *)
and string_of_cmd c = match c with
  | Declare x -> Printf.sprintf "{var %s}" !x
  | ProceduceCall (p, y) ->
    Printf.sprintf "{%s(%s)}" (string_of_expr p) (string_of_expr y)
  | Malloc x  -> Printf.sprintf "{malloc(%s)}" !x
  (* | MallocField xf -> Printf.sprintf "{malloc(%s)}" (string_of_field xf) *)
  | Assign (x, e) -> Printf.sprintf "{%s = %s}" !x (string_of_expr e)
  | FieldAssign (e1, e2, e3) ->
    Printf.sprintf "{%s.%s = %s}" (string_of_expr e1) (string_of_expr e2) (string_of_expr e3)
  | Skip -> "{skip}"
  | CmdSequence cs -> Printf.sprintf "{%s}" (string_of_cmds cs)
  | While (b, c) ->
    Printf.sprintf "{while %s %s}" (string_of_bool_expr b) (string_of_cmd c)
  | IfElse (b, c1, c2) ->
    Printf.sprintf "{if %s then %s else %s}"
      (string_of_bool_expr b) (string_of_cmd c1) (string_of_cmd c2)
  | Parallel (c1, c2) ->
    Printf.sprintf "{%s ||| %s}" (string_of_cmd c1) (string_of_cmd c2)
  | Atom c -> Printf.sprintf "{atom(%s)}" (string_of_cmd c)


(** Create a string of the commands.
    @param e Commands to stringify.
    @return a string representation of the commands. *)
and string_of_cmds cmds = match cmds with
  | [] -> "";
  | h :: t ->
    Printf.sprintf "%s%s%s"
      (string_of_cmd h) (if t == [] then "" else "; ") (string_of_cmds t);;
