open AbstractSyntaxTree;;
open To_string;;

let rec field_to_tree_string id prefix : string =
  let new_prefix = (prefix ^ "\t") in
  prefix ^ (match id with
      | Field (x, f) -> (
          ".\n" ^
          new_prefix ^ !x ^ "\n" ^
          new_prefix ^ f)
      | FieldExpr (id', f) -> (
          ".\n" ^
          (field_to_tree_string id' new_prefix) ^ "\n" ^
          new_prefix ^ f));;

let rec bool_expr_to_tree_string b prefix =
  let new_prefix = (prefix ^ "\t") in
  prefix ^ (match b with
      | Bool b -> if b then "true" else "false"
      | ComparisonBinaryOperator (op, e1, e2) ->
        ((bool_op_to_string b) ^ "\n" ^
         (expr_to_tree_string e1 new_prefix) ^ "\n" ^
         (expr_to_tree_string e2 new_prefix))
      | BoolUnaryOperator (op, b1) ->
        ((bool_op_to_string b) ^ "\n" ^
         (bool_expr_to_tree_string b1 new_prefix))
      | BoolBinaryOperator (op, b1, b2) ->
        ((bool_op_to_string b) ^ "\n" ^
         (bool_expr_to_tree_string b1 new_prefix) ^ "\n" ^
         (bool_expr_to_tree_string b2 new_prefix)))

and expr_to_tree_string e prefix : string =
  let new_prefix = (prefix ^ "\t") in match e with
  | FieldAccess id -> (field_to_tree_string id prefix)
  | _ -> prefix ^ (
      match e with
      | Num n -> string_of_int n
      | Null -> "null"
      | Ident x -> !x
      | ArithmeticBinaryOperator (op, e1, e2) ->
        ((arithmetic_op_to_string e) ^ "\n" ^
         (expr_to_tree_string e1 new_prefix) ^ "\n" ^
         (expr_to_tree_string e2 new_prefix))
      | ArithmeticUnaryOperator (op, e1) ->
        ((arithmetic_op_to_string e) ^ "\n" ^
         (expr_to_tree_string e1 new_prefix))
      | Procedure (y, c) ->(
          "proc\n" ^
          new_prefix ^ !y ^ "\n" ^
          (cmd_to_tree_string c new_prefix))
      | _ -> "")

and cmd_to_tree_string c prefix : string =
  let new_prefix = (prefix ^ "\t") in (prefix ^ (
      match c with
      | Declare x -> ("var\n" ^ new_prefix ^ !x)
      | ProceduceCall (p, y) -> (
          "ProceduceCall\n" ^
          (expr_to_tree_string p new_prefix) ^ "\n" ^
          (expr_to_tree_string y new_prefix))
      | MallocVar   x  -> ("malloc\n" ^ new_prefix ^ !x)
      | MallocField xf -> ("malloc\n" ^ (field_to_tree_string xf new_prefix))
      | Assign (x, e) ->
        ("=\n" ^ new_prefix ^ !x ^ "\n" ^ (expr_to_tree_string e new_prefix))
      | FieldAssign (id, e) ->(
          "=\n" ^
          (field_to_tree_string id new_prefix) ^ "\n" ^
          (expr_to_tree_string e new_prefix))
      | Skip -> "skip"
      | Cmds cs -> ("cmds\n" ^ (cmds_to_tree_string cs new_prefix))
      | While (b, c) ->
        ("while\n" ^
         (bool_expr_to_tree_string b new_prefix) ^ "\n" ^
         (cmd_to_tree_string c new_prefix))
      | IfElse (b, c1, c2) ->
        ("ifelse\n" ^
         (bool_expr_to_tree_string b new_prefix) ^ " \n" ^
         (cmd_to_tree_string c1 new_prefix) ^ "\n" ^
         (cmd_to_tree_string c2 new_prefix))
      | Parallel (c1, c2) ->
        ("|||\n" ^
         (cmd_to_tree_string c1 new_prefix) ^ "\n" ^
         (cmd_to_tree_string c2 new_prefix))
      | Atom c -> ("atom\n" ^ (cmd_to_tree_string c new_prefix))))

and cmds_to_tree_string cs prefix : string =
  match cs with
  | [] -> ""
  | h :: t -> ((cmd_to_tree_string h prefix) ^ "\n" ^
               (cmds_to_tree_string t prefix));;
