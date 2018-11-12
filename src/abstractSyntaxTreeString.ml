(** Create a string of a tree of the abstract syntax tree types for MiniOOL.
    @author Zachary Ferguson *)

open AbstractSyntaxTree;;
open ProgramString;;


(** Type of lines in the tree string. *)
type depth_marker =
    Down (** Vertical bar for the different layers: "│   " *)
  | UpTurn (** Up and turn to the right to a node "╭──" *)
  | DownTurn (** Down and turn to the right to a node "╰──" *)
  | Branch (** Branch off from a layer to a node in the tree "├──" *)
  | None (** Do not print a marker "    " *)


(** Convert the marker to the corresponging string.
    @param mkr depth_marker to convert to a string
    @return the string representation of the marker. *)
let string_of_depth_marker mkr = match mkr with
  | Down -> "│   "
  | UpTurn -> "╭──"
  | DownTurn -> "╰──"
  | Branch -> "├──"
  | None -> "    ";;


(** Add a line containing no nodes but the apporiate markers.
    @param line The line to surround.
    @param prefix The current prefix line.
    @param mkr The marker for the line being surrounded.
    @return the line with pre/apended extra line. *)
let add_empty_line line prefix mkr =
  let extra_line = prefix ^ (string_of_depth_marker Down) in
  match mkr with
  | UpTurn -> line ^ "\n" ^ extra_line
  | _ -> extra_line ^ "\n" ^ line;;


(** Get the previous line's prefix based on the current depth marker.
    @param prefix The current line's prefix.
    @param mkr The depth marker for the current line
    @return the prefix appended with the marker string for the current layer. *)
let mkr_to_prev_line_prefix prefix mkr = prefix ^ string_of_depth_marker (
    if mkr = UpTurn then None else Down)


(** Get the next line's prefix based on the current depth marker.
    @param prefix The current line's prefix.
    @param mkr The depth marker for the current line
    @return the prefix appended with the marker string for the current layer. *)
let mkr_to_next_line_prefix prefix mkr = prefix ^ string_of_depth_marker (
    if mkr = Branch || mkr = UpTurn then Down else None)


(** Create a string of the field as string representation of a tree.
    @param id The field to stringify.
    @param prefix The prefix for the line to include other layers.
    @param mkr The depth marker for how the node should be added.
    @return field as string representation of a tree. *)
let rec tree_string_of_field id prefix mkr =
  let prev_line_prefix = mkr_to_prev_line_prefix prefix mkr
  and next_line_prefix = mkr_to_next_line_prefix prefix mkr
  and new_prefix = prefix ^ (string_of_depth_marker mkr) in
  match id with
  | Field (x, f) ->
    (tree_string_of_expr (Ident x) prev_line_prefix UpTurn) ^ "\n" ^
    new_prefix ^ "(.)\n" ^
    (tree_string_of_expr (Ident (ref f)) next_line_prefix DownTurn)
  | FieldExpr (id', f) ->
    (tree_string_of_field id' prev_line_prefix UpTurn) ^ "\n" ^
    new_prefix ^ "(.)\n" ^
    (tree_string_of_expr (Ident (ref f)) next_line_prefix DownTurn)


(** Create a string of an boolean expression as string representation of a tree.
    @param b The boolean expression to stringify.
    @param prefix The prefix for the line to include other layers.
    @param mkr The depth marker for how the node should be added.
    @return boolean expression as string representation of a tree. *)
and tree_string_of_bool_expr b prefix mkr =
  let prev_line_prefix = mkr_to_prev_line_prefix prefix mkr
  and next_line_prefix = mkr_to_next_line_prefix prefix mkr
  and new_prefix = prefix ^ (string_of_depth_marker mkr) in
  match b with
  | Bool b -> new_prefix ^ if b then "(true)" else "(false)"
  | ComparisonBinaryOperator (op, e1, e2) ->
    (tree_string_of_expr e1 prev_line_prefix UpTurn) ^ "\n" ^
    Printf.sprintf "%s(%s)\n" new_prefix (bool_op_to_string b) ^
    (tree_string_of_expr e2 next_line_prefix DownTurn)
  | BoolUnaryOperator (op, b1) ->
    Printf.sprintf "%s(%s)\n" new_prefix (bool_op_to_string b) ^
    (tree_string_of_bool_expr b1 next_line_prefix DownTurn)
  | BoolBinaryOperator (op, b1, b2) ->
    (tree_string_of_bool_expr b1 prev_line_prefix UpTurn) ^ "\n" ^
    Printf.sprintf "%s(%s)\n" new_prefix (bool_op_to_string b) ^
    (tree_string_of_bool_expr b2 next_line_prefix DownTurn)


(** Create a string of an expression as string representation of a tree.
    @param e The expression to stringify.
    @param prefix The prefix for the line to include other layers.
    @param mkr The depth marker for how the node should be added.
    @return expression as string representation of a tree. *)
and tree_string_of_expr e prefix mkr =
  let prev_line_prefix = mkr_to_prev_line_prefix prefix mkr
  and next_line_prefix = mkr_to_next_line_prefix prefix mkr
  and new_prefix = prefix ^ (string_of_depth_marker mkr) in
  match e with
  | Num n -> new_prefix ^ Printf.sprintf "(%d)" n
  | Null -> new_prefix ^ "(null)"
  | Ident x -> Printf.sprintf "%s(%s)" new_prefix !x
  | FieldAccess f -> tree_string_of_field f prefix mkr
  | ArithmeticBinaryOperator (op, e1, e2) ->
    (tree_string_of_expr e1 prev_line_prefix UpTurn) ^ "\n" ^
    new_prefix ^ (Printf.sprintf "(%s)\n" (arithmetic_op_to_string e)) ^
    (tree_string_of_expr e2 next_line_prefix DownTurn)
  | ArithmeticUnaryOperator (op, e1) ->
    new_prefix ^ (Printf.sprintf "(%s)\n" (arithmetic_op_to_string e)) ^
    (tree_string_of_expr e1 next_line_prefix DownTurn)
  | Procedure (y, c) ->
    new_prefix ^ "(proc)\n" ^
    (tree_string_of_expr (Ident y) next_line_prefix Branch) ^ "\n" ^
    (tree_string_of_cmd c  next_line_prefix DownTurn)


(** Create a string of a command as string representation of a tree.
    @param c The command to stringify.
    @param prefix The prefix for the line to include other layers.
    @param mkr The depth marker for how the node should be added.
    @return command as string representation of a tree. *)
and tree_string_of_cmd c prefix mkr =
  let prev_line_prefix = mkr_to_prev_line_prefix prefix mkr
  and next_line_prefix = mkr_to_next_line_prefix prefix mkr
  and new_prefix = prefix ^ (string_of_depth_marker mkr) in
  match c with
  | Declare x ->
    new_prefix ^ "(var)\n" ^
    (tree_string_of_expr (Ident x) next_line_prefix DownTurn)
  | ProceduceCall (p, y) ->
    new_prefix ^ "(ProceduceCall)\n" ^
    (tree_string_of_expr p next_line_prefix Branch) ^ "\n" ^
    (tree_string_of_expr y next_line_prefix DownTurn)
  | MallocVar   x  ->
    new_prefix ^ "(malloc)\n" ^
    (tree_string_of_expr (Ident x) next_line_prefix DownTurn)
  | MallocField xf ->
    new_prefix ^ "(malloc)\n" ^
    (tree_string_of_field xf next_line_prefix DownTurn)
  | Assign (x, e) ->
    (tree_string_of_expr (Ident x) prev_line_prefix UpTurn) ^ "\n" ^
    new_prefix ^ "(=)\n" ^
    (tree_string_of_expr e next_line_prefix DownTurn)
  | FieldAssign (id, e) ->
    (tree_string_of_field id prev_line_prefix UpTurn) ^ "\n" ^
    new_prefix ^ "(=)\n" ^
    (tree_string_of_expr e next_line_prefix DownTurn)
  | Skip -> new_prefix ^ "(skip)"
  | CmdSequence cs ->
    new_prefix ^ "(cmds)\n" ^
    (tree_string_of_cmds cs next_line_prefix)
  | While (b, c) ->
    new_prefix ^ "(while)\n" ^
    (tree_string_of_bool_expr b next_line_prefix Branch) ^ "\n" ^
    (tree_string_of_cmd c next_line_prefix DownTurn)
  | IfElse (b, c1, c2) ->
    new_prefix ^ "(ifelse)\n" ^
    (tree_string_of_bool_expr b next_line_prefix Branch) ^ " \n" ^
    (tree_string_of_cmd c1 next_line_prefix Branch) ^ "\n" ^
    (tree_string_of_cmd c2 next_line_prefix DownTurn)
  | Parallel (c1, c2) ->
    (tree_string_of_cmd c1 prev_line_prefix UpTurn) ^ "\n" ^
    new_prefix ^ "(|||)\n" ^
    (tree_string_of_cmd c2 next_line_prefix DownTurn)
  | Atom c ->
    new_prefix ^ "(atom)\n" ^
    (tree_string_of_cmd c next_line_prefix DownTurn)


(** Create a string of commands as string representation of a tree.
    @param b The commands to stringify.
    @param prefix The prefix for the line to include other layers.
    @return commands as string representation of a tree. *)
and tree_string_of_cmds cs prefix =
  match cs with
  | [] -> ""
  | h :: t ->
    (tree_string_of_cmd h prefix (if t = [] then DownTurn else Branch)) ^
    if t = [] then "" else ("\n" ^ (tree_string_of_cmds t prefix));;
