open AbstractSyntaxTree;;
open ProgramString;;

type depth_marker = Down | UpTurn | DownTurn | Branch | None

let string_of_depth_marker marker = match marker with
  | Down -> "│   "
  | UpTurn -> "╭──"
  | DownTurn -> "╰──"
  | Branch -> "├──"
  | None -> "    ";;

let rec string_of_depth_markers (depth_markers : depth_marker list) =
  match depth_markers with
  | [] -> ""
  | h :: t -> (string_of_depth_markers t) ^ (string_of_depth_marker h);;

(* let add_empty_line depth_markers line =
   let extra_line = (string_of_depth_markers (List.tl depth_markers)) ^
                   (string_of_depth_marker Down) in
   match List.hd depth_markers with
   | UpTurn -> line ^ "\n" ^ extra_line
   | _ -> extra_line ^ "\n" ^ line;; *)

let rec tree_string_of_field id (depth_markers : depth_marker list) : string =
  let head_mkr = List.hd depth_markers
  and tail_mkrs = List.tl depth_markers in
  let depth_string = string_of_depth_markers tail_mkrs
  and next_line_marker = if head_mkr = Branch || head_mkr = UpTurn then Down else None
  and prev_line_marker = if head_mkr = UpTurn then None else Down in
  let prefix = depth_string ^ (string_of_depth_marker head_mkr) in
  match id with
  | Field (x, f) ->
    (tree_string_of_expr (Ident x) (UpTurn :: prev_line_marker :: tail_mkrs))^ "\n" ^
    prefix ^ "(.)\n" ^
    (tree_string_of_expr (Ident (ref f)) (DownTurn :: next_line_marker :: tail_mkrs))
  | FieldExpr (id', f) ->
    (tree_string_of_field id' (UpTurn :: prev_line_marker :: tail_mkrs)) ^ "\n" ^
    prefix ^ "(.)\n" ^
    (tree_string_of_expr (Ident (ref f)) (DownTurn :: next_line_marker :: tail_mkrs))

and tree_string_of_bool_expr b (depth_markers : depth_marker list) =
  let head_mkr = List.hd depth_markers
  and tail_mkrs = List.tl depth_markers in
  let depth_string = string_of_depth_markers tail_mkrs
  and next_line_marker = if head_mkr = Branch || head_mkr = UpTurn then Down else None
  and prev_line_marker = if head_mkr = UpTurn then None else Down in
  let prefix = depth_string ^ (string_of_depth_marker head_mkr) in
  match b with
  | Bool b -> prefix ^ if b then "(true)" else "(false)"
  | ComparisonBinaryOperator (op, e1, e2) ->
    (tree_string_of_expr e1 (UpTurn :: prev_line_marker :: tail_mkrs)) ^ "\n" ^
    Printf.sprintf "%s(%s)\n" prefix (bool_op_to_string b "") ^
    (tree_string_of_expr e2 (DownTurn :: next_line_marker :: tail_mkrs))
  | BoolUnaryOperator (op, b1) ->
    Printf.sprintf "%s(%s)\n" prefix (bool_op_to_string b "") ^
    (tree_string_of_bool_expr b1 (DownTurn :: next_line_marker :: tail_mkrs))
  | BoolBinaryOperator (op, b1, b2) ->
    (tree_string_of_bool_expr b1 (UpTurn :: prev_line_marker :: tail_mkrs)) ^ "\n" ^
    Printf.sprintf "%s(%s)\n" prefix (bool_op_to_string b "") ^
    (tree_string_of_bool_expr b2 (DownTurn :: next_line_marker :: tail_mkrs))

and tree_string_of_expr e (depth_markers : depth_marker list) =
  let head_mkr = List.hd depth_markers
  and tail_mkrs = List.tl depth_markers in
  let depth_string = string_of_depth_markers tail_mkrs
  and next_line_marker = if head_mkr = Branch || head_mkr = UpTurn then Down else None
  and prev_line_marker = if head_mkr = UpTurn then None else Down in
  let prefix = depth_string ^ (string_of_depth_marker head_mkr) in
  match e with
  | Num n -> prefix ^ Printf.sprintf "(%d)" n
  | Null -> prefix ^ "(null)"
  | Ident x -> Printf.sprintf "%s(%s)" prefix !x
  | FieldAccess id -> tree_string_of_field id depth_markers
  | ArithmeticBinaryOperator (op, e1, e2) ->
    (tree_string_of_expr e1 (UpTurn :: prev_line_marker :: tail_mkrs)) ^ "\n" ^
    prefix ^ (Printf.sprintf "(%s)\n" (arithmetic_op_to_string e "")) ^
    (tree_string_of_expr e2 (DownTurn :: next_line_marker :: tail_mkrs))
  | ArithmeticUnaryOperator (op, e1) ->
    prefix ^ (Printf.sprintf "(%s)\n" (arithmetic_op_to_string e "")) ^
    (tree_string_of_expr e1 (DownTurn :: next_line_marker :: tail_mkrs))
  | Procedure (y, c) ->
    prefix ^ "(proc)\n" ^
    (tree_string_of_expr (Ident y) (Branch :: next_line_marker :: tail_mkrs)) ^ "\n" ^
    (tree_string_of_cmd c (DownTurn :: next_line_marker :: tail_mkrs))

and tree_string_of_cmd c (depth_markers : depth_marker list) =
  let head_mkr = List.hd depth_markers
  and tail_mkrs = List.tl depth_markers in
  let next_line_marker = if head_mkr = Branch || head_mkr = UpTurn then Down else None
  and prev_line_marker = if head_mkr = UpTurn then None else Down in
  let prefix = (string_of_depth_markers tail_mkrs) ^ (string_of_depth_marker head_mkr) in
  match c with
  | Declare x ->
    prefix ^ "(var)\n" ^
    (tree_string_of_expr (Ident x) (DownTurn :: next_line_marker :: tail_mkrs))
  | ProceduceCall (p, y) ->
    prefix ^ "(ProceduceCall)\n" ^
    (tree_string_of_expr p (Branch :: next_line_marker :: tail_mkrs)) ^ "\n" ^
    (tree_string_of_expr y (DownTurn :: next_line_marker :: tail_mkrs))
  | MallocVar   x  ->
    prefix ^ "(malloc)\n" ^
    (tree_string_of_expr (Ident x) (DownTurn :: next_line_marker :: tail_mkrs))
  | MallocField xf ->
    prefix ^ "(malloc)\n" ^
    (tree_string_of_field xf (DownTurn :: next_line_marker :: tail_mkrs))
  | Assign (x, e) ->
    (tree_string_of_expr (Ident x) (UpTurn :: prev_line_marker :: tail_mkrs)) ^ "\n" ^
    prefix ^ "(=)\n" ^
    (tree_string_of_expr e (DownTurn :: next_line_marker :: tail_mkrs))
  | FieldAssign (id, e) ->
    (tree_string_of_field id (UpTurn :: prev_line_marker :: tail_mkrs)) ^ "\n" ^
    prefix ^ "(=)\n" ^
    (tree_string_of_expr e (DownTurn :: next_line_marker :: tail_mkrs))
  | Skip -> prefix ^ "(skip)"
  | CmdSequence cs ->
    prefix ^ "(cmds)\n" ^
    (tree_string_of_cmds cs (DownTurn :: next_line_marker :: tail_mkrs))
  | While (b, c) ->
    prefix ^ "(while)\n" ^
    (tree_string_of_bool_expr b (Branch :: next_line_marker :: tail_mkrs)) ^ "\n" ^
    (tree_string_of_cmd c (DownTurn :: next_line_marker :: tail_mkrs))
  | IfElse (b, c1, c2) ->
    prefix ^ "(ifelse)\n" ^
    (tree_string_of_bool_expr b (Branch :: next_line_marker :: tail_mkrs)) ^ " \n" ^
    (tree_string_of_cmd c1 (Branch :: next_line_marker :: tail_mkrs)) ^ "\n" ^
    (tree_string_of_cmd c2 (DownTurn :: next_line_marker :: tail_mkrs))
  | Parallel (c1, c2) ->
    (tree_string_of_cmd c1 (UpTurn :: prev_line_marker :: tail_mkrs)) ^ "\n" ^
    prefix ^ "( ||| )\n" ^
    (tree_string_of_cmd c2 (DownTurn :: next_line_marker :: tail_mkrs))
  | Atom c ->
    prefix ^ "(atom)\n" ^
    (tree_string_of_cmd c (DownTurn :: next_line_marker :: tail_mkrs))

and tree_string_of_cmds cs (depth_markers : depth_marker list) =
  let next_depth_markers = if depth_markers = [] then [] else List.tl depth_markers in
  match cs with
  | [] -> ""
  | h :: t ->
    (tree_string_of_cmd h ((if t = [] then DownTurn else Branch) :: next_depth_markers)) ^
    if t = [] then "" else ("\n" ^ (tree_string_of_cmds t depth_markers));;
