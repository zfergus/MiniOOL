(** Evaluate the abstract syntax tree.
    @author Zachary Ferguson*)
open AbstractSyntaxTree
open ProgramString
open SemanticDomains

(*  *)
let name_string_of_tainted_value v = match v with
  | Int _ -> "integers"
  | Object _ -> "objects"
  | Null -> "null"
  | Closure _ -> "procedures"
  | Error _ -> "errors"

let next_loc = ref 0;;
let get_next_loc () = next_loc := !next_loc + 1; !next_loc - 1;;


let location_of_ident id s h =
  match Hashtbl.find_opt s !id with
  | Some loc -> loc
  | None -> let loc = get_next_loc () in
    Hashtbl.add s !id loc; Hashtbl.add h loc Null; loc;;


let value_of_ident id s h = Hashtbl.find h (location_of_ident id s h);;


let rec eval_field (xf : field) (s : stack) (h : heap) : tainted_value =
  match xf with
  (* xf is a terminal field x.f *)
  | TerminalField (x, f) -> (let x_val = eval_expr (Ident x) s h in match x_val with
    (* only allocated objects can have fields *)
    | Object obj -> (match Hashtbl.find_opt obj f with
        (* check if the field has been initialized before *)
        | Some f_val -> f_val
        (* all unused fields are implicitly null *)
        | None -> Null)
    (* null locations do not have fields *)
    | Null -> Error (Printf.sprintf "Variable %s has a value of null in %s"
                       !x (string_of_field xf))
    (* all other values of x in x.f produce an error *)
    | Error msg -> Error msg
    | _ -> Error (Printf.sprintf "Variable %s is not allocated heap in %s"
                    !x (string_of_field xf)))
  (* xf is a recursive field x. ... .g.f *)
  | RecursiveField (xg, f) -> (let xg_val = eval_field xg s h in match xg_val with
    | Object obj -> (match Hashtbl.find_opt obj f with
        | Some f_val -> f_val
        | None -> Null)
    | Null -> Error (Printf.sprintf "Field %s has a value of null in %s"
                       (string_of_field xg) (string_of_field xf))
    | Error msg -> Error msg
    | _ -> Error (Printf.sprintf "Field %s is not allocated on the heap in %s"
                    (string_of_field xg) (string_of_field xf)))


and eval_expr (e : expr) (s : stack) (h : heap) : tainted_value =
  match e with
  | Num n -> Int n
  | BinaryArithmeticOperator (op, e1, e2) -> (
      let v1 = eval_expr e1 s h and v2 = eval_expr e2 s h in
      match v1 with
      | Int i1 -> (
          match v2 with
          | Int i2 -> (try Int (op i1 i2) with Division_by_zero ->
              Error ("Division by zero in " ^ (string_of_expr e)))
          | _ -> Error (
              Printf.sprintf
                "Operation (%s) not defined between %s and %s in %s"
                (string_of_arithmetic_op e) (name_string_of_tainted_value v1)
                (name_string_of_tainted_value v2) (string_of_expr e)))
      | _ -> Error (
          Printf.sprintf
            "Operation (%s) not defined between %s and %s in %s"
            (string_of_arithmetic_op e) (name_string_of_tainted_value v1)
            (name_string_of_tainted_value v2) (string_of_expr e)))
  | UnaryArithmeticOperator (op, e1) -> (
      let v1 = eval_expr e1 s h in match v1 with
      | Int i1 -> Int (op i1)
      | _ -> Error (
          Printf.sprintf "Operation (%s) not defined for %s in %s"
            (string_of_arithmetic_op e) (name_string_of_tainted_value v1)
            (string_of_expr e)))
  | AbstractSyntaxTree.Null -> SemanticDomains.Null
  | FieldAccess xf -> eval_field xf s h
  | Procedure (y, c) -> Closure (y, c, (Hashtbl.copy s))
  | Ident id -> value_of_ident id s h


and eval_bool_expr (b : bool_expr) (s : stack) (h : heap) : bool =
  match b with
  | Bool v -> v
  | BinaryComparisonOperator (op, e1, e2) ->
    let v1 = eval_expr e1 s h and v2 = eval_expr e2 s h in
    let fail_str =  Printf.sprintf
        "Operation (%s) not defined between %s and %s in %s"
        (string_of_bool_op b) (name_string_of_tainted_value v1)
        (name_string_of_tainted_value v2) (string_of_bool_expr b) in
    (match (string_of_bool_op b) with
     | "!=" | "==" ->
       (match v1 with
        | Int i1 ->
          (match v2 with
           |Int i2 -> (op i1 i2)
           | _ -> failwith fail_str)
        | Object obj1 ->
          (match v2 with
           | Object obj2 -> failwith fail_str (* TODO: implement this *)
           | Null -> false
           | _ -> failwith fail_str)
        | Null ->
          (match v2 with
           | Null -> true
           | Object _ -> false
           | _ -> failwith fail_str)
        | Closure (id1, c1, s1) ->
          (match v2 with
           | Closure (id2, c2, s2) ->
             failwith fail_str (* TODO: implement this *)
           | _ -> failwith fail_str)
        | _ -> failwith fail_str)
     (** all other comparison operators are defined only on integers *)
     | _ ->
       (match v1 with
        | Int i1 ->
          (match v2 with
           | Int i2 -> (op i1 i2)
           | _ -> failwith fail_str)
        | _ -> failwith (fail_str)))
  | UnaryLogicOperator (op, b1) ->
    let v1 = eval_bool_expr b1 s h in (op v1)
  | BinaryLogicOperator (op, b1, b2) ->
    let v1 = eval_bool_expr b1 s h and v2 = eval_bool_expr b2 s h in (op v1 v2)


and eval_cmd (c : cmd) (s : stack) (h : heap) : cmds =
  match c with
  (* Declare a variable: add the variable to the stack and settings the heap
     location to null. *)
  | Declare id -> let loc = get_next_loc () in
    Hashtbl.add s !id loc; Hashtbl.add h loc Null; []
  (* Recursive procedure call: add the parameter to the stack of the closure
     then evaluate the body of the procedure in the context of the closure's
     modified stack. *)
  | ProceduceCall (e1, e2) ->
    let v1 = eval_expr e1 s h and v2 = eval_expr e2 s h in
    (match v1 with
     | Closure (id, c', s') ->
       let loc = get_next_loc () in
       Hashtbl.add s' !id loc; Hashtbl.add h loc v2;
       eval_cmds [c'] s' h;
       Hashtbl.remove s' !id; []
     | _ -> failwith (Printf.sprintf "Expression %s is not a procedure in %s"
                        (string_of_expr e1) (string_of_cmd c)))
  (* Allocate a variable on the heap as an object. *)
  | MallocVar id ->
    Hashtbl.replace h (location_of_ident id s h) (Object (Hashtbl.create 10)); []
  (* Allocate a field of a variable on the heap as an object. *)
  | MallocField xf ->
    (match xf with
     | TerminalField (x, f) ->
       (match eval_expr (Ident x) s h with
        | Object obj ->
          Hashtbl.replace obj f (Object (Hashtbl.create (Hashtbl.length obj)))
        | _ -> failwith (Printf.sprintf "Variable %s is not an object" !x))
     | RecursiveField (xg, f) ->
       (match eval_field xg s h with
        | Object obj ->
          Hashtbl.replace obj f (Object (Hashtbl.create (Hashtbl.length obj)))
        | _ -> failwith (Printf.sprintf "Field %s is not an object"
                           (string_of_field xg)))); []
  (* Assign a value to a variable. *)
  | Assign (id, e) -> (let v = eval_expr e s h in match v with
    | Error msg -> failwith msg
    | _ -> Hashtbl.replace h (location_of_ident id s h) v); []
  (* Assign a value to a field of a variable allocated on the heap. *)
  | FieldAssign (xf, e) -> let v = eval_expr e s h in (match xf with
      | TerminalField (x, f) -> let x_val = eval_expr (Ident x) s h in (
          match x_val with
          | Object obj -> Hashtbl.replace obj f v
          | Error msg -> failwith msg
          | _ -> failwith (!x ^ " is not an object allocated on the heap"))
      | RecursiveField (xg, f) -> let xg_val = eval_field xg s h in (
          match xg_val with
          | Object obj -> Hashtbl.replace obj f v
          | Error msg -> failwith msg
          | _ -> failwith ((string_of_field xg) ^
                           " is not an object allocated on the heap"))); []
  | Skip -> []
  | CmdSequence cs ->
    (match cs with
     | [] -> []
     | head :: tail -> eval_cmd head s h @ tail)
  | IfElse (b, c1, c2) ->
    if eval_bool_expr b s h then [c1] else [c2]
  | While (b, c1) ->
    if eval_bool_expr b s h then [c1; c] else []
  | Parallel (c1, c2) ->
    if Random.bool() then
      (match eval_cmd c1 s h with
       | [] -> [c2]
       | cs -> [Parallel (CmdSequence cs, c2)])
    else
      (match eval_cmd c2 s h with
       | [] -> [c1]
       | cs -> [Parallel (c2, CmdSequence cs)])
  | Atom c1 -> eval_cmds [c1] s h; []


and eval_cmds (cs : cmds) (s : stack) (h : heap) : unit=
  match cs with
  | [] -> ()
  | head :: tail -> eval_cmds ((eval_cmd head s h) @ tail) s h;;



let diff_stacks s1 s2 = let s_diff = Hashtbl.create (Hashtbl.length s1) in
  Hashtbl.iter
    (fun k v -> if Hashtbl.mem s2 k then () else Hashtbl.add s_diff k v) s1;
  s_diff;;

let print_stack s = Hashtbl.iter
    (fun k v -> Printf.printf "%s: %d\n" k v) s;;

let rec string_of_tainted_value v s h = match v with
  | Int i -> string_of_int i
  | Object obj -> let buff = Buffer.create (Hashtbl.length obj) in
    Buffer.add_string buff "(object: {";
    let first_el = ref true in
    Hashtbl.iter (fun k v -> Buffer.add_string buff (
        Printf.sprintf (if !first_el then "%s: %s" else ", %s: %s") k
          (string_of_tainted_value v s h)); first_el := false) obj;
    Buffer.add_string buff "})";
    Buffer.contents buff
  | Null -> "null"
  | Closure (y, c, s') ->
    (* print_vals (diff_stacks s' s) h;  *)
    string_of_expr (Procedure (y, c))
  | Error msg -> Printf.sprintf "(error: %s)" msg

and string_of_vals (s : stack) (h : heap) =
  let buff = Buffer.create ((Hashtbl.length s) * 10) in
  Hashtbl.iter (fun k v ->
      Buffer.add_string buff
        (Printf.sprintf "%s: %s\n" k
           (string_of_tainted_value (Hashtbl.find h v) s h))) s;
  Buffer.contents buff;;
