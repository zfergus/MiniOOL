(** Evaluate the abstract syntax tree.
    @author Zachary Ferguson*)
open AbstractSyntaxTree
open ProgramString
open SemanticDomains


(** Get a string representation of the type of tainted_value v.
    @param v Tainted value to get the type string of.
    @return a string representation of the tainted value type. *)
let type_string_of_tainted_value v plural = match v with
  | Field _ -> "field" ^ (if plural then "s" else "")
  | Int _ -> "integer" ^ (if plural then "s" else "")
  | Object _ -> "object" ^ (if plural then "s" else "")
  | Null -> "null"
  | Closure _ -> "procedure" ^ (if plural then "s" else "")
  | Error _ -> "error" ^ (if plural then "s" else "");;

(** Location of the global variables in the heap. *)
let global_loc = -1;;
(** Next free location available in the heap. *)
let next_loc = ref 0;;

(** Get the next free location available in the heap and update the the
    next_loc value.
    @return the next location available on the heap. *)
let get_next_loc () =
  (* Global values are stored at location -1 in the heap. *)
  if !next_loc == global_loc then failwith
      "Out of memory (no free location available on the heap)"
  else next_loc := !next_loc + 1; !next_loc - 1;;


(** Get the value of an identifier. Implicitly get the value as a global
    variable if necessary.
    @param x Identifier to get the value of.
    @param s Stack of a mapping from identifier to locations in the heap for
    the identifier's value.
    @param h Heap of values for identifiers.
    @return the value of the identifier (h[s[!x], "val"] -> tainted value). *)
let value_of_variable x s h =
  match Hashtbl.find_opt s !x with
  (** If the x is a variable on the stack, return its value. *)
  | Some loc -> Hashtbl.find h (loc, "val")
  (** If the x is not a variable on the stack, it is an implicit global
      variable, so initialize the x if necessary and return its value. *)
  | None -> match Hashtbl.find_opt h (global_loc, !x) with
    (* Globals have been initialized. *)
    | Some tval -> tval
    (* Globals have not been initialized. *)
    | None -> Hashtbl.replace h (global_loc, !x) Null; Null;;


(** Updates the value of an identifier. Implicitly the identifier as a global
    variable if necessary.
    @param x Identifier to get the value of.
    @param value' New value to assign to the identifier.
    @param s Stack of a mapping from identifier to locations in the heap for
    the identifier's value.
    @param h Heap of values for identifiers.
    @return unit. *)
let update_value_of_ident x value' s h =
  match Hashtbl.find_opt s !x with
  (** If the x is a variable on the stack, update its value. *)
  | Some loc -> Hashtbl.replace h (loc, "val") value';
    (** If the x is not a variable on the stack, it is an implicit global
        variable, so initialize the x if necessary and update its value. *)
  | None -> Hashtbl.replace h (global_loc, !x) value';;


let gen_field_access_error_message e1 e2 v1 v2 =
  let e1_str = string_of_expr e1 and e2_str = string_of_expr e2 in
  match v1, v2 with
  | Null, _ -> Printf.sprintf "Expression %s has a value of null in %s.%s"
                 e1_str e1_str e2_str
  | Object obj, _ ->
    Printf.sprintf "Expression %s does not have a value of a field in %s.%s"
      e2_str e1_str e2_str
  | Error msg1, Error msg2 -> msg1 ^ " and " ^ msg2
  | Error msg, _ | _, Error msg -> msg
  | _, Field f ->  Printf.sprintf "Expression %s is not an object in %s.%s"
                     e1_str e1_str e2_str
  | _, _ ->
    Printf.sprintf
      "Expression %s is not an object and expression %s is not a field in %s.%s"
      e1_str e2_str e1_str e2_str;;


(** Evaluate the value of an expression.
    @param e Expression to evaluate.
    @param s Stack of a mapping from identifier to locations in the heap for
    the identifier's value.
    @param h Heap of values for identifiers.
    @return the tainted_value associated to the expression. *)
let rec eval_expr e s h = match e with
  (* Number to number. *)
  | AbstractSyntaxTree.Field f -> SemanticDomains.Field f
  | Num n -> Int n
  (* Perform operation on evaluated values. *)
  | BinaryArithmeticOperator (op, e1, e2) -> (
      let v1 = eval_expr e1 s h and v2 = eval_expr e2 s h in
      match v1, v2 with
      | Int i1, Int i2 -> (try Int (op i1 i2) with Division_by_zero ->
          Error ("Division by zero in " ^ (string_of_expr e)))
      | _, _ -> Error (
          Printf.sprintf
            "Operation (%s) not defined between %s and %s in %s"
            (string_of_arithmetic_op e) (type_string_of_tainted_value v1 true)
            (type_string_of_tainted_value v2 true) (string_of_expr e)))
  (* Perform operation on evaluated values. *)
  | UnaryArithmeticOperator (op, e1) -> (
      let v1 = eval_expr e1 s h in match v1 with
      | Int i1 -> Int (op i1)
      | _ -> Error (
          Printf.sprintf "Operation (%s) not defined for %s in %s"
            (string_of_arithmetic_op e) (type_string_of_tainted_value v1 true)
            (string_of_expr e)))
  (* Null -> Null *)
  | AbstractSyntaxTree.Null -> SemanticDomains.Null
  (* Evaluate a field access. *)
  | FieldAccess (e1, e2) ->
    let v1 = eval_expr e1 s h and v2 = eval_expr e2 s h in (match v1, v2 with
      | Object obj, Field f -> (match Hashtbl.find_opt h (obj, f) with
          | Some f_val -> f_val
          | None -> Null)
      | _, _ -> Error (gen_field_access_error_message e1 e2 v1 v2))
  (* Create a closure of the current stack. *)
  | Procedure (y, c) ->
    Closure (ref {param = y; body = c; call_stack = (Hashtbl.copy s)})
  (* Get the value of the identifier. *)
  | Variable x -> value_of_variable x s h


(** Evaluate the value of a boolean expression.
    @param b Boolean expression to evaluate.
    @param s Stack of a mapping from identifier to locations in the heap for
    the identifier's value.
    @param h Heap of values for identifiers.
    @return the boolean value associated to the expression. *)
and eval_bool_expr b s h = match b with
  (* Literal boolean values *)
  | Bool v -> v
  (* Perform operation on evaluated values. *)
  | BinaryComparisonOperator (op, e1, e2) ->
    let v1 = eval_expr e1 s h and v2 = eval_expr e2 s h in
    let fail_str =  Printf.sprintf
        "Operation (%s) not defined between %s and %s in %s"
        (string_of_bool_op b) (type_string_of_tainted_value v1 true)
        (type_string_of_tainted_value v2 true) (string_of_bool_expr b) in
    (let op_str = (string_of_bool_op b) in match op_str with
      (* ==/!= are defined on integers, locations, and closures only *)
      | "==" | "!=" -> (match v1, v2 with
          | Int i1, Int i2 -> (op i1 i2)
          | Object obj1, Object obj2 -> (op obj1 obj2)
          | Object _, Null | Null, Object _ -> (op 0 1)
          | Null, Null -> (op 0 0)
          | Closure c1, Closure c2 ->
            (* Compare the reference to closures. *)
            (if op_str = "==" then (==) else (!=)) c1  c2
          | _, _ -> failwith fail_str)
      (* All other comparison operators are defined only on integers. *)
      | _ -> (match v1, v2 with
          | Int i1, Int i2 -> (op i1 i2)
          | _, _ -> failwith (fail_str)))
  (* Perform operation on evaluated values. *)
  | UnaryLogicOperator (op, b1) ->
    let v1 = eval_bool_expr b1 s h in (op v1)
  (* Perform operation on evaluated values. *)
  | BinaryLogicOperator (op, b1, b2) ->
    let v1 = eval_bool_expr b1 s h and v2 = eval_bool_expr b2 s h in (op v1 v2)


(** Evaluate the value of a command.
    @param c Command to evaluate.
    @param s Stack of a mapping from identifier to locations in the heap for
    the identifier's value.
    @param h Heap of values for identifiers.
    @return a list of additional commands to perform next. *)
and eval_cmd c s h = match c with
  (* Declare a variable: add the variable to the stack and settings the heap
     location to null. *)
  | Declare x -> let loc = get_next_loc () in
    Hashtbl.add s !x loc; Hashtbl.add h (loc, "val") Null; []
  (* Recursive procedure call: add the parameter to the stack of the closure
     then evaluate the body of the procedure in the context of the closure's
     modified stack. *)
  | ProceduceCall (e1, e2) ->
    let v1 = eval_expr e1 s h and v2 = eval_expr e2 s h in
    (match v1 with
     | Closure clo ->
       let call_stack = (Hashtbl.copy !clo.call_stack)
       and loc = get_next_loc () in
       Hashtbl.add call_stack !(!clo.param) loc; Hashtbl.add h (loc, "val") v2;
       eval_cmds [!clo.body] call_stack h; []
     | _ -> failwith (Printf.sprintf "Expression %s is not a procedure in %s"
                        (string_of_expr e1) (string_of_cmd c)))
  (* Allocate a variable on the heap as an object. *)
  | Malloc x -> let obj = Object (get_next_loc ()) in
    update_value_of_ident x obj s h; []
  (* Assign a value to a variable. *)
  | Assign (x, e) -> (let v = eval_expr e s h in match v with
    | Error msg -> failwith msg
    | _ -> update_value_of_ident x v s h; [])
  (* Assign a value to a field of a variable allocated on the heap. *)
  | FieldAssign (e1, e2, e3) ->
    let v1 = eval_expr e1 s h and v2 = eval_expr e2 s h in (match v1, v2 with
      | Object obj, Field f -> Hashtbl.replace h (obj, f) (eval_expr e3 s h); []
      | _, _ -> failwith (gen_field_access_error_message e1 e2 v1 v2))
  | Skip -> []
  (* Evaluate the first command and return the resulting commands, if any,
     prepended to the rest of the command sequence. *)
  | CmdSequence cs ->
    (match cs with
     | [] -> []
     | head :: tail -> eval_cmd head s h @ tail)
  (* Evaluate the conditional, b.
     If b then return [c1] else return [c2] to evaluate next. *)
  | IfElse (b, c1, c2) ->
    if eval_bool_expr b s h then [c1] else [c2]
  (* Evaluate the conditional, b.
     If b then return [c1; while b c1] else return [] to evaluate next. *)
  | While (b, c1) ->
    if eval_bool_expr b s h then [c1; c] else []
  (* Randomly choose one operand command to evaluate. *)
  | Parallel (c1, c2) ->
    if Random.bool() then
      (match eval_cmd c1 s h with
       | [] -> [c2]
       | cs -> [Parallel (CmdSequence cs, c2)])
    else
      (match eval_cmd c2 s h with
       | [] -> [c1]
       | cs -> [Parallel (c1, CmdSequence cs)])
  (* Exceute the entire body of the Atom before continuing on. *)
  | Atom c1 -> eval_cmds [c1] s h; []


(** Evaluate the value of a list of commands.
    @param cs Commands to evaluate.
    @param s Stack of a mapping from identifier to locations in the heap for
    the identifier's value.
    @param h Heap of values for identifiers.
    @return unit. *)
and eval_cmds cs s h = match cs with
  | [] -> ()
  (* Prepend the evaluated results of the head command and continue evaluating
     recursivly. *)
  | head :: tail -> eval_cmds ((eval_cmd head s h) @ tail) s h;;


(** Create a string representation of a tainted value.
    @param v Tainted value to stringify.
    @param s Stack of a mapping from identifier to locations in the heap for
    the identifier's value.
    @param h Heap of values for identifiers.
    @return a string representation of the tainted value. *)
let rec string_of_tainted_value v s h obj_names =
  match v with
  | Field f -> f
  | Int i -> string_of_int i
  | Object obj -> let buff = Buffer.create 10 in
    Buffer.add_string buff "(object: {";
    let first_el = ref true in
    Hashtbl.iter
      (fun k v -> if (fst k) = obj then Buffer.add_string buff
            (Printf.sprintf "%s%s: %s"
               (if !first_el then (first_el := false; "") else ", ")
               (snd k) (match Hashtbl.find_opt obj_names v with
                   | Some id -> id
                   | None -> string_of_tainted_value v s h obj_names))) h;
    Buffer.add_string buff "})";
    Buffer.contents buff
  | Null -> "null"
  | Closure clo ->
    (* print_vals (diff_stacks s' s) h;  *)
    string_of_expr (Procedure (!clo.param, !clo.body))
  | Error msg -> Printf.sprintf "(error: %s)" msg


(** Create a string representation of the values in the stack and globals in
    the heap.
    @param s Stack of a mapping from identifier to locations in the heap for
    the identifier's value.
    @param h Heap of values for identifiers.
    @return a string representation of all the global values. *)
and string_of_vals s (h : heap) =
  let buff = Buffer.create ((Hashtbl.length s) * 10) in
  (* Print values of variable in the stack. *)
  Hashtbl.iter (
    fun k v -> let obj_names = Hashtbl.create 10 in
      Hashtbl.replace obj_names (Hashtbl.find h (v, "val")) k;
      (try Buffer.add_string buff (
           Printf.sprintf "%s: %s\n" k
             (string_of_tainted_value (Hashtbl.find h (v, "val")) s h
                obj_names))
       with Stack_overflow ->
         Utils.print_warning "Cannot display the value of objects with recursive references";
         Buffer.add_string buff (
           Printf.sprintf "%s: %s\n" k (type_string_of_tainted_value
                                          (Hashtbl.find h (v, "val")) false))))
    s;
  (* Print values of implicit global variables in the heap. *)
  Hashtbl.iter
    (fun k v -> let obj_names = Hashtbl.create 10 in
      if fst k = global_loc then try Buffer.add_string buff (
          Printf.sprintf "%s: %s\n" (snd k)
            (string_of_tainted_value v s h obj_names))
        with Stack_overflow ->
          Utils.print_warning "Cannot display the value of objects with recursive references";
          Buffer.add_string buff (
            Printf.sprintf "%s: %s\n" (snd k)
              (type_string_of_tainted_value v false))) h;
  Buffer.contents buff;;
