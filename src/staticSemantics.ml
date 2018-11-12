(** Static semantics for MiniOOL
    @author Zachary Ferguson *)

open AbstractSyntaxTree;;
open ProgramString;;

(** Exception indicating a variable is out of scope. *)
exception VariableOutOfScope of string;;

(** Array of unicode subscript digits. *)
let subscript_digits = [|"₀"; "₁"; "₂"; "₃"; "₄"; "₅"; "₆"; "₇"; "₈"; "₉"|];;


(** Convert an integer into a string of subscript digits. *)
let rec subscript_string_of_int n =
  if n < 0 then "₋" ^ (subscript_string_of_int (-n))
  else if n < 10 then subscript_digits.(n)
  else (subscript_string_of_int (n / 10)) ^ (subscript_string_of_int (n mod 10));;


(** Record type of the count an ident has be declared and the current ident in
    scope. *)
type scope_rec = {mutable count: int; mutable current: int};;


(** Add a ident to the hash table of idents.
    @param scope Current variables declared and the number of time declared.
    @param id The identifier to add. *)
let add_id_to_scope scope id =
  match (Hashtbl.find_opt scope !id) with
  | None -> Hashtbl.add scope !id {count = 1; current = 0}
  | Some r -> (r.current <- r.count; r.count <- r.count + 1);;


(** Deep copy of the scope.
    @param scope Current variables declared and the number of time declared.
    @returns the new copy *)
let copy_scope scope = let scope' = Hashtbl.create (Hashtbl.length scope) in
  Hashtbl.iter
    (fun k v -> (Hashtbl.add scope' k {count = v.count; current = v.current}))
    scope;
  scope';;


(** Updates the idents in scope with the values from scope'.
    The keys of scope' should be a superset of the keys of scope.
    @param scope Current variables declared and the number of time declared.*)
let update_scope scope scope' = (
  Hashtbl.iter (fun k' v' ->
      match (Hashtbl.find_opt scope k') with
      (* Set the current to -1 implying k' is not in scope. *)
      | None -> Hashtbl.add scope k' {count = v'.count; current = -1}
      (* Increase the count, but the current version remains the same *)
      | Some v -> v.count <- v'.count)
    scope');;


(** uniquify the id by adding the current version number as a subscript.
    @param scope Current variables declared and the number of time declared.
    @param id The identifier to uniquify.
    @return a new unique identifier for the given identifier. *)
let get_current_unique_ident scope id =
  !id ^ (subscript_string_of_int (Hashtbl.find scope !id).current);;


(** Get the identifier of a field (i.e. x.f. ... .f => x).
    @param xf field to get the identifier.
    @return the identifier of the field access. *)
let rec get_ident_of_field xf = match xf with
  | Field (x, f) -> x
  | FieldExpr (xf, f) -> (get_ident_of_field xf);;


(** Add the id to the scope of scope and uniquify the id.
    @param scope Current variables declared and the number of time declared.
    @param id An ident to add to the scope hash table. *)
let declare_ident_in_scope scope id =
  add_id_to_scope scope id; (* Add the id to the scope *)
  id := get_current_unique_ident scope id;; (* uniquify the id *)


(** Is the given ident in the current scope
    @param scope Current variables declared and the number of time declared.
    @param id Ident to check the scope of.
    @return a boolean for if the id is in the scope of scope. *)
let is_id_in_scope scope id =
  match Hashtbl.find_opt scope !id with
  | None -> false
  (* id is in scope if the current version is not negative. *)
  | Some r -> r.current >= 0;;


(** Checks if id is in the scope of scope.
    If not then fail else update id to be the current version.
    @param scope Current variables declared and the number of time declared.
    @param id Ident to check the scope of.
    @raise VariableOutOfScope The identifier is not in the scope given. *)
let check_ident_in_scope scope id =
  if is_id_in_scope scope id then id := get_current_unique_ident scope id
  else raise (VariableOutOfScope ("Variable " ^ !id ^ " is not defined"));;


(** Checks if the field is in the scope of scope.
    If not then fail else update id to be the current version.
    @param scope Current variables declared and the number of time declared.
    @param xf Field to check the scope of.
    @raise VariableOutOfScope The identifier is not scope. *)
let check_field_in_scope scope xf =
  let x = (get_ident_of_field xf) in (check_ident_in_scope scope x);;


(** Checks if the expression e is in the scope of scope.
    If not then fail else update and relevant idents to be the current
    version.
    @param scope Current variables declared and the number of time declared.
    @param e Expression to check the scope of.
    @raise VariableOutOfScope An identifier is not in scope. *)
let rec check_expr_in_scope scope e =
  match e with
  | ArithmeticBinaryOperator (op, e1, e2) ->
    (check_expr_in_scope scope e1; check_expr_in_scope scope e2)
  | ArithmeticUnaryOperator (op, e1) -> check_expr_in_scope scope e1
  | FieldAccess xf -> check_field_in_scope scope xf
  | Procedure (id, c) ->
    let proc_scope = copy_scope scope in
    declare_ident_in_scope proc_scope id;
    check_cmd_in_scope proc_scope c;
    update_scope scope proc_scope
  | Ident id -> check_ident_in_scope scope id
  | _ -> ()


(** Checks if the boolean expression b is in the scope of scope.
    If not then fail else update and relevant idents to be the current
    version.
    @param scope Current variables declared and the number of time declared.
    @param b Boolean expression to check the scope of.
    @raise VariableOutOfScope An identifier is not in scope. *)
and check_bool_expr_in_scope scope b =
  match b with
  | ComparisonBinaryOperator (op, e1, e2) ->
    ((check_expr_in_scope scope e1); (check_expr_in_scope scope e2))
  | BoolUnaryOperator (op, b1) ->
    (check_bool_expr_in_scope scope b1)
  | BoolBinaryOperator (op, b1, b2) ->
    ((check_bool_expr_in_scope scope b1); (check_bool_expr_in_scope scope b2))
  | _ -> ()

(** Checks if the command c is in the scope of scope.
    If not then fail else update and relevant idents to be the current
    version.
    @param scope Current variables declared and the number of time declared.
    @param c Command to check the scope of.
    @raise VariableOutOfScope An identifier is not in scope. *)
and check_cmd_in_scope scope c =
  try
    match c with
    | Declare id -> declare_ident_in_scope scope id
    | ProceduceCall (e1, e2) ->
      check_expr_in_scope scope e1; check_expr_in_scope scope e2
    | MallocVar id -> check_ident_in_scope scope id
    | MallocField xf -> check_field_in_scope scope xf
    | Assign (id, e) ->
      check_ident_in_scope scope id;
      check_expr_in_scope scope e
    | FieldAssign (xf, e) ->
      check_field_in_scope scope xf;
      check_expr_in_scope scope e
    | CmdSequence cs -> check_cmds_in_scope scope cs
    | While (b, c1) -> let while_scope = copy_scope scope in
      check_bool_expr_in_scope while_scope b;
      check_cmd_in_scope while_scope c1;
      update_scope scope while_scope
    | IfElse (b, c1, c2) ->
      check_bool_expr_in_scope scope b;
      (let if_scope = copy_scope scope in
       check_cmd_in_scope if_scope c1;
       update_scope scope if_scope);
      (let else_scope = copy_scope scope in
       check_cmd_in_scope else_scope c2;
       update_scope scope else_scope)
    | Parallel (c1, c2) ->
      (let c1_scope = copy_scope scope in
       check_cmd_in_scope c1_scope c1;
       update_scope scope c1_scope);
      (let c2_scope = copy_scope scope in
       check_cmd_in_scope c2_scope c2;
       update_scope scope c2_scope)
    | Atom c1 -> check_cmd_in_scope scope c1
    | _ -> ()
  with VariableOutOfScope msg ->
    raise (VariableOutOfScope (msg ^ " in " ^ (cmd_to_string c)))

(** Checks if the commands ast are in the scope of scope.
    If not then fail else update and relevant idents to be the current
    version.
    @param scope Current variables declared and the number of time declared.
    @param ast Commands to check the scope of.
    @raise VariableOutOfScope An identifier is not in scope. *)
and check_cmds_in_scope scope ast =
  match ast with
  | [] -> ()
  | h :: t -> check_cmd_in_scope scope h; check_cmds_in_scope scope t;;

(** Print the entrys of the scope Hashtbl. *)
let rec print_scope scope = Hashtbl.iter (
    fun k v ->
      Printf.printf "%s: {count = %d; current = %d}\n" k v.count v.current)
    scope;;
