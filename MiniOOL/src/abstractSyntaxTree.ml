(*
Abstract syntax tree type for MiniOOL
Writen by Zachary Ferguson
*)

(*
Identifiers:
Reference to a string so the ident can be uniqified by the static semantics.
*)
type ident = string ref;;

(*
Fields:
Either an ident with a string or a series of field string pairs.
*)
type field = Field of ident * string | FieldExpr of field * string

(* Expressions *)
and expr =
    Num of int (* Number *)
  (* Integer binary operations: +, -, *, /, % *)
  | ArithmeticBinaryOperator of (int -> int -> int) * expr * expr
  (* Integer unary operations: - *)
  | ArithmeticUnaryOperator of (int -> int) * expr
  | Null (* Null location *)
  (* Accessing a field: e.e *)
  | FieldAccess of field
  (* Procedure declaration: only one parameter, no return value *)
  | Procedure of ident * cmd
  (* Identifier: variable name *)
  | Ident of ident

(* Boolean Expressions *)
and bool_expr =
    Bool of bool (* Boolean literals: true or false *)
  (* Comparison of integer operations: ==, != (<>), <, <=, >, >= *)
  | ComparisonBinaryOperator of (int -> int -> bool) * expr * expr
  (* Boolean unary operations: ! (not) *)
  | BoolUnaryOperator of (bool -> bool) * bool_expr
  (* Boolean binary operations: && (and), || (or) *)
  | BoolBinaryOperator of (bool -> bool -> bool) * bool_expr * bool_expr

(* Commands *)
and cmd =
    Declare of ident (* Declare an identifier: var *)
  | ProceduceCall of expr * expr (* Procedure call: (e)(e) *)
  | MallocVar of ident (* Allocate a simple identifier on the heap *)
  | MallocField of field (* Allocate a field of an identifier to allow x.f.f *)
  | Assign of ident * expr (* Varable assignment: ident = e *)
  | FieldAssign of field * expr (* Field assignment: ident.field = e *)
  | Skip (* No operation *)
  | CmdSequence of cmds (* Sequence of commands *)
  | While of bool_expr * cmd (* While statement: while b C *)
  | IfElse of bool_expr * cmd * cmd (* IfElse statement: if b then C else *)
  (* Parrallel commands: {c1 ||| c2} do c1 and c2 in parallel (random order) *)
  | Parallel of cmd * cmd
  (* Atomic command: no other command can run until this command finishes *)
  | Atom of cmd

(* Sequence of Commands *)
and cmds = cmd list

(* Program = Sequence of Commands *)
and prog = cmds;;
