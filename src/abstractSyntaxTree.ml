(** Abstract syntax tree type for MiniOOL.
    @author Zachary Ferguson *)

(** Identifiers: Reference to a string so the ident can be uniqified by the
    static semantics. *)
type ident = string ref;;

(** Expressions *)
type expr =
  | Field of string (** Field *)
  | Num of int (** Number *)
  | BinaryArithmeticOperator of (int -> int -> int) * expr * expr
  (** Integer binary operations: +, -, *, /, % *)
  | UnaryArithmeticOperator of (int -> int) * expr
  (** Integer unary operations: - *)
  | Null (** Null location *)
  | Variable of ident (** Identifier: variable name *)
  | FieldAccess of expr * expr (** Accessing a field: e.e *)
  | Procedure of ident * cmd
  (** Procedure declaration: only one parameter, no return value *)

(** Boolean Expressions *)
and bool_expr =
  | Bool of bool (** Boolean literals: true or false *)
  | BinaryComparisonOperator of (int -> int -> bool) * expr * expr
  (** Comparison of integer operations: ==, != (<>), <, <=, >, >= *)
  | UnaryLogicOperator of (bool -> bool) * bool_expr
  (** Boolean unary operations: ! (not) *)
  | BinaryLogicOperator of (bool -> bool -> bool) * bool_expr * bool_expr
  (** Boolean binary operations: && (and), || (or) *)

(** Commands *)
and cmd =
  | Declare of ident (** Declare an identifier: var *)
  | ProceduceCall of expr * expr (** Procedure call: (e)(e) *)
  | Malloc of ident (** Allocate a simple identifier on the heap *)
  (* | MallocVar of ident (** Allocate a simple identifier on the heap *) *)
  (* | MallocField of field (** Allocate a field of an identifier to allow x.f.f *) *)
  | Assign of ident * expr (** Varable assignment: ident = e *)
  | FieldAssign of expr * expr * expr (** Field assignment: e.e = e *)
  | Skip (** No operation *)
  | CmdSequence of cmds (** Sequence of commands *)
  | While of bool_expr * cmd (** While statement: while b C *)
  | IfElse of bool_expr * cmd * cmd (** IfElse statement: if b then C else *)
  | Parallel of cmd * cmd
  (** Parrallel commands: {c1 ||| c2} do c1 and c2 in parallel (random order) *)
  | Atom of cmd
  (** Atomic command: no other command can run until this command finishes *)

(** Sequence of Commands *)
and cmds = cmd list

(** Program = Sequence of Commands *)
and prog = cmds;;
