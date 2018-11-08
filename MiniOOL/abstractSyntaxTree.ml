(* Types used in MiniOOL *)

type ident = string ref;;
type idents = ident list;;
type field = Field of ident * string | FieldExpr of field * string

type expr =
    Num of int | ArithmeticBinaryOperator of (int -> int -> int) * expr * expr |
    ArithmeticUnaryOperator of (int -> int) * expr | Null |
    FieldAccess of field | Procedure of ident * cmd | Ident of ident
and bool_expr =
    Bool of bool |
    ComparisonBinaryOperator of (int -> int -> bool) * expr * expr |
    BoolUnaryOperator of (bool -> bool) * bool_expr |
    BoolBinaryOperator of (bool -> bool -> bool) * bool_expr * bool_expr
and cmd =
    Declare of ident | ProceduceCall of expr * expr |
    MallocVar of ident | MallocField of field |
    Assign of ident * expr | FieldAssign of field * expr | Skip |
    Cmds of cmds | While of bool_expr * cmd | IfElse of bool_expr * cmd * cmd |
    Parallel of cmd * cmd | Atom of cmd
and cmds = cmd list;;

type prog = cmds;;
