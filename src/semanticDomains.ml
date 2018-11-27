(** Semantic Domains
    @author Zachary Ferguson

    Semantic domains:
    i ∈ Int (Machine) integers
    b ∈ {true, false, error} Booleans
    l ∈ Objects (where null ̸∈ Objects) Objects
    ℓ ∈ Loc ≜ Objects ∪ {null} Locations
    ν = clo⟨x, C, ξ⟩ ∈ Clo ≜ clo(Var × Cmd × Stack) Closures
    v ∈ Val ≜ Field ∪ Int ∪ Loc ∪ Clo Values
    t ∈ Tva ≜ Val ∪ {error} Tainted values8
    ρ ∈ Env ≜ Var ̸7→ Objects Environments
    φ = decl⟨ρ⟩/ ∈ Frame ≜ decl(Env) ∪ Frames
    call⟨ρ, ξ⟩ call(Env × Stack)
    ξ ∈ Stack ≜ (Frame)
    ⋆ Stacks
    h ∈ Heap ≜ Objects × Field ̸7→ Tva Heap
    σ = ⟨ξ, h⟩ ∈ State ≜ Stack × Heap States
    C ∈ Ctrl ::= skip | {Ctrl; Ctrl} | while b Ctrl | Control
    if b Ctrl else Ctrl | {Ctrl 9 Ctrl} | atom(Ctrl) |
    block(Ctrl)
    Γ = ⟨C, σ⟩/σ ∈ Conf ≜ (Ctrl × State) ∪ State ∪ {error}*)

type obj = int;;

type closure = {param : AbstractSyntaxTree.ident; body : AbstractSyntaxTree.cmd; call_stack : stack}

and tainted_value =
  | Field of string
  | Int of int
  | Object of obj
  | Null
  | Closure of closure ref
  | Error of string
  (* and environment = (AbstractSyntaxTree.ident, int) Hashtbl.t
     and frame = Decl of environment | Call of environment * stack
     and stack = frame list;; *)
(* and tainted_value = Value of value | Error of string *)

and stack = (string, obj) Hashtbl.t;;

type heap = (obj * string, tainted_value) Hashtbl.t;;

(* type state = state * heap;; *)

(* type ctrl = Cmd of AbstractSyntaxTree.cmd | Block of ctrl;; *)
