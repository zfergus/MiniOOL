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
    Γ = ⟨C, σ⟩/σ ∈ Conf ≜ (Ctrl × State) ∪ State ∪ {error}
*)


type tainted_value = Int of int
                   | Object of (string, tainted_value) Hashtbl.t
                   | Null
                   | Closure of AbstractSyntaxTree.ident * AbstractSyntaxTree.cmd * stack
                   | Error of string

(* and tainted_value = Value of value | Error of string *)

and stack = (string, int) Hashtbl.t

and heap = (int, tainted_value) Hashtbl.t;;
