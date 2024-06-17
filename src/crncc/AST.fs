module CRN.AST

[<RequireQualifiedAccess>]
type SpeciesS = string

[<RequireQualifiedAccess>]
type PNumberS = float

[<RequireQualifiedAccess>]
type ValueS =
    | Literal of string
    | Number of PNumberS

[<RequireQualifiedAccess>]
type ConcS = SpeciesS * ValueS

[<RequireQualifiedAccess>]
type ExprS = list<SpeciesS>

[<RequireQualifiedAccess>]
type ReactionS = ExprS * ExprS * PNumberS

[<RequireQualifiedAccess>]
type ModuleS =
    | Ld of SpeciesS * SpeciesS
    | Add of SpeciesS * SpeciesS * SpeciesS
    | Sub of SpeciesS * SpeciesS * SpeciesS
    | Mul of SpeciesS * SpeciesS * SpeciesS
    | Div of SpeciesS * SpeciesS * SpeciesS
    | Sqrt of SpeciesS * SpeciesS
    | Cmp of SpeciesS * SpeciesS

[<RequireQualifiedAccess>]
type ConditionS =
    | Gt of CommandS list
    | Ge of CommandS list
    | Eq of CommandS list
    | Lt of CommandS list
    | Le of CommandS list

and CommandS =
    | Reaction of ReactionS
    | Module of ModuleS
    | Condition of ConditionS

[<RequireQualifiedAccess>]
type RootS =
    | Conc of ConcS
    | Step of CommandS list

[<RequireQualifiedAccess>]
type CrnS = list<RootS>

type UntypedAST = CrnS
type TypedAST = CrnS
