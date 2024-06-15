module CRN.AST

type SpeciesS = string
type PNumberS = PNumber of float

type ConcS = Conc of SpeciesS * PNumberS

type ExprS = Expr of list<SpeciesS>

type ReactionS = Reaction of ExprS * ExprS * PNumberS

type ModuleS =
    | Ld of SpeciesS * SpeciesS
    | Add of SpeciesS * SpeciesS * SpeciesS
    | Sub of SpeciesS * SpeciesS * SpeciesS
    | Mul of SpeciesS * SpeciesS * SpeciesS
    | Div of SpeciesS * SpeciesS * SpeciesS
    | Sqrt of SpeciesS * SpeciesS
    | Cmp of SpeciesS * SpeciesS

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

type StepS = Step of list<CommandS>

type RootS =
    | Conc of ConcS
    | Step of StepS

type CrnS = Crn of list<RootS>