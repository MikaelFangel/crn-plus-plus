module CRN.AST

type TypingEnv =
    {
        // All species have to be defined before use and appear on the left side
        Species: Set<string>
        // Consts can only appear in limited circumstances and species cannot share names with consts
        Consts: Set<string>
    }

[<RequireQualifiedAccess>]
type SpeciesS = string

type ExprSpecies = Species of SpeciesS
    with override this.ToString() =
            match this with 
                Species(s) -> s.ToString()

[<RequireQualifiedAccess>]
type PNumberS = float

[<RequireQualifiedAccess>]
type ValueS =
    | Literal of string
    | Number of PNumberS
    with override this.ToString() =
            match this with
            Literal(st) -> st
            | Number(num) -> num.ToString()

[<RequireQualifiedAccess>]
type ConcS = Conc of SpeciesS * ValueS
    with override this.ToString() =
                match this with
                    Conc(species, value) -> $"conc[{species}, {value}]"

[<RequireQualifiedAccess>]
type ExprS = Expr of list<ExprSpecies>
    with override this.ToString() =
            match this with
                Expr(lst) -> List.map string lst |> String.concat "+" 

[<RequireQualifiedAccess>]
type ReactionS = Reaction of ExprS * ExprS * PNumberS
    with override this.ToString() =
            match this with
                Reaction(expr1, expr2, num) -> $"rxn[{expr1}, {expr2}, {num}]"

[<RequireQualifiedAccess>]
type ModuleS =
    | Ld of SpeciesS * SpeciesS
    | Add of SpeciesS * SpeciesS * SpeciesS
    | Sub of SpeciesS * SpeciesS * SpeciesS
    | Mul of SpeciesS * SpeciesS * SpeciesS
    | Div of SpeciesS * SpeciesS * SpeciesS
    | Sqrt of SpeciesS * SpeciesS
    | Cmp of SpeciesS * SpeciesS
    with override this.ToString() =
            match this with
                | Ld(sp1, sp2) -> $"ld[{sp1}, {sp2}]"
                | Sqrt(sp1, sp2) -> $"sqrt[{sp1}, {sp2}]"
                | Cmp(sp1, sp2) -> $"cmp[{sp1}, {sp2}]"
                | Add(sp1, sp2, sp3) -> $"add[{sp1}, {sp2}, {sp3}]"
                | Sub(sp1, sp2, sp3) -> $"sub[{sp1}, {sp2}, {sp3}]"
                | Mul(sp1, sp2, sp3) -> $"mul[{sp1}, {sp2}, {sp3}]"
                | Div(sp1, sp2, sp3) -> $"div[{sp1}, {sp2}, {sp3}]"
                

let conditionString beginning lst =
    let center = List.map string lst |> String.concat ",\n"
    $"{beginning}[{{ {center} }}]"


[<RequireQualifiedAccess>]
type ConditionS =
    | Gt of CommandS list
    | Ge of CommandS list
    | Eq of CommandS list
    | Lt of CommandS list
    | Le of CommandS list
    with override this.ToString() =
            match this with
                | Gt(lst) -> conditionString "ifGT" lst
                | Ge(lst) -> conditionString "ifGE" lst
                | Eq(lst) -> conditionString "ifEQ" lst
                | Lt(lst) -> conditionString "ifLT" lst
                | Le(lst) -> conditionString "ifLE" lst

and CommandS =
    | Reaction of ReactionS
    | Module of ModuleS
    | Condition of ConditionS
    with override this.ToString() =
            match this with
                | Reaction(reaction) -> reaction.ToString()
                | Module(m) -> m.ToString()
                | Condition(con) -> con.ToString()

[<RequireQualifiedAccess>]
type RootS =
    | Conc of ConcS
    | Step of CommandS list
    with override this.ToString() =
            match this with
                | Conc(conc) -> conc.ToString()
                | Step(commands) -> conditionString "step" commands

[<RequireQualifiedAccess>]
type CrnS = Crn of list<RootS>
    with override this.ToString() =
            match this with
            | Crn(lst) -> 
                let center = String.concat "," (List.map string lst)
                $"crn = {{ {center} }}"

type UntypedAST = CrnS
type TypedAST = CrnS * TypingEnv
