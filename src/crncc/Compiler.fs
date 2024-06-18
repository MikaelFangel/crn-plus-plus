module CRN.Compiler

open CRN.AST

let createReactionWRate (rate: float) (lhs: SpeciesS list) (rhs: SpeciesS list) =
    match rhs with
    | [] ->
        ReactionS(
            lhs |> List.map (fun e -> ExprSpecies.Species e),
            rhs |> List.map (fun e -> ExprSpecies.Species e),
            rate
        )
    | _ -> ReactionS(lhs |> List.map (fun e -> ExprSpecies.Species e), [ ExprSpecies.Null ], rate)

let createReaction = createReactionWRate 1.0

let compileModule (mods: ModuleS) =
    match mods with
    | ModuleS.Ld(a, b) -> [ createReaction [ a ] [ a; b ]; createReaction [ b ] [] ]
    | ModuleS.Add(a, b, c) ->
        [ createReaction [ a ] [ a; c ]
          createReaction [ b ] [ b; c ]
          createReaction [ c ] [] ]
    | ModuleS.Sub(a, b, c) ->
        [ createReaction [ a ] [ a; b ]
          createReaction [ b ] [ b; SpeciesS "H" ]
          createReaction [ c ] []
          createReaction [ c; SpeciesS "H" ] [] ]
    | ModuleS.Mul(a, b, c) -> [ createReaction [ a; b ] [ a; b; c ]; createReaction [ c ] [] ]
    | ModuleS.Div(a, b, c) -> [ createReaction [ a ] [ a; c ]; createReaction [ b; c ] [ b ] ]
    | ModuleS.Sqrt(a, b) -> [ createReaction [ a ] [ a; b ]; createReactionWRate 0.5 [ b; b ] [] ]
    | ModuleS.Cmp(_, _) -> []

let rec compileCommand (com: CommandS) =
    match com with
    | CommandS.Module(m) -> compileModule m
    | CommandS.Reaction(r) -> [ r ]
    | CommandS.Condition(c) -> compileCondition c

and compileCondition (cond: ConditionS) =
    match cond with
    | ConditionS.Gt commands
    | ConditionS.Ge commands
    | ConditionS.Eq commands
    | ConditionS.Lt commands
    | ConditionS.Le commands -> List.collect compileCommand commands

let compileRootS (conc, step) (root: RootS) =
    match root with
    | RootS.Conc(c) -> (c :: conc, step)
    | RootS.Step(s) -> (conc, List.collect (fun s -> compileCommand s) s :: step)

let compileCrnS (ast: TypedAST) =
    ast |> fst |> List.map (fun r -> compileRootS ([], []) r)
