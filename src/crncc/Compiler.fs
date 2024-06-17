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
    | ModuleS.Sub(a, b, c) -> failwith "not implemented"
    | ModuleS.Mul(a, b, c) -> [ createReaction [ a; b ] [ a; b; c ]; createReaction [ c ] [] ]
    | ModuleS.Div(a, b, c) -> [ createReaction [ a ] [ a; c ]; createReaction [ b; c ] [ b ] ]
    | ModuleS.Sqrt(a, b) -> [ createReaction [ a ] [ a; b ]; createReactionWRate 0.5 [ b; b ] [] ]
    | ModuleS.Cmp(a, b) -> failwith "not implemented"

let compileCommand (com: CommandS) =
    match com with
    | CommandS.Module(x) -> compileModule x
    | CommandS.Reaction(x) -> [ x ]
    | CommandS.Condition(x) -> failwith "Not implemented"

let compileCommandList (coms: CommandS list) = List.map compileCommand coms

let compileRoot (root: RootS) xs =
    match root with
    | RootS.Step(x) -> compileCommandList x
    | RootS.Conc(x) -> failwith "not implemented"

let rec compileAst (ast: TypedAST) = ast |> fst |> List.map compileRoot
