module CRN.Compiler

open CRN.AST

[<TailCall>]
let createClockSpecies nstep =
    let n = nstep * 3

    let rec createClockSpeciesInner acc =
        function
        | 0 -> List.rev acc
        | n -> createClockSpeciesInner (ExprSpecies.Species $"X_{n}" :: acc) (n - 1)

    createClockSpeciesInner [] n

let addClotckToExprs (cspec: ExprSpecies) (side: ExprS) =
    match side with
    | ExprS.Expr(e) -> ExprS.Expr(cspec :: e)

let addClockToRxn (cspec: ExprSpecies) (rxn: ReactionS) =
    match rxn with
    | ReactionS.Reaction(lhs, rhs, rate) ->
        ReactionS.Reaction(addClotckToExprs cspec lhs, addClotckToExprs cspec rhs, rate)

let addClockToStep (cspec: ExprSpecies) (step: ReactionS list) =
    List.map (fun rxn -> addClockToRxn cspec rxn) step

let createReactionWRate (rate: float) (lhs: SpeciesS list) (rhs: SpeciesS list) =
    match rhs with
    | [] ->
        ReactionS.Reaction(
            lhs |> List.map (fun e -> ExprSpecies.Species e) |> ExprS.Expr,
            rhs |> List.map (fun e -> ExprSpecies.Species e) |> ExprS.Expr,
            rate
        )
    | _ -> ReactionS.Reaction(lhs |> List.map (fun e -> ExprSpecies.Species e) |> ExprS.Expr, [] |> ExprS.Expr, rate)

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
    | ModuleS.Cmp(x, y) ->
        [ createReaction [ SpeciesS "XgtY"; x ] [ SpeciesS "XltY"; y ]
          createReaction [ SpeciesS "XltY"; x ] [ SpeciesS "XgtY"; x ] ]

let injectWhenCmp =
    List.collect (fun com ->
        match com with
        | CommandS.Module(m) ->
            match m with
            | ModuleS.Cmp(_, _) ->
                [ createReaction [ SpeciesS "XgtY"; SpeciesS "XltY" ] [ SpeciesS "XltY"; SpeciesS "B" ]
                  createReaction [ SpeciesS "B"; SpeciesS "XltY" ] [ SpeciesS "XltY"; SpeciesS "XltY" ]
                  createReaction [ SpeciesS "XltY"; SpeciesS "XgtY" ] [ SpeciesS "XgtY"; SpeciesS "B" ]
                  createReaction [ SpeciesS "B"; SpeciesS "XgtY" ] [ SpeciesS "XgtY"; SpeciesS "XgtY" ] ]
            | _ -> []
        | _ -> [])

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
    | RootS.Step(s) ->
        (conc,
         List.collect (fun s -> compileCommand s) s :: injectWhenCmp s :: step
         |> List.filter (fun e -> e <> []))

let compileCrnS (ast: TypedAST) =
    let (conc, step) =
        match ast |> fst with
        | CrnS.Crn(rootlist) -> rootlist |> List.map (fun r -> compileRootS ([], []) r) |> List.unzip

    let (conc, step) = (conc |> List.collect id, step |> List.collect id)
    let cspec = step |> List.length |> createClockSpecies

    step |>List.mapi (fun i s -> addClockToStep cspec.[i * 3]  s)
