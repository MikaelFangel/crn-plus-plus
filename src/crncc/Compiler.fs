module CRN.Compiler

open CRN.AST

type Env = Map<string, float>

// Flag species
let XgtY = ExprSpecies.Species "XgtY"
let XltY = ExprSpecies.Species "XltY"
let YgtX = ExprSpecies.Species "YgtX"
let YltX = ExprSpecies.Species "YltX"
let H = ExprSpecies.Species "H"
let B1 = ExprSpecies.Species "B1"
let B2 = ExprSpecies.Species "B2"

// Convert ExprSpecies to Species
let exprSpeciesToSpecies =
    function
    | ExprSpecies.Species(s) -> s

// Create clock species by given the number of steps
[<TailCall>]
let createClockSpecies nstep =
    let n = nstep * 3

    let rec createClockSpeciesInner acc =
        function
        | 0 -> acc
        | n -> createClockSpeciesInner (ExprSpecies.Species $"X_{n}" :: acc) (n - 1)

    createClockSpeciesInner [] n

// Add a list of species to a side of an reaction
let addClotckToExprs (cspec: ExprSpecies list) (side: ExprS) =
    match side with
    | ExprS.Expr(e) -> ExprS.Expr(cspec @ e)

// Add a list species to a reaction
let addClockToRxn (cspec: ExprSpecies list) (rxn: ReactionS) =
    match rxn with
    | ReactionS.Reaction(lhs, rhs, rate) ->
        ReactionS.Reaction(addClotckToExprs cspec lhs, addClotckToExprs cspec rhs, rate)

// Add a list species to a step
let addClockToStep (cspec: ExprSpecies) (step: ReactionS list) =
    List.map (fun rxn -> addClockToRxn [ cspec ] rxn) step

// Create a reaction with a given rate
let createReactionWRate (rate: float) (lhs: SpeciesS list) (rhs: SpeciesS list) =
    ReactionS.Reaction(
        lhs |> List.map (fun e -> ExprSpecies.Species e) |> ExprS.Expr,
        rhs |> List.map (fun e -> ExprSpecies.Species e) |> ExprS.Expr,
        rate
    )

// Create a reaction with rate default rate of 1.0
let createReaction = createReactionWRate 1.0

// Compile a module to a list of reactions
let compileModule (mods: ModuleS) =
    match mods with
    | ModuleS.Ld(a, b) -> [ createReaction [ a ] [ a; b ]; createReaction [ b ] [] ]
    | ModuleS.Add(a, b, c) ->
        [ createReaction [ a ] [ a; c ]
          createReaction [ b ] [ b; c ]
          createReaction [ c ] [] ]
    | ModuleS.Sub(a, b, c) ->
        [ createReaction [ a ] [ a; c ]
          createReaction [ b ] [ b; exprSpeciesToSpecies H ]
          createReaction [ c ] []
          createReaction [ c; exprSpeciesToSpecies H ] [] ]
    | ModuleS.Mul(a, b, c) -> [ createReaction [ a; b ] [ a; b; c ]; createReaction [ c ] [] ]
    | ModuleS.Div(a, b, c) -> [ createReaction [ a ] [ a; c ]; createReaction [ b; c ] [ b ] ]
    | ModuleS.Sqrt(a, b) -> [ createReaction [ a ] [ a; b ]; createReactionWRate 0.5 [ b; b ] [] ]
    | ModuleS.Cmp(x, y) ->
        [ createReaction [ exprSpeciesToSpecies XgtY; y ] [ exprSpeciesToSpecies XltY; y ]
          createReaction [ exprSpeciesToSpecies XltY; x ] [ exprSpeciesToSpecies XgtY; x ]
          createReaction [ exprSpeciesToSpecies YgtX; x ] [ exprSpeciesToSpecies YltX; x ]
          createReaction [ exprSpeciesToSpecies YltX; y ] [ exprSpeciesToSpecies YgtX; y ] ]

// Inject the approximated majority gate when a comparison is made
let injectWhenCmp =
    List.collect (fun com ->
        match com with
        | CommandS.Module(ModuleS.Cmp(_, _)) ->
            // Approximated majority for X
            [ createReaction
                  [ exprSpeciesToSpecies XgtY; exprSpeciesToSpecies XltY ]
                  [ exprSpeciesToSpecies XltY; exprSpeciesToSpecies B1 ]
              createReaction
                  [ exprSpeciesToSpecies B1; exprSpeciesToSpecies XltY ]
                  [ exprSpeciesToSpecies XltY; exprSpeciesToSpecies XltY ]
              createReaction
                  [ exprSpeciesToSpecies XltY; exprSpeciesToSpecies XgtY ]
                  [ exprSpeciesToSpecies XgtY; exprSpeciesToSpecies B1 ]
              createReaction
                  [ exprSpeciesToSpecies B1; exprSpeciesToSpecies XgtY ]
                  [ exprSpeciesToSpecies XgtY; exprSpeciesToSpecies XgtY ]

              // Approximated majority for Y
              createReaction
                  [ exprSpeciesToSpecies YgtX; exprSpeciesToSpecies YltX ]
                  [ exprSpeciesToSpecies YltX; exprSpeciesToSpecies B2 ]
              createReaction
                  [ exprSpeciesToSpecies B2; exprSpeciesToSpecies YltX ]
                  [ exprSpeciesToSpecies YltX; exprSpeciesToSpecies YltX ]
              createReaction
                  [ exprSpeciesToSpecies YltX; exprSpeciesToSpecies YgtX ]
                  [ exprSpeciesToSpecies YgtX; exprSpeciesToSpecies B2 ]
              createReaction
                  [ exprSpeciesToSpecies B2; exprSpeciesToSpecies YgtX ]
                  [ exprSpeciesToSpecies YgtX; exprSpeciesToSpecies YgtX ] ]
        | _ -> [])

// Compiles a single command to a list of reactions
let rec compileCommand (com: CommandS) =
    match com with
    | CommandS.Module(m) -> compileModule m
    | CommandS.Reaction(r) -> [ r ]
    | CommandS.Condition(c) -> compileCondition c

and compileCondition (cond: ConditionS) =
    match cond with
    | ConditionS.Gt cmd ->
        List.collect (fun c -> compileCommand c) cmd
        |> List.map (fun r -> addClockToRxn [ XgtY; YltX ] r)
    | ConditionS.Ge cmd ->
        List.collect (fun c -> compileCommand c) cmd
        |> List.map (fun r -> addClockToRxn [ XgtY; YgtX; YltX ] r)
    | ConditionS.Eq cmd ->
        List.collect (fun c -> compileCommand c) cmd
        |> List.map (fun r -> addClockToRxn [ XgtY; YgtX ] r)
    | ConditionS.Lt cmd ->
        List.collect (fun c -> compileCommand c) cmd
        |> List.map (fun r -> addClockToRxn [ XltY; YgtX ] r)
    | ConditionS.Le cmd ->
        List.collect (fun c -> compileCommand c) cmd
        |> List.map (fun r -> addClockToRxn [ XgtY; XltY; YgtX ] r)

// Compile the root of the AST to a list of reactions
let compileRootS (conc, step) (root: RootS) =
    match root with
    | RootS.Conc(c) -> (c :: conc, step)
    | RootS.Step(s) ->
        (conc,
         List.collect (fun s -> compileCommand s) s :: injectWhenCmp s :: step
         |> List.filter (fun e -> e <> []))

// Convert ExprSpecies to string
let exprSpeciesToString =
    function
    | ExprSpecies.Species(s) -> s

// Initialized the environment with the initial values
let intialEnv typeEnv clocksp flag conc : Env =
    let emptyState =
        typeEnv.Species
        |> Seq.append (flag |> List.map (fun s -> s |> exprSpeciesToString))
        |> Seq.fold (fun acc s -> Map.add s 0.0 acc) Map.empty
        |> Map.add (exprSpeciesToString XgtY) 0.50
        |> Map.add (exprSpeciesToString XltY) 0.50
        |> Map.add (exprSpeciesToString YgtX) 0.50
        |> Map.add (exprSpeciesToString YltX) 0.50

    let emptyState =
        List.fold (fun map c -> Map.add (exprSpeciesToString c) 0.0001 map) emptyState clocksp
        |> Map.add ("X_3") (1.0 - (float (List.length clocksp) * 0.0001))

    List.fold
        (fun map c ->
            match c with
            | ConcS.Conc(s, v) ->
                Map.add
                    s
                    (match v with
                     | ValueS.Number(v) -> v)
                    map)
        emptyState
        conc

// Create the oscillator for the clock species
let rec createOscillator n firstspec cspec =
    match cspec with
    | c1 :: c2 :: cspec' ->
        [ createReaction
              [ exprSpeciesToSpecies c1; exprSpeciesToSpecies c2 ]
              [ exprSpeciesToSpecies c2; exprSpeciesToSpecies c2 ] ]
        :: createOscillator (n - 1) firstspec (c2 :: cspec')
    | c :: cspec' ->
        [ createReaction
              [ exprSpeciesToSpecies c; exprSpeciesToSpecies firstspec ]
              [ exprSpeciesToSpecies firstspec; exprSpeciesToSpecies firstspec ] ]
        :: createOscillator (n - 1) firstspec cspec'
    | [] -> []

// Compile a CRN to a list of reactions
let compileCrnS (ast: TypedAST) =
    let (conc, step) =
        match ast |> fst with
        | CrnS.Crn(rootlist) -> rootlist |> List.map (fun r -> compileRootS ([], []) r) |> List.unzip

    let (conc, step) = (conc |> List.collect id, step |> List.collect id)
    let cspec = step |> List.length |> createClockSpecies
    let oscillator = createOscillator (cspec |> List.length) cspec.[0] cspec

    let env = intialEnv (snd ast) cspec [ H; B1; B2 ] conc

    let rxn =
        step
        |> List.mapi (fun i s -> addClockToStep cspec.[3 + i * 3 - 1] s)
        |> List.append oscillator
        |> List.collect id

    (env, rxn)
