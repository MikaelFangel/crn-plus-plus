module CRN.Compiler

open CRN.AST

type Env = Map<string, float>

// Flag species
let XgtY = ExprSpecies.Species "XgtY"
let XltY = ExprSpecies.Species "XltY"
let YgtX = ExprSpecies.Species "YgtX"
let YltX = ExprSpecies.Species "YltX"
let H = ExprSpecies.Species "H"
let B = ExprSpecies.Species "B"
let CmpOff = ExprSpecies.Species "CmpOff"

// Convert ExprSpecies to Species
let species =
    function
    | ExprSpecies.Species(s) -> s

// Create clock species by given the number of steps
[<TailCall>]
let clockSpecies nstep =
    let n = nstep * 3

    let rec clockSpeciesInner acc =
        function
        | 0 -> acc
        | n -> clockSpeciesInner (ExprSpecies.Species $"X_{n}" :: acc) (n - 1)

    clockSpeciesInner [] n

// Add a list of species to a side of an reaction
let specToExpr (spec: ExprSpecies list) (side: ExprS) =
    match side with
    | ExprS.Expr(e) -> ExprS.Expr(spec @ e)

// Add a list species to a reaction
let addSpecToRxn (spec: ExprSpecies list) (rxn: ReactionS) =
    match rxn with
    | ReactionS.Reaction(lhs, rhs, rate) -> ReactionS.Reaction(specToExpr spec lhs, specToExpr spec rhs, rate)

// Add a list species to a step
let addSpecToStep (spec: ExprSpecies) (step: ReactionS list) =
    List.map (fun rxn -> addSpecToRxn [ spec ] rxn) step

// Create a reaction with a given rate
let rxnWRate (rate: float) (lhs: SpeciesS list) (rhs: SpeciesS list) =
    ReactionS.Reaction(
        lhs |> List.map (fun e -> ExprSpecies.Species e) |> ExprS.Expr,
        rhs |> List.map (fun e -> ExprSpecies.Species e) |> ExprS.Expr,
        rate
    )

// Create a reaction with rate default rate of 1.0
let rxn = rxnWRate 1.0

// c a module to a list of reactions
let cModule (mods: ModuleS) =
    match mods with
    | ModuleS.Ld(a, b) -> [ rxn [ a ] [ a; b ]; rxn [ b ] [] ]
    | ModuleS.Add(a, b, c) -> [ rxn [ a ] [ a; c ]; rxn [ b ] [ b; c ]; rxn [ c ] [] ]
    | ModuleS.Sub(a, b, c) ->
        [ rxn [ a ] [ a; c ]
          rxn [ b ] [ b; species H ]
          rxn [ c ] []
          rxn [ c; species H ] [] ]
    | ModuleS.Mul(a, b, c) -> [ rxn [ a; b ] [ a; b; c ]; rxn [ c ] [] ]
    | ModuleS.Div(a, b, c) -> [ rxn [ a ] [ a; c ]; rxn [ b; c ] [ b ] ]
    | ModuleS.Sqrt(a, b) -> [ rxn [ a ] [ a; b ]; rxnWRate 0.5 [ b; b ] [] ]
    | ModuleS.Cmp(x, y) ->
        [ rxn [ species XgtY; y ] [ species XltY; y ]
          rxn [ species XltY; species CmpOff ] [ species XgtY; species CmpOff ]
          rxn [ species XltY; x ] [ species XgtY; x ]

          rxn [ species YgtX; x ] [ species YltX; x ]
          rxn [ species YltX; species CmpOff ] [ species YgtX; species CmpOff ]
          rxn [ species YltX; y ] [ species YgtX; y ] ]

// Inject the approximated majority gate when a comparison is made
let am =
    List.collect (fun com ->
        match com with
        | CommandS.Module(ModuleS.Cmp(_, _)) ->
            // Approximated majority for X
            [ rxn [ species XgtY; species XltY ] [ species XltY; species B ]
              rxn [ species B; species XltY ] [ species XltY; species XltY ]
              rxn [ species XltY; species XgtY ] [ species XgtY; species B ]
              rxn [ species B; species XgtY ] [ species XgtY; species XgtY ]

              // Approximated majority for Y
              rxn [ species YgtX; species YltX ] [ species YltX; species B ]
              rxn [ species B; species YltX ] [ species YltX; species YltX ]
              rxn [ species YltX; species YgtX ] [ species YgtX; species B ]
              rxn [ species B; species YgtX ] [ species YgtX; species YgtX ] ]
        | _ -> [])

// cs a single command to a list of reactions
let rec cCommand (com: CommandS) =
    match com with
    | CommandS.Module(m) -> cModule m
    | CommandS.Reaction(r) -> [ r ]
    | CommandS.Condition(c) -> cCondition c

and cCondition (cond: ConditionS) =
    match cond with
    | ConditionS.Gt cmd ->
        List.collect (fun c -> cCommand c) cmd
        |> List.map (fun r -> addSpecToRxn [ XgtY; YltX ] r)
    | ConditionS.Ge cmd ->
        List.collect (fun c -> cCommand c) cmd
        |> List.map (fun r -> addSpecToRxn [ XgtY ] r)
    | ConditionS.Eq cmd ->
        List.collect (fun c -> cCommand c) cmd
        |> List.map (fun r -> addSpecToRxn [ XgtY; YgtX ] r)
    | ConditionS.Lt cmd ->
        List.collect (fun c -> cCommand c) cmd
        |> List.map (fun r -> addSpecToRxn [ XltY; YgtX ] r)
    | ConditionS.Le cmd ->
        List.collect (fun c -> cCommand c) cmd
        |> List.map (fun r -> addSpecToRxn [ YgtX ] r)

// c the root of the AST to a list of reactions
let cRootS (conc, step) (root: RootS) =
    match root with
    | RootS.Conc(c) -> (c :: conc, step)
    | RootS.Step(s) ->
        (conc,
         List.collect (fun s -> cCommand s) s :: am s :: step
         |> List.filter (fun e -> e <> []))

// Initialized the environment with the initial values
let intialEnv typeEnv clocksp flag conc : Env =
    let emptyState =
        typeEnv.Species
        |> Seq.append (flag |> List.map (fun s -> s |> string))
        |> Seq.fold (fun acc s -> Map.add s 0.0 acc) Map.empty
        |> Map.add (string XgtY) 0.50
        |> Map.add (string XltY) 0.50
        |> Map.add (string YgtX) 0.50
        |> Map.add (string YltX) 0.50
        |> Map.add (string CmpOff) 0.50

    let emptyState =
        List.fold (fun map c -> Map.add (string c) 0.0001 map) emptyState clocksp
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
let rec oscillator n firstspec spec =
    match spec with
    | c1 :: c2 :: spec' ->
        [ rxn [ species c1; species c2 ] [ species c2; species c2 ] ]
        :: oscillator (n - 1) firstspec (c2 :: spec')
    | c :: spec' ->
        [ rxn [ species c; species firstspec ] [ species firstspec; species firstspec ] ]
        :: oscillator (n - 1) firstspec spec'
    | [] -> []

// c a CRN to a list of reactions
let compile (ast: TypedAST) =
    let (conc, step) =
        match ast |> fst with
        | CrnS.Crn(rootlist) -> rootlist |> List.map (fun r -> cRootS ([], []) r) |> List.unzip

    let (conc, step) = (conc |> List.collect id, step |> List.collect id)
    let spec = step |> List.length |> clockSpecies
    let oscillator = oscillator (spec |> List.length) spec.[0] spec

    let env = intialEnv (snd ast) spec [ H; B ] conc

    let rxn =
        step
        |> List.mapi (fun i s -> addSpecToStep spec.[3 + i * 3 - 1] s)
        |> List.append oscillator
        |> List.collect id

    (env, rxn)
