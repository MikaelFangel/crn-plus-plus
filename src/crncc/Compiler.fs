module CRN.Compiler

open CRN.AST

type Env = Map<string, float>

// Flag species.
let private XgtY = ExprSpecies.Species "_XgtY"
let private XltY = ExprSpecies.Species "_XltY"
let private YgtX = ExprSpecies.Species "_YgtX"
let private YltX = ExprSpecies.Species "_YltX"
let private H = ExprSpecies.Species "_H"
let private B1 = ExprSpecies.Species "_B1"
let private B2 = ExprSpecies.Species "_B2"
let private CmpOff = ExprSpecies.Species "_CmpOff"

// Convert ExprSpecies to Species.
let private species =
    function
    | ExprSpecies.Species(s) -> s

// Create clock species by given the number of steps.
[<TailCall>]
let private clockSpecies nstep =
    let n = nstep * 2

    let rec clockSpeciesInner acc =
        function
        | 0 -> acc
        | n -> clockSpeciesInner (ExprSpecies.Species $"_X{n}" :: acc) (n - 1)

    clockSpeciesInner [] n

// Add a list of species to a side of an reaction.
let private specToExpr (spec: ExprSpecies list) =
    function
    | e -> spec @ e

// Add a list species to a reaction.
let private addSpecToRxn (spec: ExprSpecies list) =
    function
    | ReactionS.Reaction(lhs, rhs, rate) -> ReactionS.Reaction(specToExpr spec lhs, specToExpr spec rhs, rate)

// Add a list species to the entire step.
let private addSpecToStep (spec: ExprSpecies) (step: ReactionS list) =
    List.map (fun rxn -> addSpecToRxn [ spec ] rxn) step

// Create a reaction with a given rate.
let private rxnWRate (rate: float) (lhs: SpeciesS list) (rhs: SpeciesS list) =
    ReactionS.Reaction(
        lhs |> List.map (fun e -> ExprSpecies.Species e),
        rhs |> List.map (fun e -> ExprSpecies.Species e),
        rate
    )

// Create a reaction with rate default rate of 1.0.
let private rxn = rxnWRate 1.0

// Compile a module to a list of its corresponding reactions.
let private cModule (mods: ModuleS) =
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
        // Normalization for X
        [ rxn [ species XgtY; y ] [ species XltY; y ]
          rxn [ species XltY; species CmpOff ] [ species XgtY; species CmpOff ]
          rxn [ species XltY; x ] [ species XgtY; x ]

          // Normalization for Y
          rxn [ species YgtX; x ] [ species YltX; x ]
          rxn [ species YltX; species CmpOff ] [ species YgtX; species CmpOff ]
          rxn [ species YltX; y ] [ species YgtX; y ] ]

// Inject the approximated majority when a comparison is made.
let private am =
    List.collect (fun com ->
        match com with
        | CommandS.Module(ModuleS.Cmp(_, _)) ->
            // Approximated majority for X.
            [ rxn [ species XgtY; species XltY ] [ species XltY; species B1 ]
              rxn [ species B1; species XltY ] [ species XltY; species XltY ]
              rxn [ species XltY; species XgtY ] [ species XgtY; species B1 ]
              rxn [ species B1; species XgtY ] [ species XgtY; species XgtY ]

              // Approximated majority for Y.
              rxn [ species YgtX; species YltX ] [ species YltX; species B2 ]
              rxn [ species B2; species YltX ] [ species YltX; species YltX ]
              rxn [ species YltX; species YgtX ] [ species YgtX; species B2 ]
              rxn [ species B2; species YgtX ] [ species YgtX; species YgtX ] ]
        | _ -> [])

// Compiles a single command to a list of reactions.
let rec private cCommand =
    function
    | CommandS.Module(m) -> cModule m
    | CommandS.Reaction(rxn) -> [ rxn ]
    | CommandS.Condition(c) -> cCondition c

and cCondition =
    function
    | ConditionS.Gt cmds ->
        List.collect (fun c -> cCommand c) cmds
        |> List.map (fun rxn -> addSpecToRxn [ XgtY; YltX ] rxn)
    | ConditionS.Ge cmds ->
        List.collect (fun c -> cCommand c) cmds
        |> List.map (fun rxn -> addSpecToRxn [ XgtY ] rxn)
    | ConditionS.Eq cmds ->
        List.collect (fun c -> cCommand c) cmds
        |> List.map (fun rxn -> addSpecToRxn [ XgtY; YgtX ] rxn)
    | ConditionS.Lt cmds ->
        List.collect (fun c -> cCommand c) cmds
        |> List.map (fun rxn -> addSpecToRxn [ YgtX; XltY ] rxn)
    | ConditionS.Le cmds ->
        List.collect (fun c -> cCommand c) cmds
        |> List.map (fun rxn -> addSpecToRxn [ YgtX ] rxn)

// Compile the root of the AST to a list of reactions.
let private cRootS (conc, step) =
    function
    | RootS.Conc(c) -> (c :: conc, step)
    | RootS.Step(s) ->
        (conc,
         List.collect (fun s -> cCommand s) s :: am s :: step
         |> List.filter (fun e -> e <> []))

// Initialize the environment with initial values.
let private initialEnv typeEnv clocksp flag constmap conc : Env =
    let flags = [ XgtY; XltY; YgtX; YltX; CmpOff ]
    let iv = 0.1e-11
    let specv = (float (List.length clocksp) * iv) / 2.0
    let chead = clocksp |> List.head |> string
    let clast = clocksp |> List.rev |> List.head |> string

    let state =
        typeEnv.Species
        |> Seq.append (flag |> List.map (fun s -> s |> string))
        |> Seq.fold (fun acc s -> Map.add s 0.0 acc) constmap
        |> (fun map -> List.fold (fun acc k -> Map.add (string k) 0.5 acc) map flags)
        |> (fun map -> List.fold (fun acc c -> Map.add (string c) iv acc) map clocksp)
        |> Map.add (chead) (0.5 - specv)
        |> Map.add (clast) (0.5 - specv)

    List.fold
        (fun map c ->
            match c with
            | ConcS.Conc(s, v) ->
                Map.add
                    s
                    (match v with
                     | ValueS.Number(v) -> v
                     | ValueS.Literal(v) -> constmap |> Map.find v)
                    map)
        state
        conc

// Create the oscillator from the clock species.
[<TailCall>]
let private oscillator n firstspec spec =
    let rec oscillatorInner n firstspec spec acc =
        match spec with
        | c1 :: c2 :: spec' ->
            oscillatorInner
                (n - 1)
                firstspec
                (c2 :: spec')
                ([ rxn [ species c1; species c2 ] [ species c2; species c2 ] ] :: acc)
        | c :: spec' ->
            oscillatorInner
                (n - 1)
                firstspec
                spec'
                ([ rxn [ species c; species firstspec ] [ species firstspec; species firstspec ] ]
                 :: acc)
        | [] -> List.rev acc

    oscillatorInner n firstspec spec []

// Compile a CRN to a list of reactions.
let compile (constmap: Env) (ast: TypedAST) =
    let (conc, step) =
        match ast |> fst with
        | CrnS.Crn(rootlist) -> rootlist |> List.map (fun r -> cRootS ([], []) r) |> List.unzip

    let (conc, step) = (conc |> List.collect id, step |> List.collect id)
    let spec = step |> List.length |> clockSpecies

    let sclocks =
        spec |> List.indexed |> List.filter (fun (i, _) -> i % 2 = 0) |> List.map snd

    let oscillator = oscillator (spec |> List.length) (List.head spec) spec
    let env = initialEnv (snd ast) spec [ H; B1; B2 ] constmap conc

    let rxn =
        step
        |> List.mapi (fun i s -> addSpecToStep (List.item i sclocks) s)
        |> List.append oscillator
        |> List.collect id

    (env, rxn)
