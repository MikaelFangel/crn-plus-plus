// Cem Akarsubasi, 2024-06-21
module CRN.Simulator

open CRN.AST

let private getlhs =
    function
    | ReactionS.Reaction(lhs, _, _) -> lhs

let private getrhs =
    function
    | ReactionS.Reaction(_, rhs, _) -> rhs

let private getspeed =
    function
    | ReactionS.Reaction(_, _, speed) -> speed

let private getname =
    function
    | ExprSpecies.Species name -> name

// get all of the unique species
let private getspecies reactions =
    match reactions with
    | ReactionS.Reaction(rhs, lhs, _) -> rhs @ lhs |> set

// count how many x species exist in the given reactions
let rec private count x reactions =
    match reactions with
    | [] -> 0
    | (Species y :: ys) when x = y -> 1 + (count x ys)
    | (y :: ys) -> count x ys

// get the change of the given species between lhs and rhs
let private getchange lhs rhs current =
    let before = count current lhs
    let after = count current rhs
    let change = after - before
    change

let private checkInputs fromODE (fromArguments: Map<_, _>) =
    let missing = Set.difference (Set fromODE) (Set fromArguments.Keys)

    if missing.Count <> 0 then
        raise (System.ArgumentException($"Missing elements: %A{missing}"))

    let unnecessary = Set.difference (Set fromArguments.Keys) (Set fromODE)

    if unnecessary.Count <> 0 then
        eprintfn $"ODE Solver: Unnecessary arguments dropped: %A{unnecessary}"

    unnecessary

// Internal functions for the original obnoxiously overengineered ODE Solver
module private Functional1 =
    /// The state of a system of key value pairs
    /// Each value represents the current concentration of a species
    /// Pass it to an OdeEq to evaluate the current change in the system
    type private OdeState = Map<string, float>

    /// A single ODE Equation
    [<RequireQualifiedAccess>]
    type private OdeEq = OdeState -> float

    /// A system of ordinary differential equations
    type internal Ode = { Eqs: Map<string, OdeEq> }

    let private composeOde ode1 ode2 =
        fun m ->
            let r1 = ode1 m
            let r2 = ode2 m
            (r1 + r2)

    let private composeOdes (odes: OdeEq list) =
        List.fold (fun state m -> composeOde state m) (fun _ -> 0.0) odes

    let private odeof current lhs rhs speed : OdeEq =
        let name = getname current
        let rhsthis = List.filter (fun elem -> elem = current) rhs
        // change indicates the multiplicity of the current element
        let multiplicity = getchange lhs rhsthis name


        let ode' (Os: Map<string, float>) =
            let res =
                lhs
                |> List.map getname
                |> List.map (fun spec -> Map.find spec Os)
                |> List.fold (fun acc v -> acc * v) 1.0

            speed * res * (float multiplicity)

        ode'

    let internal createODE (reactions: ReactionS list) : Ode =
        let m =
            List.map getspecies reactions
            |> List.fold Set.union Set.empty
            |> Set.toList
            |> List.map (fun one ->
                let func =
                    reactions
                    |> List.map (fun reaction -> reaction |> getlhs, reaction |> getrhs, reaction |> getspeed)
                    |> List.map (fun (lhs, rhs, speed) -> odeof one lhs rhs speed)
                    |> composeOdes

                getname one, func)
            |> Map.ofList

        { Eqs = m }

    let internal forwardEuler system state time =
        state
        |> Map.map (fun k current ->
            let eq = (system.Eqs.Item k)
            let diff = eq state
            current + time * diff)


// Typedefs for the optimized versions

type rxnmask_t = int array array
type eqmask_t = float array array

/// Optimized functional ODE solver based on the imperative version
/// Mutation has been replaced with list adapters
/// However since dotNET list adapters eagerly create intermediate arrays "garbage"
/// This implementation is quite a bit slower than the imperative version
module private Functional2 =

    let inline private applyrxn (arr: float array) (rxnmask: int array) : float =
        Array.zip arr rxnmask
        |> Array.filter (fun (_, maskelem) -> maskelem > 0)
        |> Array.fold (fun acc (elem, mask) -> acc * pown elem mask) 1.0

    let inline private composerxns (rxnresults: float array) (eqmask: float array) : float =
        Array.zip rxnresults eqmask
        |> Array.fold (fun acc (elem, mask) -> acc + elem * mask) 0.0

    let inline private differences
        (input: float array)
        (rxnmasks: int array array)
        (eqmasks: float array array)
        : float array =
        let inter = Array.map (applyrxn input) rxnmasks
        Array.map (composerxns inter) eqmasks

    let internal forwardEuler input rxnmasks eqmasks time =
        let diff = differences input rxnmasks eqmasks

        Array.zip input diff
        |> Array.map (fun (cur, dif) -> (cur + dif * time))

/// Imperative implementation of the ODE solver
/// This implementation avoids most intermediate allocations and does all operations
/// inside "hot loops" in the most direct way which will most likely result in the best 
/// possible performance.
/// 
/// The other idea I had would be to metaprogram C code and call a C compiler, build it
/// onto dynlib, link it at runtime and use shared memory to directly access unmanaged
/// memory which would completely avoid allocations.
module private Imperative =
    /// arr: 1 x reactions
    /// rxnmask: 1 x reactions
    let inline private applyrxn (arr: float array) (rxnmask: int array) : float =
        let mutable ret = 1.0

        for i in 0 .. arr.Length - 1 do
            if rxnmask[i] > 0 then
                ret <- ret * pown arr[i] rxnmask[i]

        ret

    /// rxnresults: 1 x species
    /// eqmask: 1 x species
    let inline private composerxns (rxnresults: float array) (eqmask: float array) : float =
        let mutable ret = 0.0

        for i in 0 .. rxnresults.Length - 1 do
            ret <- ret + rxnresults[i] * eqmask[i]

        ret

    /// input: 1 x species
    /// rxnmasks: reactions x species
    /// inter: 1 x reactions
    /// eqmasks: species x reactions
    /// output: 1 x species
    let inline private differences
        (input: float array)
        (rxnmasks: int array array)
        (eqmasks: float array array)
        : float array =
        let reactions = rxnmasks.Length
        let species = input.Length
        let output = Array.zeroCreate species
        let inter = Array.zeroCreate reactions

        for i: int32 in 0 .. reactions - 1 do
            inter[i] <- applyrxn input rxnmasks[i]

        for j in 0 .. species - 1 do
            output[j] <- composerxns inter eqmasks[j]

        output

    let inline internal forwardEuler input rxnmasks eqmasks time =
        let diff = differences input rxnmasks eqmasks

        for i in 0 .. input.Length - 1 do
            diff[i] <- (diff[i] * time + input[i])

        diff

// Common functions for Functional2, Imperative, Imperative2 solvers

let private createRXNmask (namelist: string list) (reaction: ReactionS) : int array =
    let lhs = getlhs reaction
    namelist |> List.map (fun name -> count name lhs) |> List.toArray

let private createEQmask (rxns: ReactionS list) (name: string) : float array =
    let oneeq name rxn =
        let (lhs, rhs, speed) = getlhs rxn, getrhs rxn, getspeed rxn
        let change = getchange lhs rhs name
        float change * speed

    rxns |> List.map (oneeq name) |> List.toArray

/// Returns list of names, rxnmask, eqmask from given reactions
let private createODE2 reactions : (string list * rxnmask_t * eqmask_t) =
    let species =
        List.map getspecies reactions |> List.fold Set.union Set.empty |> Set.toList

    let namelist = List.map string species

    let rxnmasks = reactions |> List.map (createRXNmask namelist) |> List.toArray

    let eqmasks = namelist |> List.map (createEQmask reactions) |> List.toArray

    namelist, rxnmasks, eqmasks


/// Solve a given ODE based on an initial state and step size
/// Use solveODEFast instead as it is 50-100 times faster.
let solveODE (initial: Map<string, float>) step reactions =
    let ode = Functional1.createODE reactions

    let unnecessary = checkInputs ode.Eqs.Keys initial

    let initial = Map.filter (fun key _ -> Set.contains key unnecessary |> not) initial

    initial
    |> Seq.unfold (fun state ->
        let newstate = Functional1.forwardEuler ode state step
        Some(newstate, newstate))
    |> Seq.append (Seq.singleton initial)

/// Solve a given ODE based on an initial state and step size
/// Use the imperative solver
let solveODEFast
    (initial: Map<string, float>)
    (step: float)
    (reactions: ReactionS list)
    : string list * float array seq =

    let (namelist, rxnmasks, eqmasks) = createODE2 reactions

    let _ = checkInputs namelist initial

    let initial =
        namelist |> List.map (fun elem -> Map.find elem initial) |> List.toArray

    let sequence =
        initial
        |> Seq.unfold (fun state ->
            let newstate = Imperative.forwardEuler state rxnmasks eqmasks step
            Some(newstate, newstate))
        |> Seq.append (Seq.singleton initial)

    namelist, sequence


/// Solve a given ODE based on an initial state and step size
/// Use the functional solver
let solveODEFunctional
    (initial: Map<string, float>)
    (step: float)
    (reactions: ReactionS list)
    : string list * float array seq =
    let (namelist, rxnmasks, eqmasks) = createODE2 reactions

    let _ = checkInputs namelist initial

    let initial =
        namelist |> List.map (fun elem -> Map.find elem initial) |> List.toArray

    let sequence =
        initial
        |> Seq.unfold (fun state ->
            let newstate = Functional2.forwardEuler state rxnmasks eqmasks step
            Some(newstate, newstate))
        |> Seq.append (Seq.singleton initial)

    namelist, sequence

/// Convert the result of SolveODEFast to the result of SolveODE
let ArraytoMap names arr =
    arr |> Array.toList |> List.zip names |> Map.ofList
