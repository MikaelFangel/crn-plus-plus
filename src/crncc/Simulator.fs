module CRN.Simulator

open CRN.AST

/// The state of a system of key value pairs
/// Each value represents the current concentration of a species
/// Pass it to an OdeEq to evaluate the current change in the system
type OdeState = Map<string, float>

/// A single ODE Equation
[<RequireQualifiedAccess>]
type OdeEq = OdeState -> float

/// A system of ordinary differential equations
type Ode = { Eqs: Map<string, OdeEq> }

let private getlhs =
    function
    | ReactionS.Reaction(ExprS.Expr(lhs), _, _) -> lhs

let private getrhs =
    function
    | ReactionS.Reaction(_, ExprS.Expr(rhs), _) -> rhs

let private getspeed =
    function
    | ReactionS.Reaction(_, _, speed) -> speed

let private getname =
    function
    | ExprSpecies.Species name -> name

// get all of the unique species
let private getspecies =
    function
    | ReactionS.Reaction(ExprS.Expr(rhs), ExprS.Expr(lhs), _) -> rhs @ lhs |> set

let rec private count x =
    function
    | [] -> 0
    | (Species y :: ys) when x = y -> 1 + (count x ys)
    | (y :: ys) -> count x ys

let private getchange lhs rhs current =
    let before = count current lhs
    let after = count current rhs
    let change = after - before
    change

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
            |> List.map (
                fun spec -> 
                Map.find spec Os
                )
            |> List.fold (fun acc v -> acc * v) 1.0

        speed * res * (float multiplicity)

    ode'

let private createODE (reactions: ReactionS list) : Ode =
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

let private forwardEuler system state time =
    state
    |> Map.map (fun k v ->
        let eq = (system.Eqs.Item k)
        let result = eq state
        v + time * result |> max 0.0)

/// Solve a given ODE based on an initial state and step size
let solveODE (initial: Map<string, float>) step reactions =
    let ode = createODE reactions

    let missing = Set.difference (Set ode.Eqs.Keys) (Set initial.Keys)
    if missing.Count <> 0 then
        raise (System.ArgumentException($"Missing elements: %A{missing}"))

    let unnecessary = Set.difference (Set initial.Keys) (Set ode.Eqs.Keys)
    if unnecessary.Count <> 0 then
        eprintfn $"ODE Solver: Unnecessary arguments dropped: %A{unnecessary}"

    let initial = Map.filter (fun key _ -> Set.contains key unnecessary |> not) initial

    initial
    |> Seq.unfold (fun state ->
        let newstate = forwardEuler ode state step
        Some(newstate, newstate))
    |> Seq.append (Seq.singleton initial)


/// arr: species
/// rxnmask: species
let inline private applyrxn (arr: float array) (rxnmask: int array): float =
    let mutable ret = 0.0
    for i in 0..arr.Length-1 do
        if rxnmask[i] > 0 then
            ret <- ret + pown arr[i] rxnmask[i]
    ret


let inline private composerxns (rxnresults: float array) (eqmask: float array): float =
    let mutable ret = 0.0
    for i in 0..rxnresults.Length-1 do
        ret <- ret + rxnresults[i] * eqmask[i]
    ret

///   spec x rxns
/// input: 1 x spec
/// rxnarrays: rxns x spec (as many arrays as reactions, array length of species)
/// eqmasks: spec x rxns (as many arrays as species)
let inline private differences (input: float array) 
                       (rxnmasks: int array array) 
                       (eqmasks: float array array): float array =
    let species = rxnmasks.Length
    let rxns = eqmasks.Length
    let output = Array.zeroCreate input.Length

    let inter = Array.zeroCreate species
    for j in 0..rxns-1 do
        for i in 0..species-1 do 
            inter[i] <- applyrxn input rxnmasks[i]
        output[j] <- composerxns inter eqmasks[j]
    output
        
let inline private eulerFast input rxnmasks eqmasks time =
    let diff = differences input rxnmasks eqmasks
    for i in 0..input.Length-1 do
        diff[i] <- max (diff[i] * time + input[i]) 0.0
    diff

type rxnmask_t = int array array
type eqmask_t = float array array

// species length
let private createRXNmask (namelist: string list) (reaction: ReactionS): int array =
    let lhs = getlhs reaction
    namelist
    |> List.map (fun name -> count name lhs)
    |> List.toArray

// number of equations
let private createEQmask (rxns: ReactionS list) (name: string): float array =
    let oneeq name rxn =
        let (lhs, rhs, speed) = getlhs rxn, getrhs rxn, getspeed rxn
        let change = getchange lhs rhs name
        float change * speed
    rxns
    |> List.map (oneeq name)
    |> List.toArray

let private createODE2 reactions: (string list * rxnmask_t * eqmask_t) =
    let species = 
        List.map getspecies reactions
        |> List.fold Set.union Set.empty
        |> Set.toList
    let namelist = List.map string species

    let rxnmasks = 
        reactions
        |> List.map (createRXNmask namelist) 
        |> List.toArray 

    let eqmasks =
        namelist 
        |> List.map (createEQmask reactions)
        |> List.toArray

    namelist, rxnmasks, eqmasks 

let solveODEFast (initial: Map<string, float>) 
                 (step: float) 
                 (reactions: ReactionS list): string list * float array seq =
    
    let (namelist, rxnmasks, eqmasks) = createODE2 reactions

    let missing = Set.difference (Set namelist) (Set initial.Keys)
    if missing.Count <> 0 then
        raise (System.ArgumentException($"Missing elements: %A{missing}"))

    let unnecessary = Set.difference (Set initial.Keys) (Set namelist)
    if unnecessary.Count <> 0 then
        eprintfn $"ODE Solver: Unnecessary arguments dropped: %A{unnecessary}"

    let initial = 
        namelist
        |> List.map (fun elem -> Map.find elem initial)
        |> List.toArray
    
    let sequence = 
        initial
        |> Seq.unfold (fun state ->
            let newstate = eulerFast state rxnmasks eqmasks step
            Some(newstate, newstate))

    namelist, sequence
