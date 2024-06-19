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
    let change = getchange lhs rhsthis name

    let lhs =
        List.map
            (fun r ->
                match r with
                | ExprSpecies.Species(s) -> s)
            lhs

    let ode' (Os: Map<string, float>) =
        let res =
            Os
            |> Map.filter (fun k _ -> List.contains k lhs)
            |> Map.fold (fun acc _ v -> acc * v) 1.0

        speed * res * (float change)

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
        v + time * result)

/// Solve a given ODE based on an initial state and step size
let solveODE initial step reactions =
    let h = step
    let mutable state = initial
    let ode = createODE reactions

    Seq.initInfinite (fun _ ->
        state <- forwardEuler ode state h
        state)