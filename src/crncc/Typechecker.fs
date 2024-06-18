module CRN.Typechecker

open AST

/// Type errors are a series of strings
type TypeError = string

/// This is the same as the TypingEnv type but also keeps track of dangling references
type private TypingEnv' =
    {
        /// All species have to be defined before use and appear on the left side
        Species: Set<string>
        /// Consts can only appear in limited circumstances and species cannot share names with consts
        Consts: Set<string>
        /// Dangling values are ones that are used before being previously declared
        Dangling: Set<string>
        /// Values can only be mutated at one part inside a step
        Mutated: Set<string>
    }

/// Add a species reference to the environment
let private addreference name env : TypingEnv' =
    {env with Species = env.Species.Add name }

/// Resolve a reference to the environment
let private resolvereference name env = 
    if env.Species.Contains name then
        env
    else
        {env with Dangling = env.Dangling.Add name}

/// Check if environment has any dangling species
let private checkdangling env =
    if env.Dangling.IsSubsetOf env.Species then
        Ok env
    else
        let dangling = Set.difference env.Dangling env.Species
        Error [sprintf "Dangling references %O" dangling]

/// Check if something has been previously mutated
let private checkmutation name env =
    if env.Mutated.Contains name then
        Error [sprintf "Species %s mutated multiple times in step." name]
    else
        Ok {env with Mutated = env.Mutated.Add name}

/// left hand side values must be defined
let rec private exprtyperleft env expr =
    let rec exprtyperleft' env expr used =
        match expr with 
            [] -> Ok(env)
            | Null :: _ -> Error [sprintf "Null element on lhs"]
            | Species(head) :: tail -> 
                let env = resolvereference head env
                if Set.contains head used then
                    Error [sprintf "Duplicate species %s in expression" head]
                else 
                    exprtyperleft' env tail (Set.add head used)
    exprtyperleft' env expr (Set [])

/// right hand side values can be created
let private exprtyperright env expr =
    let rec exprtyperright' env expr used =
        match expr with 
            [] -> Ok(env)
            | Null :: tail -> exprtyperright' env tail used
            | Species(head) :: tail -> 
                if Set.contains head used then
                    Error [sprintf "Duplicate species %s in expression" head]
                else 
                    let env' = addreference head env
                    exprtyperright' env' tail (Set.add head used)
    exprtyperright' env expr (Set [])

/// Reaction speed must be above zero, left hand side must not be empty
let private reactiontyper env reaction =
    match reaction with
    (_, _, speed) when speed <= 0.0 -> 
        Error [sprintf "Reaction speed %f cannot be 0 or below" speed]
    | ([], _, _)
    | ([ExprSpecies.Null], _, _) -> Error [sprintf "Attempt at immaculate conception of a species"]
    | (expr1, expr2, _) -> 
        exprtyperleft env expr1 
        |> Result.bind (fun env -> exprtyperright env expr2)

/// Modules should not have lhs species appearing in the target
/// and target species can only be mutated once
let private moduletyper env module' =
    match module' with
    | ModuleS.Ld(sp1, target)
    | ModuleS.Sqrt(sp1, target)
    | ModuleS.Cmp(sp1, target) ->
        let env = resolvereference sp1 env
        if sp1 = target then
            Error [sprintf "Same value %s in both sides of expression" sp1]
        else
            addreference target env |> Ok
            |> Result.bind (checkmutation target)
    | ModuleS.Add(sp1, sp2, target)
    | ModuleS.Sub(sp1, sp2, target)
    | ModuleS.Mul(sp1, sp2, target)
    | ModuleS.Div(sp1, sp2, target) ->
        let env = 
            resolvereference sp1 env 
            |> resolvereference sp2
        if sp2 = target then
            Error [sprintf "Same value %s in all sides of expression" sp2]   
        else if sp1 = target then
            Error [sprintf "Same value %s in all sides of expression" sp1]      
        else
            addreference target env |> Ok
            |> Result.bind (checkmutation target)

let rec private commandtyper env command =
    match command with
        | Reaction(reaction) -> reactiontyper env reaction
        | Module(module') -> moduletyper env module'
        | Condition(cond) -> conditiontyper env cond

/// conditions can mutate mutually exclusive things
and private conditiontyper env condition =
    // TODO: we still have a problem if we mutate something outside of a condition
    // in the same step since that is not checked. It might make sense to just reject
    // all of those programs rather than try to hack my way around this in a more clever
    // way.
    let rec conditiontyper' env lst =
        match lst with
        [] -> Ok(env)
        | head :: tail -> 
            let env' = commandtyper env head
            env' |> Result.bind (fun env -> conditiontyper' env tail)
    match condition with
    | ConditionS.Gt(lst)
    | ConditionS.Ge(lst)
    | ConditionS.Eq(lst)
    | ConditionS.Lt(lst)
    | ConditionS.Le(lst) ->
        let mutables = env.Mutated
        conditiontyper' env lst
        |> Result.bind (fun env -> Ok {env with Mutated = mutables})


/// Within each step, species can be mutated only once
let rec private steptyper (env, steps) =
    let rec steptyper' env steps = 
        match steps with 
        [] -> Ok(env)
        | head :: tail -> 
            commandtyper env head
            |> Result.bind (fun env -> steptyper' env tail)
    steptyper' env steps

/// Concentrations must not include multiple definitions
/// Right hand side value must be a number or a "constant" that
/// has to be evaluated before we run the program
let private conctyper env conc =
    match conc with
    (concName, ValueS.Literal(constName)) -> 
        if env.Species.Contains concName then
            Error [sprintf "Duplicate definition of %s" concName]
        else if env.Consts.Contains concName then
            Error [sprintf "Constant value %s cannot be used as species" concName]        
        else if env.Species.Contains constName then
            Error [sprintf "Using species %s as a const value" constName]
        else
            Ok {env with Species = env.Species.Add concName; Consts = env.Consts.Add constName }
    | (concName, ValueS.Number(number)) ->
        if number < 0.0 then
            Error [sprintf "Number %f is not positive" number]
        else if env.Species.Contains concName then
            Error [sprintf "Duplicate definition of %s" concName]
        else if env.Consts.Contains concName then
            Error [sprintf "Constant value %s cannot be used as species" concName]
        else
            Ok {env with Species = env.Species.Add concName; Consts = env.Consts }    

let rec private roottyper (env, rootlist): Result<TypingEnv', TypeError list>  =
    let env = {env with Mutated = Set []}
    match rootlist with
    | [] -> Ok(env)
    | head :: tail -> 
            match head with
            RootS.Conc(conc) -> 
                let result = conctyper env conc
                result |> Result.bind (fun state -> roottyper (state, tail))
            | RootS.Step(step) ->
                let result = steptyper (env, step)
                result |> Result.bind (fun state -> roottyper (state, tail))

let rec private crntyper env crn : Result<TypingEnv', TypeError list> =
    let result = roottyper (env, crn)
    match result with
    Ok env' -> checkdangling env'
    | Error err -> Error err 

/// Typecheck the CRN to ensure that it is valid. Since everything in a CRN is global
/// This returns the same AST combined with a typing environment that has a list of all
/// species and constant values that must be evaluated before running the CRN.
let typecheck untypedast: Result<TypedAST, TypeError list> =
    let env: TypingEnv' = { Species  = Set [] 
                            Consts   = Set [] 
                            Dangling = Set []
                            Mutated  = Set [] }
    match crntyper env untypedast with
    | Ok(env) -> Ok(untypedast, {Species = env.Species; Consts = env.Consts })
    | Error e -> 
        List.map (printf "%s") e |> ignore
        Error e