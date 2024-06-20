module CRN.Typechecker

open AST

/// Type errors are a series of strings
type TypeError = string

type private Condition =
    | G
    | E
    | L

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
        ///
        Conditions: Set<Condition>
    }

let private unresolvedReferenceError name = $"Unresolved reference: {name}"

let private multipleMutationError name =
    $"Species {name} mutated multiple times in step"

let private reactionSpeedError speed = $"Reaction speed {speed} is below 0.0"

let private immaculateConceptionError _ =
    $"Attempt at immaculate conception of a species"

let private sameValueInBothSidesError name =
    $"Same value {name} in both sides of expression"

let private intersectingConditionsError _ =
    "Intersecting conditions in condition expression"

let private conditionOrElseError _ =
    "Conditions cannot exist at the same level as modules and reactions"

let private duplicateDefinitionError name = $"Duplicate definition of {name}"

let private constantValueAsSpeciesError name =
    $"Constant value {name} cannot be used as species"

let private speciesAsConstantValueError name =
    $"Species {name} cannot be used as constant value"

let private concentrationError concentration =
    $"Concentration {concentration} is below 0.0"

/// Add a species reference to the environment
let private addreference name env : TypingEnv' =
    { env with
        Species = env.Species.Add name }

/// Resolve a reference to the environment
let private resolvereference name env =
    if env.Species.Contains name then
        env
    else
        { env with
            Dangling = env.Dangling.Add name }

/// Check if environment has any dangling species
let private checkdangling env =
    if env.Dangling.IsSubsetOf env.Species then
        Ok env
    else
        Set.difference env.Dangling env.Species
        |> Set.toList
        |> List.map unresolvedReferenceError
        |> Error

/// Check if something has been previously mutated
let private checkmutation name env =
    if env.Mutated.Contains name then
        [ multipleMutationError name ] |> Error
    else
        Ok
            { env with
                Mutated = env.Mutated.Add name }

/// left hand side values must be defined
let rec private exprtyperleft env expr =
    match expr with
    | [] -> Ok(env)
    | Species(head) :: tail ->
        let env = resolvereference head env
        exprtyperleft env tail


/// right hand side values can be created
let rec private exprtyperright env expr =
    match expr with
    | [] -> Ok(env)
    | Species(head) :: tail ->
        let env' = addreference head env
        exprtyperright env' tail

/// Reaction speed must be above zero, left hand side must not be empty
let private reactiontyper env reaction =
    match reaction with
    | ReactionS.Reaction(_, _, speed) when speed <= 0.0 -> [ reactionSpeedError speed ] |> Error
    | ReactionS.Reaction(ExprS.Expr [], _, _) -> [ immaculateConceptionError () ] |> Error
    | ReactionS.Reaction(ExprS.Expr expr1, ExprS.Expr expr2, _) ->
        exprtyperleft env expr1 |> Result.bind (fun env -> exprtyperright env expr2)

/// Modules should not have lhs species appearing in the target
/// and target species can only be mutated once
let private moduletyper env module' =
    match module' with
    | ModuleS.Ld(sp1, target)
    | ModuleS.Sqrt(sp1, target)
    | ModuleS.Cmp(sp1, target) ->
        let env = resolvereference sp1 env

        let result =
            if sp1 = target then
                [ sameValueInBothSidesError sp1 ] |> Error
            else
                Ok env

        result
        |> Result.bind (fun env -> addreference target env |> checkmutation target)
    | ModuleS.Add(sp1, sp2, target)
    | ModuleS.Sub(sp1, sp2, target)
    | ModuleS.Mul(sp1, sp2, target)
    | ModuleS.Div(sp1, sp2, target) ->
        let env = resolvereference sp1 env |> resolvereference sp2

        let result =
            if sp2 = target then
                [ sameValueInBothSidesError sp2 ] |> Error
            else if sp1 = target then
                [ sameValueInBothSidesError sp1 ] |> Error
            else
                Ok env

        result
        |> Result.bind (fun env -> addreference target env |> (checkmutation target))

/// conditions can mutate mutually exclusive things
/// Conditions within a step have to be mutually exclusive themselves
let rec private conditiontyper env condition =
    let covered =
        match condition with
        | ConditionS.Gt(_) -> Set.singleton G
        | ConditionS.Ge(_) -> Set.ofList [ G; E ]
        | ConditionS.Eq(_) -> Set.singleton E
        | ConditionS.Lt(_) -> Set.singleton L
        | ConditionS.Le(_) -> Set.ofList [ L; E ]

    let intersection = Set.intersect covered env.Conditions

    let res1 =
        if intersection.IsEmpty then
            let env' =
                { env with
                    Conditions = Set.union covered env.Conditions }

            Ok env'
        else
            [ intersectingConditionsError () ] |> Error

    let conditiontyper' env condition =
        match condition with
        | ConditionS.Gt(lst)
        | ConditionS.Ge(lst)
        | ConditionS.Eq(lst)
        | ConditionS.Lt(lst)
        | ConditionS.Le(lst) ->
            let mutables = env.Mutated
            steptyper env lst |> Result.bind (fun env -> Ok { env with Mutated = mutables })

    res1 |> Result.bind (fun env -> conditiontyper' env condition)

/// Within each step, species can be mutated only once
/// And if there are conditions, there can only be conditions
and private steptyper env steps =
    let rec steptyper' env steps =
        match steps with
        | [] -> Ok(env)
        | Reaction(reaction) :: tail -> reactiontyper env reaction |> Result.bind (fun env -> steptyper' env tail)
        | Module(m') :: tail -> moduletyper env m' |> Result.bind (fun env -> steptyper' env tail)
        | _ -> [ conditionOrElseError () ] |> Error

    let rec steptyper'' env steps =
        match steps with
        | [] -> Ok({ env with Conditions = Set.empty })
        | Condition(cond) :: tail -> conditiontyper env cond |> Result.bind (fun env -> steptyper'' env tail)
        | _ -> [ conditionOrElseError () ] |> Error

    match steps with
    | [] -> Ok(env)
    | Reaction(_) :: _
    | Module(_) :: _ -> steptyper' env steps
    | Condition(_) :: _ -> steptyper'' env steps

/// Concentrations must not include multiple definitions
/// Right hand side value must be a number or a "constant" that
/// has to be evaluated before we run the program
let private conctyper env conc =
    match conc with
    | ConcS.Conc(concName, ValueS.Literal(constName)) ->
        if env.Species.Contains concName then
            [ duplicateDefinitionError concName ] |> Error
        else if env.Consts.Contains concName then
            [ constantValueAsSpeciesError concName ] |> Error
        else if env.Species.Contains constName then
            [ speciesAsConstantValueError constName ] |> Error
        else
            Ok
                { env with
                    Species = env.Species.Add concName
                    Consts = env.Consts.Add constName }
    | ConcS.Conc(concName, ValueS.Number(number)) ->
        if number < 0.0 then
            [ concentrationError number ] |> Error
        else if env.Species.Contains concName then
            [ duplicateDefinitionError concName ] |> Error
        else if env.Consts.Contains concName then
            [ constantValueAsSpeciesError concName ] |> Error
        else
            Ok
                { env with
                    Species = env.Species.Add concName }

let rec private roottyper env rootlist : Result<TypingEnv', TypeError list> =
    let env = { env with Mutated = Set [] }

    match rootlist with
    | [] -> Ok(env)
    | head :: tail ->
        match head with
        | RootS.Conc(conc) ->
            let result = conctyper env conc
            result |> Result.bind (fun state -> roottyper state tail)
        | RootS.Step(step) ->
            let result = steptyper env step
            result |> Result.bind (fun state -> roottyper state tail)

let rec private crntyper env crn : Result<TypingEnv', TypeError list> =
    match crn with
    | CrnS.Crn(rootlist) ->
        let result = roottyper env rootlist

        match result with
        | Ok env' -> checkdangling env'
        | Error err -> Error err

/// Typecheck the CRN to ensure that it is valid. Since everything in a CRN is global
/// This returns the same AST combined with a typing environment that has a list of all
/// species and constant values that must be evaluated before running the CRN.
let typecheck (untypedast: UntypedAST) : Result<TypedAST, TypeError list> =
    let env: TypingEnv' =
        { Species = Set.empty
          Consts = Set.empty
          Dangling = Set.empty
          Mutated = Set.empty
          Conditions = Set.empty }

    match crntyper env untypedast with
    | Ok(env) ->
        Ok(
            untypedast,
            { Species = env.Species
              Consts = env.Consts }
        )
    | Error e ->
        List.map (printf "%s") e |> ignore
        Error e
