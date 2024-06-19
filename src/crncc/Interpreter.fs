module CRN.Interpreter
open CRN.AST
type State = Map<string, float>
exception MissingConst of string
exception ReactionEncountered of string

let private stepModule (oldstate:State) newstate cmp =
    function
    | ModuleS.Ld (x,y) -> let xconc = Map.find x oldstate
                          (Map.add y xconc newstate, cmp)
    | ModuleS.Add (x,y,z) -> let xconc = Map.find x oldstate
                             let yconc = Map.find y oldstate
                             (Map.add z (xconc+yconc) newstate, cmp)
    | ModuleS.Sub (x,y,z) ->    let xconc = Map.find x oldstate
                                let yconc = Map.find y oldstate
                                if xconc > yconc then
                                    (Map.add z (xconc-yconc) newstate, cmp)
                                    else (Map.add z 0 newstate, cmp)
    | ModuleS.Mul (x,y,z) ->    let xconc = Map.find x oldstate
                                let yconc = Map.find y oldstate
                                (Map.add z (xconc*yconc) newstate, cmp)
    | ModuleS.Div (x,y,z) ->    let xconc = Map.find x oldstate
                                let yconc = Map.find y oldstate
                                (Map.add z (xconc/yconc) newstate, cmp)
    | ModuleS.Sqrt (x,y) -> let xconc = Map.find x oldstate
                            (Map.add y (sqrt xconc) newstate, cmp)
    | ModuleS.Cmp (x,y) ->  (newstate, (x,y))

let rec private step oldstate newstate cmp =
    function
    | [] -> (newstate, cmp)
    | Reaction x::tail ->  raise (ReactionEncountered "Found a reaction")
    | Module x::tail -> let (state, newcmp) = stepModule oldstate newstate cmp x
                        step oldstate state newcmp tail
    | Condition x::tail -> let (state, newcmp) = stepCondition oldstate newstate cmp x
                           step oldstate state newcmp tail
and private stepCondition (oldstate:State) newstate cmp cond =
    let (x,y) = cmp
    let xconc = Map.find x oldstate
    let yconc = Map.find y oldstate
    match cond with 
    | ConditionS.Gt cmd ->  if xconc>yconc then step oldstate newstate cmp cmd
                            else (newstate, cmp)
    | ConditionS.Ge cmd ->  if xconc>=yconc then step oldstate newstate cmp cmd
                            else (newstate, cmp)
    | ConditionS.Eq cmd ->  if xconc=yconc then step oldstate newstate cmp cmd
                            else (newstate, cmp)
    | ConditionS.Lt cmd ->  if xconc<yconc then step oldstate newstate cmp cmd
                            else (newstate, cmp)
    | ConditionS.Le cmd ->  if xconc<=yconc then step oldstate newstate cmp cmd
                            else (newstate, cmp)

let private initial program constmap = 
    let (CrnS.Crn crn, env) = program
    let concmap = Set.fold (fun map s -> Map.add s 0.0 map) Map.empty env.Species
    List.foldBack (fun x s ->   let (concs, steps) = s
                                match x with
                                | RootS.Conc ( ConcS.Conc ( x, ValueS.Number y)) -> (Map.add x y concs, steps)
                                | RootS.Conc ( ConcS.Conc ( x, ValueS.Literal y)) ->  match Map.tryFind y constmap with
                                                                                      | None -> raise (MissingConst y)
                                                                                      | Some a -> (Map.add x a concs, steps)                               
                                | RootS.Step x -> (concs, x::steps)) crn (concmap, [])

let generate s0 steps =
    (s0,("0","0"), 0)
    |> Seq.unfold (fun st ->
        let (state, cmp, count) = st
        if count < 0 then  // overflow
            None
        else
            let len = List.length steps
            let (state', cmp') = step state state cmp steps[count%len]
            Some (state,(state', cmp', count+1)))

let interpreter constmap (program:TypedAST) =
    try
        let (s0, steps) = initial program constmap
        printfn "Steps %A" steps
        let res = generate s0 steps 
        Result.Ok res
    with
        | MissingConst a -> Result.Error ["Could not find "+a]
        | ReactionEncountered a -> Result.Error ["Could not find "+a]
        | _ -> Result.Error ["Could not intepret"]
    
    //let rec gen state cmp crn =
    //    match crn with
    //    | RootS.Step x::xs -> let (news, newcmp) = step state state cmp x
    //                          state::gen news newcmp xs              
    //    | _::xs -> gen state cmp xs
    //    | [] ->  []
    
    
    

