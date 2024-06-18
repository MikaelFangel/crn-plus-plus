module CRN.Interpreter
open CRN.AST
type State = Map<string, float>

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
    | ModuleS.Cmp (x,y) ->  let xconc = Map.find x oldstate
                            let yconc = Map.find y oldstate
                            (newstate, (xconc,yconc))


let rec private step oldstate newstate cmp =
    function
    | [] -> (newstate, cmp)
    | Reaction x::tail ->   let Reaction (reactant, products, speed) = x
                            step oldstate newstate cmp tail
    | Module x::tail -> let (state, newcmp) = stepModule oldstate newstate cmp x
                        step oldstate state newcmp tail
    | Condition x::tail -> let (state, newcmp) = stepCondition oldstate newstate cmp x
                           step oldstate state newcmp tail
and private stepCondition (oldstate:State) newstate cmp x =
    match (x,cmp) with 
    | (ConditionS.Gt cmd,(x,y)) ->  if x>y then step oldstate newstate cmp cmd
                                    else (newstate, cmp)
    | (ConditionS.Ge cmd,(x,y)) ->  if x>=y then step oldstate newstate cmp cmd
                                    else (newstate, cmp)
    | (ConditionS.Eq cmd,(x,y)) ->  if x=y then step oldstate newstate cmp cmd
                                    else (newstate, cmp)
    | (ConditionS.Lt cmd,(x,y)) ->  if x<y then step oldstate newstate cmp cmd
                                    else (newstate, cmp)
    | (ConditionS.Le cmd,(x,y)) ->  if x<=y then step oldstate newstate cmp cmd
                                    else (newstate, cmp)
let private initial program constmap = 
    let (crn, env) = program
    let concmap = Set.fold (fun map s -> Map.add s 0.0 map) Map.empty env.Species
    List.fold (fun s x ->   let (concs, steps) = s
                            match x with
                            | RootS.Conc (x, ValueS.Number y) -> (Map.add x y concs, steps)
                            | RootS.Conc (x, ValueS.Literal y) -> (Map.add x (Map.find y constmap) concs, steps)
                            | RootS.Step x -> (concs, x::steps)) (concmap, []) crn
let generate s0 steps =
    (s0,(0.0,0.0), 0)
    |> Seq.unfold (fun state ->
        let (state, cmp, count) = state
        if count = 0 then  // overflow
            None
        else
            let len = List.length steps
            let (state', cmp') = step state state cmp steps[count%len]
            Some (state,(state', cmp', count+1) ))

let interpreter constmap program =

    let (s0, steps) = initial program constmap
    let rec gen state cmp crn =
        match crn with
        | RootS.Step x::xs -> let (news, newcmp) = step state state cmp x
                              state::gen news newcmp xs              
        | _::xs -> gen state cmp xs
        | [] ->  []
    
    generate s0 steps 
    

