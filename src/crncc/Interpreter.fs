module CRN.Interpreter
open CRN.AST
type State = Map<string, float>

let stepModule (oldstate:State) newstate cmp =
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


let rec step oldstate newstate cmp =
    function
    | [] -> (newstate, cmp)
    | Reaction x::tail ->   let Reaction (reactant, products, speed) = x
                            step oldstate newstate cmp tail
    | Module x::tail -> let (state, newcmp) = stepModule oldstate newstate cmp x
                        step oldstate state newcmp tail
    | Condition x::tail -> let (state, newcmp) = stepCondition oldstate newstate cmp x
                           step oldstate state newcmp tail
and stepCondition (oldstate:State) newstate cmp x =
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
let initial program env = 
    List.fold (fun s0 x -> match x with
                           | RootS.Conc (x, ValueS.Number y) -> Map.add x y s0
                           | RootS.Conc (x, ValueS.Literal y) -> failwith "variables not handled for conc"
                           | _ -> s0) Map.empty program
let interpreter program =
    let s0 = initial program
    let rec seq state cmp program =
        match program with
        | RootS.Step x::xs -> let (news, newcmp) = step state state cmp x
                              yield news
                              yield! seq news newcmp xs 
    seq s0 (ValueS "","") program
    