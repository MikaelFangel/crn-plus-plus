module CRN.Interpreter
open CRN.AST

exception MissingConst of string

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
    | [] -> Ok (newstate, cmp)
    | Reaction x::tail ->  Error ("Found a reaction "+ (string (Reaction x)))
    | Module x::tail -> let (state, newcmp) = stepModule oldstate newstate cmp x
                        step oldstate state newcmp tail
    | Condition x::tail -> stepCondition oldstate newstate cmp x
                           |> Result.bind (fun (state,newcmp) -> step oldstate state newcmp tail)
and private stepCondition (oldstate:State) newstate cmp cond =
    let (x,y) = cmp
    let xfind = Map.tryFind x oldstate
    let yfind = Map.tryFind y oldstate
    match (xfind,yfind) with
    | (None,_) -> Error ("Could not find flag "+ x) 
    | (_,None) -> Error ("Could not find flag "+ y) 
    | (Some xconc, Some yconc) ->   match cond with 
                                    | ConditionS.Gt cmd ->  if xconc>yconc then step oldstate newstate cmp cmd
                                                            else Ok (newstate, cmp)
                                    | ConditionS.Ge cmd ->  if xconc>=yconc then step oldstate newstate cmp cmd
                                                            else Ok (newstate, cmp)
                                    | ConditionS.Eq cmd ->  if xconc=yconc then step oldstate newstate cmp cmd
                                                            else Ok (newstate, cmp)
                                    | ConditionS.Lt cmd ->  if xconc<yconc then step oldstate newstate cmp cmd
                                                            else Ok (newstate, cmp)
                                    | ConditionS.Le cmd ->  if xconc<=yconc then step oldstate newstate cmp cmd
                                                            else Ok (newstate, cmp)

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
    let len = List.length steps
    (s0,("0","0"), 0)
    |> Seq.unfold (fun st ->
        let (state, cmp, count) = st
        if count < 0 then  // overflow
            None
        else
            match step state state cmp steps[count%len] with
            | Error a -> None
            | Ok (state',cmp') -> Some (state,(state', cmp', count+1)))

let generate' s0 steps=
    let len = List.length steps
    (Ok s0,("0","0"), 0)
    |> Seq.unfold (fun state ->
        match state with 
        | (Error a,_,_) -> Some (Error a,(Error a,("0","0"), 0))
        | (Ok map, cmp, count) ->
            match step map map cmp steps[count%len] with
            | Error a -> Some (Error a,(Error a,("0","0"), 0))
            | Ok (newstate, newcmp) ->
                Some (Ok map,(Ok newstate, newcmp, count+1)))

let interpreter constmap (program:TypedAST) =
    try
        let (s0, steps) = initial program constmap
        Ok (generate s0 steps)
    with
        | MissingConst a -> Result.Error ["Could not find "+a]
        | _ -> Result.Error ["Unknown error"]
    
    //let rec gen state cmp crn =
    //    match crn with
    //    | RootS.Step x::xs -> let (news, newcmp) = step state state cmp x
    //                          state::gen news newcmp xs              
    //    | _::xs -> gen state cmp xs
    //    | [] ->  []
    
    
    

