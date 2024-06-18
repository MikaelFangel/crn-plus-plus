module CRN.Simulator
open CRN.AST

type A = 
        | Mul of A * A
        | Add of A * A
        | Num of float
        | Var of string
let rec count x =
    function
    | [] -> 0
    | y::ys when x=y -> 1+ (count x ys)
    | y::ys -> count x ys

let rec math species =
    function
    | [] -> Num 0
    | Reaction (x,y,k)::xs ->   let before = count species x
                                let after = count species y
                                let change = after-before
                                if change = 0 then math species xs 
                                else Mul (Num ((float change)*k), Var "R") //change*k*reactants
let rxnstoodesys rxnsys =
    let (rxns, env) = rxnsys  
    Set.fold (fun acc x -> x::acc) [] (env.Species)
    rxns
