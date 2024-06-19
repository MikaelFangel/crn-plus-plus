module CRN.Simulator
open CRN.AST

type A = 
        | Mul of A * A
        | Add of A * A
        | Num of float
        | Var of string
let rec count x =
    function
    | ExprS.Expr [] -> 0
    | ExprS.Expr (Species y::ys) when x=y -> 1+ (count x (ExprS.Expr ys))
    | ExprS.Expr (y::ys) -> count x (ExprS.Expr ys)

let rec mulreac =
    function 
    | ExprS.Expr [Species y] -> Var y
    | ExprS.Expr ((Species y)::ys) -> Mul (Var y, mulreac (ExprS.Expr ys))
    
    
let rec odeof species =
    function
    | [] -> Num 0
    | ReactionS.Reaction (x,y,k)::xs -> let before = count species x
                                        let after = count species y
                                        let change = after-before
                                        if change = 0 then odeof species xs 
                                        else let reactants = mulreac y
                                             Add(Mul (Num ((float change)*k), reactants), odeof species xs) //change*k*reactants+ rest
let rxnstoodesys rxnsys =
    let (rxns, env) = rxnsys  
    Set.fold (fun acc x -> (odeof x rxns)::acc) [] (env.Species)
    