module CRN.Program

open CRN.Simulator
open CRN.AST

let [<EntryPoint>] main _ =
    let initial = Map.ofList ["A", 9.0; "B", 0.0]
    let reactions = 
        [ReactionS.Reaction(
            ExprS.Expr(
                [Species("A")]), 
            ExprS.Expr(
                [Species("A")
                 Species("B")]), 1.0)
         ReactionS.Reaction(
            ExprS.Expr(
                [Species("B")
                 Species("B")]
            ),
            ExprS.Expr([]), 0.5
         )]
    let states = Seq.take 50 (solveODE initial 0.1 reactions)
    for state in states do
        printfn "%A" state
    0
