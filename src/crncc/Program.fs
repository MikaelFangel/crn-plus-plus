module CRN.Program

open CRN.Simulator
open CRN.AST

let [<EntryPoint>] main _ =
    let initial = Map.ofList ["A", 3.0; "B", 4.0; "C", 0.0]
    let reactions = 
        [ReactionS.Reaction(
            ExprS.Expr(
                [Species("A")
                 Species("B")]), 
            ExprS.Expr(
                [Species("A")
                 Species("B")
                 Species("C")]), 1.0)
         ReactionS.Reaction(
            ExprS.Expr(
                [Species("C")]
            ),
            ExprS.Expr([]), 1.0
         )]
    let states = Seq.take 50 (solveODE initial 0.1 reactions)
    for state in states do
        printfn "%A" state
    0
