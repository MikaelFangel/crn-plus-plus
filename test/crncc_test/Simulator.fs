module CRNTest.Simulator

open CRN.AST

open Xunit

//multiplication
let mulreaction = 
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

let printresult names results =
    List.zip names (Array.toList results)
    |> List.map (fun name value -> printf $"{name}: {value}") |> ignore
    printfn ""


[<Fact>]
let ``Simulator Test`` () =
    let initial = Map.ofList ["A", 3.0; "B", 4.0; "C", 0.0] 

    let names, results = (CRN.Simulator.solveODEFast initial 0.1 mulreaction)
    let printwithnames = printresult names
    let all = 
        results 
        |> Seq.take 500000 
        |> Seq.toList 

    //all |> List.map printwithnames
    ()
