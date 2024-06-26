module CRNTest.Simulator

open CRN.AST

open Xunit

//multiplication
let mulreaction =
    [ ReactionS.Reaction([ "A"; "B" ], [ "A"; "B"; "C" ], 1.0)
      ReactionS.Reaction([ "C" ], [], 1.0) ]

let printresult names results =
    List.zip names (Array.toList results)
    |> List.map (fun name value -> printf $"{name}: {value}")
    |> ignore

    printfn ""


[<Fact>]
let ``Simulator Test`` () =
    let initial = Map.ofList [ "A", 3.0; "B", 4.0; "C", 0.0 ]

    let names, results = (CRN.Simulator.solveODEFast initial 0.1 mulreaction)
    let printwithnames = printresult names
    let all = results |> Seq.take 500000 |> Seq.toList

    //all |> List.map printwithnames
    ()
