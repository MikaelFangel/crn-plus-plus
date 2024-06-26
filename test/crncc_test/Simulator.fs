module CRNTest.Simulator
open Xunit
open CRN.AST


open FsCheck.Xunit
open FsCheck

//Load
let ldreaction = 
        [ReactionS.Reaction((["A"]), (["A"; "B"]), 1.0)
         ReactionS.Reaction((["B"]), ([]), 1.0
         )]
//Addition
let addreaction = 
        [ReactionS.Reaction((["A"]), (["A"; "C"]), 1.0)
         ReactionS.Reaction((["B"]), (["B"; "C"]), 1.0)
         ReactionS.Reaction((["C"]), ([]), 1.0
         )]

//Subtraction
let subreaction = 
        [ReactionS.Reaction((["A"]), (["A"; "C"]), 1.0)
         ReactionS.Reaction((["B"]), (["B"; "H"]), 1.0)
         ReactionS.Reaction((["C"]), ([]), 1.0)
         ReactionS.Reaction((["C"; "H"]), ([]), 1.0)
         ]

//division
let divreaction = 
        [ReactionS.Reaction((["A"]), (["A"; "C"]), 1.0)
         ReactionS.Reaction((["B"; "C"]), (["B"]), 1.0
         )]

//square root
let sqrtreaction = 
        [ReactionS.Reaction((["A"]), (["A"; "B"]), 1.0)
         ReactionS.Reaction((["B"; "B"]), ([]), 0.5
         )]


//multiplication
let mulreaction =
    [ ReactionS.Reaction([ "A"; "B" ], [ "A"; "B"; "C" ], 1.0)
      ReactionS.Reaction([ "C" ], [], 1.0) ]
(*
let printresult names results =
    List.zip names (Array.toList results)
    |> List.map (fun name value -> printf $"{name}: {value}")
    |> ignore

    printfn ""


[<Fact>]
let ``Simulator Test`` () =
    let initial = Map.ofList [ "A", 3.0; "B", 4.0; "C", 0.0 ]

    let names, results = (CRN.Simulator.solveODEFast initial 0.01 mulreaction)
    let printwithnames = printresult names
    let all = results |> Seq.take 500000 |> Seq.toList
*)


[<Property>] 
let ``Fast Simulator: multiplication`` (a: NormalFloat, b:NormalFloat) =
    let a = abs (float a)
    let b = abs (float b)
    let expect = a * b
    let initial = Map.ofList [ "A", a; "B", b; "C", 0.0 ]
    let names, states = CRN.Simulator.solveODEFast initial 0.01 mulreaction 
    Seq.map (CRN.Simulator.ArraytoMap names) states
    |> Seq.take 2000 |> Seq.exists (fun map -> abs (Map.find "C" map - expect) <= 0.5)


[<Property>] 
let ``Fast Simulator: addition`` (a: NormalFloat, b:NormalFloat) =
    let a = abs (float a)
    let b = abs (float b)
    let expect = a + b
    let initial = Map.ofList [ "A", a; "B", b; "C", 0.0]
    let names, states = CRN.Simulator.solveODEFast initial 0.01 addreaction 
    Seq.map (CRN.Simulator.ArraytoMap names) states
    |> Seq.take 2000 |> Seq.exists (fun map -> abs (Map.find "C" map - expect) <= 0.5)

[<Property>] 
let ``Fast Simulator: subtraction`` (a: NormalFloat, b:NormalFloat) =
    let a = abs (float a)
    let b = abs (float b)
    let expect = if a - b < 0 then 0.0 else (a - b)
    let initial = Map.ofList [ "A", a; "B", b; "C", 0.0; "H", 0.0]
    let names, states = CRN.Simulator.solveODEFast initial 0.01 subreaction 
    Seq.map (CRN.Simulator.ArraytoMap names) states
    |> Seq.take 2000 |> Seq.exists (fun map -> abs (Map.find "C" map - expect) <= 0.5)

[<Property>] 
let ``Fast Simulator: load`` (a: NormalFloat) =
    let a = abs (float a)
    let initial = Map.ofList [ "A", a; "B", 0.0]
    let names, states = CRN.Simulator.solveODEFast initial 0.01 ldreaction 
    Seq.map (CRN.Simulator.ArraytoMap names) states
    |> Seq.take 2000 |> Seq.exists (fun map -> abs (Map.find "B" map - a) <= 0.5)

[<Property>] 
let ``Fast Simulator: square root`` (a: NormalFloat) =
    let a = abs (float a)
    let expect = sqrt a
    let initial = Map.ofList [ "A", a; "B", 0.0]
    let names, states = CRN.Simulator.solveODEFast initial 0.01 sqrtreaction 
    Seq.map (CRN.Simulator.ArraytoMap names) states
    |> Seq.take 2000 |> Seq.exists (fun map -> abs (Map.find "B" map - expect) <= 0.5)

[<Property>] 
let ``Fast Simulator: division`` (a: NormalFloat, b:NormalFloat) =
    let a = abs (float a)
    let b = abs (float b)
    let expect = if b = 0 then 0.0 else a / b
    let initial = Map.ofList [ "A", a; "B", b; "C", 0.0]
    let names, states = CRN.Simulator.solveODEFast initial 0.01 divreaction 
    Seq.map (CRN.Simulator.ArraytoMap names) states
    |> Seq.take 50000 |> Seq.exists (fun map -> abs (Map.find "C" map - expect) <= 0.5)





[<Property>] 
let ``Simulator: multiplication`` (a: NormalFloat, b:NormalFloat) =
    let a = abs (float a)
    let b = abs (float b)
    let expect = a * b
    let initial = Map.ofList [ "A", a; "B", b; "C", 0.0 ]
    CRN.Simulator.solveODE initial 0.01 mulreaction 
    |> Seq.take 2000 |> Seq.exists (fun map -> abs (Map.find "C" map - expect) <= 0.5)


[<Property>] 
let ``Simulator: addition`` (a: NormalFloat, b:NormalFloat) =
    let a = abs (float a)
    let b = abs (float b)
    let expect = a + b
    let initial = Map.ofList [ "A", a; "B", b; "C", 0.0]
    CRN.Simulator.solveODE initial 0.01 addreaction 
    |> Seq.take 2000 |> Seq.exists (fun map -> abs (Map.find "C" map - expect) <= 0.5)

[<Property>] 
let ``Simulator: subtraction`` (a: NormalFloat, b:NormalFloat) =
    let a = abs (float a)
    let b = abs (float b)
    let expect = if a - b < 0 then 0.0 else (a - b)
    let initial = Map.ofList [ "A", a; "B", b; "C", 0.0; "H", 0.0]
    CRN.Simulator.solveODE initial 0.01 subreaction 
    |> Seq.take 2000 |> Seq.exists (fun map -> abs (Map.find "C" map - expect) <= 0.5)

[<Property>] 
let ``Simulator: load`` (a: NormalFloat) =
    let a = abs (float a)
    let initial = Map.ofList [ "A", a; "B", 0.0]
    CRN.Simulator.solveODE initial 0.01 ldreaction 
    |> Seq.take 2000 |> Seq.exists (fun map -> abs (Map.find "B" map - a) <= 0.5)

[<Property>] 
let ``Simulator: square root`` (a: NormalFloat) =
    let a = abs (float a)
    let expect = sqrt a
    let initial = Map.ofList [ "A", a; "B", 0.0]
    CRN.Simulator.solveODE initial 0.01 sqrtreaction 
    |> Seq.take 2000 |> Seq.exists (fun map -> abs (Map.find "B" map - expect) <= 0.5)

[<Property>] 
let ``Simulator: division`` (a: NormalFloat, b:NormalFloat) =
    let a = abs (float a)
    let b = abs (float b)
    let expect = if b = 0 then 0.0 else a / b
    let initial = Map.ofList [ "A", a; "B", b; "C", 0.0]
    CRN.Simulator.solveODE initial 0.01 divreaction 
    |> Seq.take 50000 |> Seq.exists (fun map -> abs (Map.find "C" map - expect) <= 0.5)
