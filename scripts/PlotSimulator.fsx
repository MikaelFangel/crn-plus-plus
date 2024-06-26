#r "nuget: Plotly.NET, 4.2.0"

#load "../src/crncc/AST.fs"
#load "../src/crncc/Simulator.fs"
#load "../src/crncc/Interpreter.fs"
#load "../src/CRN.Visualization/Library.fs"
open CRN.Simulator
open CRN.AST

//Load
let ldreaction = 
        [ReactionS.Reaction(
            (
                [("A")]), 
            (
                [("A")
                 ("B")
                ]), 1.0)
         ReactionS.Reaction(
            (
                [("B")]
            ),
            ([]), 1.0
         )]
//Addition
let addreaction = 
        [ReactionS.Reaction(
            (
                [("A")]), 
            (
                [("A")
                 ("C")
                ]), 1.0)
         ReactionS.Reaction(
            (
                [("B")]), 
            (
                [("B")
                 ("C")
                ]), 1.0)
         ReactionS.Reaction(
            (
                [("C")]
            ),
            ([]), 1.0
         )]

//Subtraction
let subreaction = 
        [ReactionS.Reaction(
            (
                [("A")]), 
            (
                [("A")
                 ("C")]), 1.0)
         ReactionS.Reaction(
            (
                [("B")]), 
            (
                [("B")
                 ("H")]), 1.0)
         ReactionS.Reaction(
            (
                [("C")]),
            ([]), 1.0)
         ReactionS.Reaction(
            (
                [("C")
                 ("H")]),
            (
                []), 1.0)
         ]

//multiplication
let mulreaction = 
        [ReactionS.Reaction(
            (
                [("A")
                 ("B")]), 
            (
                [("A")
                 ("B")
                 ("C")]), 1.0)
         ReactionS.Reaction(
            (
                [("C")]
            ),
            ([]), 1.0
         )]

//division
let divreaction = 
        [ReactionS.Reaction(
            (
                [("A")]), 
            (
                [("A")
                 ("C")]), 1.0)
         ReactionS.Reaction(
            (
                [("B")
                 ("C")]
            ),
            (
                [("B")]), 1.0
         )]

//square root
let sqrtreaction = 
        [ReactionS.Reaction(
            (
                [("A")]), 
            (
                [("A")
                 ("B")]), 1.0)
         ReactionS.Reaction(
            (
                [("B")
                 ("B")]
            ),
            (
                []), 0.5
         )]

//clock simulator
let clock3reaction = 
        [ReactionS.Reaction(
            (
                [("A")
                 ("B")]), 
            (
                [("B")
                 ("B")]), 1.0)
         ReactionS.Reaction(
            (
                [("B")
                 ("C")]
            ),
            (
                [("C")
                 ("C")]), 1.0)
         ReactionS.Reaction(
            (
                [("C")
                 ("A")]
            ),
            (
                [("A")
                 ("A")]), 1.0
        )]
let clock9reaction = 
        [ReactionS.Reaction(
            (
                [("X1")
                 ("X2")]), 
            (
                [("X2")
                 ("X2")]), 1.0)
         ReactionS.Reaction(
            (
                [("X2")
                 ("X3")]
            ),
            (
                [("X3")
                 ("X3")]), 1.0)
         ReactionS.Reaction(
            (
                [("X3")
                 ("X4")]
            ),
            (
                [("X4")
                 ("X4")]), 1.0)
         ReactionS.Reaction(
            (
                [("X4")
                 ("X5")]), 
            (
                [("X5")
                 ("X5")]), 1.0)
         ReactionS.Reaction(
            (
                [("X5")
                 ("X6")]
            ),
            (
                [("X6")
                 ("X6")]), 1.0)
         ReactionS.Reaction(
            (
                [("X6")
                 ("X7")]
            ),
            (
                [("X7")
                 ("X7")]), 1.0)
         ReactionS.Reaction(
            (
                [("X7")
                 ("X8")]), 
            (
                [("X8")
                 ("X8")]), 1.0)
         ReactionS.Reaction(
            (
                [("X8")
                 ("X9")]
            ),
            (
                [("X9")
                 ("X9")]), 1.0)
         ReactionS.Reaction(
            (
                [("X9")
                 ("X1")]
            ),
            (
                [("X1")
                 ("X1")]), 1.0)
        ]
//let initial = Map.ofList ["X1", 1.0-8.0*(0.1e-11); "X2", 0.1e-11; "X3", 0.1e-11; "X4", 0.1e-11; "X5", 0.1e-11; "X6", 0.1e-11; "X7", 0.1e-11; "X8", 0.1e-11; "X9", 0.1e-11] 
(*
for a in 0..60 do
    let a = float a
    let initial = Map["A", a; "B", 0.0]
    let (names, states) = (solveODEFast initial 0.01 sqrtreaction)
    Seq.map (CRN.Simulator.ArraytoMap names) states 
    |> Seq.take 2000 |> Seq.last |> Map.find "B"
    |> fun b -> abs(sqrt a - b)|> printf "%A,"
*)
(*
for a in 0..60 do
    printf "%A " a
    let a = float a
    for b in 0..60 do
        let b = float b
        let initial = Map["A", a; "B", b; "C", 0.0; "H", 0.0]
        let (names, states) = (solveODEFast initial 0.01 subreaction)
        Seq.map (CRN.Simulator.ArraytoMap names) states 
        |> Seq.take 2000 |> Seq.last |> Map.find "C"
        |> fun c -> let cmp =a-b 
                    if cmp <0 then  abs (0.0 - c) else abs (cmp - c)
                     |> printf "%A,"
    printfn ""
*)

let initial = Map["A", 1.304408512; "B", 0.02074312655; "C", 0.0]
let (names, states) = (solveODEFast initial 0.01 divreaction)
Seq.map (CRN.Simulator.ArraytoMap names) states
|> CRN.Visualization.Library.plotState (fun s -> seq [ "C" ] |> Seq.contains s) 20000 
