module CRN.Program

open CRN.Simulator
open CRN.AST

[<EntryPoint>]
let main _ =
    let initial = Map.ofList [ "A", 9.0; "B", 0.0 ]

    let reactions =
        [ ReactionS.Reaction(([ Species("A") ]), ([ Species("A"); Species("B") ]), 1.0)
          ReactionS.Reaction(([ Species("B"); Species("B") ]), ([]), 0.5) ]

    let states = Seq.take 50 (solveODE initial 0.1 reactions)

    for state in states do
        printfn "%A" state

    0
