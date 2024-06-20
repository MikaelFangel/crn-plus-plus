#r "nuget: FParsec, 1.1.1"
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
            ExprS.Expr(
                [Species("A")]), 
            ExprS.Expr(
                [Species("A")
                 Species("B")
                ]), 1.0)
         ReactionS.Reaction(
            ExprS.Expr(
                [Species("B")]
            ),
            ExprS.Expr([]), 1.0
         )]
//Addition
let addreaction = 
        [ReactionS.Reaction(
            ExprS.Expr(
                [Species("A")]), 
            ExprS.Expr(
                [Species("A")
                 Species("C")
                ]), 1.0)
         ReactionS.Reaction(
            ExprS.Expr(
                [Species("B")]), 
            ExprS.Expr(
                [Species("B")
                 Species("C")
                ]), 1.0)
         ReactionS.Reaction(
            ExprS.Expr(
                [Species("C")]
            ),
            ExprS.Expr([]), 1.0
         )]

//Subtraction
let subreaction = 
        [ReactionS.Reaction(
            ExprS.Expr(
                [Species("A")]), 
            ExprS.Expr(
                [Species("A")
                 Species("C")]), 1.0)
         ReactionS.Reaction(
            ExprS.Expr(
                [Species("B")]), 
            ExprS.Expr(
                [Species("B")
                 Species("H")]), 1.0)
         ReactionS.Reaction(
            ExprS.Expr(
                [Species("C")]),
            ExprS.Expr([]), 1.0)
         ReactionS.Reaction(
            ExprS.Expr(
                [Species("C")
                 Species("H")]),
            ExprS.Expr(
                []), 1.0)
         ]

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

//division
let divreaction = 
        [ReactionS.Reaction(
            ExprS.Expr(
                [Species("A")]), 
            ExprS.Expr(
                [Species("A")
                 Species("C")]), 1.0)
         ReactionS.Reaction(
            ExprS.Expr(
                [Species("B")
                 Species("C")]
            ),
            ExprS.Expr(
                [Species("B")]), 1.0
         )]

//square root
let sqrtreaction = 
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
            ExprS.Expr(
                []), 0.5
         )]

//clock simulator
let clockreaction = 
        [ReactionS.Reaction(
            ExprS.Expr(
                [Species("A")
                 Species("B")]), 
            ExprS.Expr(
                [Species("B")
                 Species("B")]), 1.0)
         ReactionS.Reaction(
            ExprS.Expr(
                [Species("B")
                 Species("C")]
            ),
            ExprS.Expr(
                [Species("C")
                 Species("C")]), 1.0)
         ReactionS.Reaction(
            ExprS.Expr(
                [Species("C")
                 Species("A")]
            ),
            ExprS.Expr(
                [Species("A")
                 Species("A")]), 1.0
        )]


let initial = Map.ofList ["A", 0.98; "B", 0.01; "C", 0.01; "H", 0.0] 

let test = (solveODE initial 0.1 clockreaction)
CRN.Visualization.plotState (fun s -> seq [ "A"; "B"; "C" ] |> Seq.contains s) 5000 test
