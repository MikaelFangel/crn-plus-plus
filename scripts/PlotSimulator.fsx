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
let clock3reaction = 
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
let clock9reaction = 
        [ReactionS.Reaction(
            ExprS.Expr(
                [Species("X1")
                 Species("X2")]), 
            ExprS.Expr(
                [Species("X2")
                 Species("X2")]), 1.0)
         ReactionS.Reaction(
            ExprS.Expr(
                [Species("X2")
                 Species("X3")]
            ),
            ExprS.Expr(
                [Species("X3")
                 Species("X3")]), 1.0)
         ReactionS.Reaction(
            ExprS.Expr(
                [Species("X3")
                 Species("X4")]
            ),
            ExprS.Expr(
                [Species("X4")
                 Species("X4")]), 1.0)
         ReactionS.Reaction(
            ExprS.Expr(
                [Species("X4")
                 Species("X5")]), 
            ExprS.Expr(
                [Species("X5")
                 Species("X5")]), 1.0)
         ReactionS.Reaction(
            ExprS.Expr(
                [Species("X5")
                 Species("X6")]
            ),
            ExprS.Expr(
                [Species("X6")
                 Species("X6")]), 1.0)
         ReactionS.Reaction(
            ExprS.Expr(
                [Species("X6")
                 Species("X7")]
            ),
            ExprS.Expr(
                [Species("X7")
                 Species("X7")]), 1.0)
         ReactionS.Reaction(
            ExprS.Expr(
                [Species("X7")
                 Species("X8")]), 
            ExprS.Expr(
                [Species("X8")
                 Species("X8")]), 1.0)
         ReactionS.Reaction(
            ExprS.Expr(
                [Species("X8")
                 Species("X9")]
            ),
            ExprS.Expr(
                [Species("X9")
                 Species("X9")]), 1.0)
         ReactionS.Reaction(
            ExprS.Expr(
                [Species("X9")
                 Species("X1")]
            ),
            ExprS.Expr(
                [Species("X1")
                 Species("X1")]), 1.0)
        ]


let initial = Map.ofList ["X1", 1.0-8.0*(0.1e-11); "X2", 0.1e-11; "X3", 0.1e-11; "X4", 0.1e-11; "X5", 0.1e-11; "X6", 0.1e-11; "X7", 0.1e-11; "X8", 0.1e-11; "X9", 0.1e-11] 

let test = (solveODE initial 0.01 clock9reaction)
CRN.Visualization.plotState (fun s -> seq [ "X1"; "X2"; "X3"; "X4"; "X5"; "X6"; "X7"; "X8"; "X9" ] |> Seq.contains s) 200000 test
