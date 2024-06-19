#r "nuget: FParsec, 1.1.1"
#r "nuget: Plotly.NET, 4.2.0"

#load "../src/crncc/AST.fs"
#load "../src/crncc/Parser.fs"
#load "../src/crncc/Typechecker.fs"
#load "../src/crncc/Interpreter.fs"
#load "../src/CRN.Visualization/Library.fs"

open System

let getTestFile name =
    let path = System.IO.Path.Combine("../examples", name)
    IO.File.ReadAllText path

let testParser name =
    getTestFile name
    |> CRN.Parser.tryParse
    |> Result.bind CRN.Typechecker.typecheck
    |> Result.bind (fun x -> CRN.Interpreter.interpreter Map.empty x)
let filename = "euler.crn"
printfn "Ran"
printfn "%A" (testParser filename)

let parse = testParser filename

let unwrap =
    match parse with
    | Ok x -> x
    | _ -> failwith "Error"

CRN.Visualization.plotState (fun s -> seq [ "e"] |> Seq.contains s) 100 unwrap
