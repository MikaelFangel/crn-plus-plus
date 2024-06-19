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
    |> Result.bind (fun x -> Ok(CRN.Interpreter.interpreter (Map.ofList [ ("a0", 20); ("b0", 5) ]) x))

printfn "Ran"
printfn "%A" (testParser "division.crn")

let parse = testParser "division.crn"

let unwrap =
    match parse with
    | Ok x -> x
    | _ -> failwith "Error"

CRN.Visualization.plotState (fun s -> seq [ "r"; "a" ] |> Seq.contains s) 100 unwrap
