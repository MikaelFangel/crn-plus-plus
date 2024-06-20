#r "nuget: FParsec, 1.1.1"
#r "nuget: Plotly.NET, 4.2.0"

#load "../src/crncc/AST.fs"
#load "../src/crncc/Parser.fs"
#load "../src/crncc/Typechecker.fs"
#load "../src/crncc/Compiler.fs"
#load "../src/crncc/Simulator.fs"
#load "../src/crncc/Interpreter.fs"
#load "../src/CRN.Visualization/Library.fs"

open System

let getTestFile name =
    let path = System.IO.Path.Combine("../examples", name)
    IO.File.ReadAllText path

let testParser name =
    getTestFile name |> CRN.Parser.tryParse |> Result.bind CRN.Typechecker.typecheck

let filename = "example.crn"

let parse = testParser filename

let compiled =
    match parse with
    | Ok x -> x |> CRN.Compiler.compileCrnS
    | _ -> failwith "Error"

snd compiled |> List.map (fun s -> printfn "R %A" s)

let simulated = CRN.Simulator.solveODE (fst compiled) 0.1 (snd compiled)
simulated |> Seq.take 2 |> Seq.toList |> List.map (fun s -> printfn "%A" s)
printfn "Ran"

CRN.Visualization.plotState (fun s -> seq [ "c" ] |> Seq.contains s) 100 simulated
