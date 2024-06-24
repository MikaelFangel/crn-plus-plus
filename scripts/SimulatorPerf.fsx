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

let filename = "pi.crn"

let parse = testParser filename

let compiled =
    match parse with
    | Ok x -> x |> (CRN.Compiler.compile (Map []))
    | _ -> failwith "Error"

snd compiled |> List.map (fun s -> printfn "R %A" s)

let inline floatEq eps f1 f2: bool =
    (((f1 - eps) < f2) && ((f1 + eps) > f2))

let compareFS names (arr, smap): bool =
    arr
    |> Array.toList
    |> List.zip names
    |> List.map (fun (name, num) -> (floatEq 1e-6) (Map.find name smap) num)
    |> List.fold (&&) true

let toMap names arr =
    arr
    |> Array.toList
    |> List.zip names
    |> Map.ofList

//let simulated = CRN.Simulator.solveODE (fst compiled) 0.1 (snd compiled)
let names, sequence = CRN.Simulator.solveODEFunctional (fst compiled) 0.1 (snd compiled)

let toMap' = toMap names

let toprint = 5

//sequence |> Seq.take 2 |> Seq.toList |> List.map (fun s -> printfn "%A" s)
let t1 = System.DateTime.Now
sequence |> Seq.take 500000 |> Seq.toList
let t2 = System.DateTime.Now
printfn "Ran first: %A" (t2 - t1)
printfn "First ten: %A" (Seq.take toprint sequence |> Seq.map toMap' |> Seq.toList) 


let slowsequence = CRN.Simulator.solveODE (fst compiled) 0.1 (snd compiled)
let t3 = System.DateTime.Now
slowsequence |> Seq.take 50000 |> Seq.toList
let t4 = System.DateTime.Now
printfn "Ran second: %A" (t4 - t3)
printfn "First ten: %A" (Seq.take toprint slowsequence |> Seq.toList)

let compare = compareFS names

let result = 
    Seq.zip sequence slowsequence
    |> Seq.take 50
    |> Seq.map compare
    |> Seq.fold (&&) true
printfn "Result: %A" result

//CRN.Visualization.plotState (fun s -> seq [ "c" ] |> Seq.contains s) 100 simulated
