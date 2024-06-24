module CRNBench.Simulator

open BenchmarkDotNet.Attributes

open CRN.AST
open CRN.Simulator

let clockoffset = 0.0000000009
let clockinitial = Map.ofList ["A", 1.0 - 2.0 * clockoffset; "B", clockoffset; "C", clockoffset] 
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

let getTestFile name =
    let path = System.IO.Path.Combine("../../../../../../../../../examples", name)
    System.IO.File.ReadAllText path

let getFile name =
    let result = 
        getTestFile name 
        |> CRN.Parser.tryParse 
        |> Result.bind CRN.Typechecker.typecheck
        |> Result.bind (fun res -> Ok (CRN.Compiler.compile (Map.ofList ["a0", 5; "b0", 3]) res))
    match result with
    Ok rex -> rex
    | Error _ -> failwith "Could not find file"

let buildSystem complexity =
    match complexity with
    | 1 -> clockinitial, clockreaction
    | 2 -> getFile "pi.crn"
    | 3 -> getFile "subalt.crn"
    | _ -> failwith "Unexpected input to buildSystem"

type SimulatorBenchmarking() =

    let mutable system = None

    let time = 0.01

    let elements = 5000

    [<Params(1, 2, 3)>]
    member val Complexity = 0 with get, set

    [<GlobalSetup(Targets = [|"SolveODEOriginal"; "SolveODEFunctional"; "SolveODEImperative1"; "SolveODEImperative2"|])>]
    member self.setupSystem() = system <- Some(buildSystem self.Complexity)

    [<Benchmark>]
    member _.SolveODEOriginal() =
        match system with
        | Some (initial, reaction) -> solveODE initial time reaction |> Seq.take elements |> Seq.toList |> ignore
        | None -> failwith "No ODE to benchmark."

    [<Benchmark>]
    member _.SolveODEFunctional() =
        match system with
        | Some (initial, reaction) -> solveODEFunctional initial time reaction |> snd |> Seq.take elements |> Seq.toList |> Seq.toList |> ignore
        | None -> failwith "No ODE to benchmark."


    [<Benchmark>]
    member _.SolveODEImperative1() =
        match system with
        | Some (initial, reaction) -> solveODEFast initial time reaction |> snd |> Seq.take elements |> Seq.toList |> ignore
        | None -> failwith "No ODE to benchmark."


    [<Benchmark>]
    member _.SolveODEImperative2() =
        match system with
        | Some (initial, reaction) -> solveODEFast2 initial time reaction |> snd |> Seq.take elements |> Seq.toList |> ignore
        | None -> failwith "No ODE to benchmark."

