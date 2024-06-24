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

let filePi = "crn = {
    conc[four, 4],
    conc[divisor1, 1],
    conc[divisor2, 3],
    conc[pi, 0],
    step[{
        div[four, divisor1, factor1],
        add[divisor1, four, divisor1Next],
        div[four, divisor2, factor2],
        add[divisor2, four, divisor2Next],
        sub[factor1, factor2, factor],
        add[pi, factor, piNext]
    }],
    step[{
        ld[divisor1Next, divisor1],
        ld[divisor2Next, divisor2],
        ld[piNext, pi]
    }]
}"

let fileSubAlt = "crn = {
    conc[a, 15], conc[b, 6],
    conc[one, 1], conc[zero, 0],
    step[{
        cmp[b, zero]
    }],
    step[{
        ifGE[{
            sub[a, one, anext],
            sub[b, one, bnext]
        }]
    }],
    step[{
        ifGE[{
            ld[anext, a],
            ld[bnext, b]
        }]
    }]
}"


let getFile str =
    let result = 
        str
        |> CRN.Parser.tryParse 
        |> Result.bind CRN.Typechecker.typecheck
        |> Result.bind (fun res -> Ok (CRN.Compiler.compile (Map.empty) res))
    match result with
    Ok rex -> rex
    | Error _ -> failwith "Could not find file"

let buildSystem complexity =
    match complexity with
    | 1 -> clockinitial, clockreaction
    | 2 -> getFile filePi
    | 3 -> getFile fileSubAlt
    | _ -> failwith "Unexpected input to buildSystem"

type SimulatorBenchmarking() =

    let mutable system = None

    let time = 0.01

    let elements = 50000

    [<Params(1, 2, 3)>]
    member val Complexity = 0 with get, set

    [<GlobalSetup(Targets = [|"SolveODEOriginal"; "SolveODEFunctional"; "SolveODEImperative"|])>]
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
    member _.SolveODEImperative() =
        match system with
        | Some (initial, reaction) -> solveODEFast initial time reaction |> snd |> Seq.take elements |> Seq.toList |> ignore
        | None -> failwith "No ODE to benchmark."

