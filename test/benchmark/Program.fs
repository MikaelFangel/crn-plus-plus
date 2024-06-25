﻿open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Running

open CRNBench.Simulator

[<EntryPoint>]
let main argv =
    BenchmarkRunner.Run<SimulatorBenchmarking>() |> ignore
    //BenchmarkRunner.Run<CompilerBenchmarking>() |> ignore
    0