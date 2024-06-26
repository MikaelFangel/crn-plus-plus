module CRNTest.Compiler
open Xunit

open CRNTest.Common
open CRN.Typechecker
open CRN.Compiler
open FsCheck.Xunit
open FsCheck

[<TailCall>]
let rec factorial (x:float) =
    function
    | n when n <= 1.0 -> x
    | n -> factorial (n*x) (n-1.0)

[<TailCall>]
let rec gcd a b  step=
    match (a,b) with 
    | (a,b) when a=b -> a,step
    | (a,b) when a>b -> gcd (a-b) b (step+1)
    | (a,b) -> gcd a (b-a) (step+1)

let convertOut env step reactions =
    let names,states = CRN.Simulator.solveODEFast env step reactions
    Seq.map (CRN.Simulator.ArraytoMap names) states

[<Property>]
let ``Compiler: counter`` (c:PositiveInt) =
    let c = int c
    if c >= 13 then Prop.classify true "Timer ran out" true else
        let result = testParser "counter.crn"
        let result = result |> Result.bind typecheck 
        let result = Result.bind (fun x -> Ok (compile (Map.add "c0" c Map.empty) x)) result
        match result with 
        | Error a -> Prop.classify true "Error" (Result.isOk result)
        | Ok s ->   let simulated = convertOut (fst s) 0.025 (snd s)
                    let existsState = (Seq.take 100000 simulated) |> Seq.exists (fun map -> Map.find "c" map <= 0.5) 
                    Prop.ofTestable existsState

[<Property>]
let ``Compiler: division`` (a:PositiveInt, b:PositiveInt) =
    let a = int a
    let b = int b
    if b = 0 then Prop.classify true "b is zero" true
    else if (a / b) >= 13 then Prop.classify true "Timer ran out" true else
        let result = testParser "division.crn"
        let result = result |> Result.bind typecheck 
        let result = Result.bind (fun x -> Ok (compile (Map.ofList [("a0", a);("b0",b)]) x)) result
        match result with 
        | Error a -> Prop.classify true "Error" (Result.isOk result)
        | Ok s ->   let simulated = convertOut (fst s) 0.01 (snd s)
                    let quot = float (a / b)
                    let res = float (a % b)
                    let existsState = (Seq.take 150000 simulated) |> Seq.exists (fun map ->  abs (Map.find "q" map - quot) <= 0.5
                                                                                            && abs (Map.find "r" map - res) <= 0.5 ) 
                    Prop.ofTestable existsState

[<Property>]
let ``Compiler: gcd`` (a:PositiveInt, b:PositiveInt) =
    let a = int a
    let b = int b
    let gcdab, step = gcd a b 0
    if step >=10 then Prop.classify true "Timer ran out" true else
        let result = testParser "gcd.crn"
        let result = result |> Result.bind typecheck 
        let result = Result.bind (fun x -> Ok (compile (Map.ofList [("a0", a);("b0", b)]) x)) result
        match result with 
        | Error a -> Prop.classify true "Error" (Result.isOk result)
        | Ok s ->   let simulated = convertOut (fst s) 0.025 (snd s)
                    let existsState = (Seq.take 150000 simulated) |> Seq.exists (fun map ->  abs (Map.find "a" map - float gcdab) <= 0.5 
                                                                                            || abs (Map.find "b" map - float gcdab) <= 0.5) 
                    Prop.ofTestable existsState

[<Property>]
let ``Compiler: isqrt`` (n:PositiveInt) =
    let n = int n
    if n >= 37 then Prop.classify true "Timer ran out" true else
        let result = testParser "isqrt.crn"
        let result = result |> Result.bind typecheck 
        let result = Result.bind (fun x -> Ok (compile (Map.add "n0" n Map.empty) x)) result
        match result with 
        | Error a -> Prop.classify true "Error" (Result.isOk result)
        | Ok s ->   let simulated = convertOut (fst s) 0.025 (snd s)
                    let sqrtn = floor (sqrt (float n))
                    if sqrtn*sqrtn = n then Prop.classify true "Off by one error" true else
                        let lastState = (Seq.take 150000 simulated) |> Seq.last |> (fun map -> abs (Map.find "out" map - sqrtn) <= 0.5) 
                        Prop.ofTestable lastState

[<Property>]
let ``Compiler: subalt`` (a:PositiveInt, b:PositiveInt) =
    let a = int a
    let b = int b
    if a >= 13 || b >= 13 then Prop.classify true "Timer ran out" true else
        let result = testParser "subalt.crn"
        let result = result |> Result.bind typecheck 
        let result = Result.bind (fun x -> Ok (compile (Map.ofList [("a0", a);("b0", b)]) x)) result
        match result with 
        | Error a -> Prop.classify true "Error" (Result.isOk result)
        | Ok s ->   let simulated = convertOut (fst s) 0.025 (snd s)
                    let lastState = (Seq.take 150000 simulated) |> Seq.last |> (fun map ->  let x = Map.find "b" map 
                                                                                            abs (x - float a) <= 0.5 || x <= 0.5 ) 
                    Prop.ofTestable lastState 

[<Property>]
let ``Compiler: factorial`` (f:PositiveInt) =
    let f = int f
    if f >= 13 then Prop.classify true "Timer ran out" true else
    let result = testParser "factorial.crn"
    let result = result |> Result.bind typecheck 
    let result = Result.bind (fun x -> Ok (compile (Map.add "f0" f Map.empty) x)) result
    match result with 
    | Error a -> Prop.classify true "Error" (Result.isOk result)
    | Ok s ->   let simulated = convertOut (fst s) 0.025 (snd s)
                let lastState = (Seq.take 150000 simulated) |> Seq.last |> (fun map -> abs (Map.find "f" map -  (factorial 1 f)) <=0.5) 
                Prop.ofTestable lastState 

[<Fact>]
let ``Compiler: euler`` () =
    let result = testParser "euler.crn"
    let result = result |> Result.bind typecheck 
    let result = Result.bind (fun x -> Ok (compile Map.empty x)) result
    match result with 
    | Error a -> Prop.classify true "Error" (Result.isOk result)
    | Ok s ->   let simulated = convertOut (fst s) 0.1 (snd s)
                let lastState = (Seq.take 50000 simulated) |> Seq.last |> (fun map -> abs (Map.find "e" map - System.Math.E)<= 0.5 ) 
                Prop.ofTestable lastState 


[<Fact>]
let ``Compiler: pi`` () =
    let result = testParser "pi.crn"
    let result = result |> Result.bind typecheck 
    let result = Result.bind (fun x -> Ok (compile Map.empty x)) result
    match result with 
    | Error a -> Prop.classify true "Error" (Result.isOk result)
    | Ok s ->   let simulated = convertOut (fst s) 0.1 (snd s)
                let lastState = (Seq.take 50000 simulated) |> Seq.last |> (fun map -> abs (Map.find "pi" map - System.Math.PI)<= 0.5 )
                Prop.ofTestable lastState 

    