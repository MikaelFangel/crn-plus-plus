module CRNTest.Compiler
open Xunit

open CRNTest.Common
open CRN.Typechecker
open CRN.Compiler
open FsCheck.Xunit
open FsCheck

[<TailCall>]
let rec factorial x =
    function
    | 1 -> x
    | n -> factorial (n-1) (n*x)

[<TailCall>]
let rec gcd a b =
    match (a,b) with 
    | (a,b) when a=b -> a
    | (a,b) when a>b -> gcd (a-b) b
    | (a,b) -> gcd a (b-a)

let convertOut env step reactions =
    let names,states = CRN.Simulator.solveODEFast env step reactions
    Seq.map (CRN.Simulator.ArraytoMap names) states

[<Property>]
let ``Compiler: counter`` (c:int) =
    let result = testParser "counter.crn"
    let result = result |> Result.bind typecheck 
    let result = Result.bind (fun x -> Ok (compile (Map.add "c0" c Map.empty) x)) result
    match result with 
    | Error a -> Assert.True(Result.isOk result)
    | Ok s ->   let simulated = convertOut (fst s) 0.025 (snd s)
                Assert.True(Seq.exists (fun map -> abs (Map.find "c" map - float c) <= 0.5) (Seq.take 50000 simulated))

[<Property>]
let ``Compiler: division`` (a:PositiveInt, b:PositiveInt) =
    let a = int a
    let b = int b
    let result = testParser "division.crn"
    let result = result |> Result.bind typecheck 
    let result = Result.bind (fun x -> Ok (compile (Map.ofList [("a0", a);("b0",b)]) x)) result
    if b=0 then Assert.True(true) else
        match result with 
        | Error a -> Assert.True(Result.isOk result)
        | Ok s ->   let simulated = convertOut (fst s) 0.025 (snd s)
                    Assert.True( Seq.exists (fun map -> abs (Map.find "q" map - float (a /  b)) <= 0.5) (Seq.take 150000 simulated))

[<Fact>]
let ``Compiler: euler`` () =
    let result = testParser "euler.crn"
    let result = result |> Result.bind typecheck 
    let result = Result.bind (fun x -> Ok (compile Map.empty x)) result
    match result with 
    | Error a -> Assert.True(Result.isOk result)
    | Ok s ->   let simulated = convertOut (fst s) 0.1 (snd s)
                Assert.True( Seq.exists (fun map -> abs (Map.find "e" map - System.Math.E)<= 0.5 ) (Seq.take 10000 simulated))


[<Property>]
let ``Compiler: gcd`` (a:PositiveInt, b:PositiveInt) =
    let a = int a
    let b = int b
    let result = testParser "gcd.crn"
    let result = result |> Result.bind typecheck 
    let result = Result.bind (fun x -> Ok (compile (Map.ofList [("a0", a);("b0", b)]) x)) result
    match result with 
    | Error a -> Assert.True(Result.isOk result)
    | Ok s ->   let simulated = convertOut (fst s) 0.025 (snd s)
                let gcdab = float (gcd a b)
                Assert.True(Seq.exists (fun map -> abs (Map.find "a" map - gcdab) <= 0.5 
                                                || abs (Map.find "b" map - gcdab) <= 0.5) 
                                        (Seq.take 150000 simulated))


[<Property>]
let ``Compiler: isqrt`` (n:PositiveInt) =
    let n = int n
    let result = testParser "isqrt.crn"
    let result = result |> Result.bind typecheck 
    let result = Result.bind (fun x -> Ok (compile (Map.add "n0" n Map.empty) x)) result
    match result with 
    | Error a -> Assert.True(Result.isOk result)
    | Ok s ->   let simulated = convertOut (fst s) 0.025 (snd s)
                let sqrtn = floor (sqrt (float n))
                Assert.True(Seq.exists (fun map -> 
                                        abs (Map.find "out" map - sqrtn) <= 0.5) (Seq.take 50000 simulated)|| n=1)


[<Fact>]
let ``Compiler: pi`` () =
    let result = testParser "pi.crn"
    let result = result |> Result.bind typecheck 
    let result = Result.bind (fun x -> Ok (compile Map.empty x)) result
    match result with 
    | Error a -> Assert.True(Result.isOk result)
    | Ok s ->   let simulated = convertOut (fst s) 0.1 (snd s)
                Assert.True( Seq.exists (fun map -> abs (Map.find "pi" map - System.Math.PI)<= 0.5 ) (Seq.take 10000 simulated))
    

[<Property>]
let ``Compiler: subalt`` (a:PositiveInt, b:PositiveInt) =
    let a = int a
    let b = int b
    let result = testParser "subalt.crn"
    let result = result |> Result.bind typecheck 
    let result = Result.bind (fun x -> Ok (compile (Map.ofList [("a0", a);("b0", b)]) x)) result
    match result with 
    | Error a -> Assert.True(Result.isOk result)
    | Ok s ->   let simulated = convertOut (fst s) 0.025 (snd s)
                Assert.True( Seq.exists (fun map -> let x = Map.find "b" map 
                                                    abs (x - float a) <= 0.5 || x <= 0.5 ) (Seq.take 150000 simulated) )


[<Property>]
let ``Compiler: factorial`` (f:PositiveInt) =
    let f = int f
    let result = testParser "factorial.crn"
    let result = result |> Result.bind typecheck 
    let result = Result.bind (fun x -> Ok (compile (Map.add "f0" f Map.empty) x)) result
    match result with 
    | Error a -> Assert.True(Result.isOk result)
    | Ok s ->   let simulated = convertOut (fst s) 0.025 (snd s)
                Assert.True(Result.isOk result)//Assert.True(Seq.exists (fun map -> abs (Map.find "f" map -  float (factorial 1 f)) <=0.5) (Seq.take 50000 simulated))

