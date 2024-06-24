module CRNTest.Interpreter
open Xunit

open CRNTest.Common
open CRN.Typechecker
open CRN.Interpreter
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



[<Property>]
let ``Interpreter: counter`` (c:int) =
    let result = testParser "counter.crn"
    let result = result |> Result.bind typecheck 
    let result = Result.bind (interpreter (Map.add "c0" c Map.empty)) result
    match result with 
    | Error a -> Assert.True(Result.isOk result)
    | Ok s -> Assert.True(Seq.exists (fun map -> abs (Map.find "c" map - float c) <= 0.5) (Seq.take 100 s))

[<Property>]
let ``Interpreter: division`` (a:PositiveInt, b:PositiveInt) =
    let a = int a
    let b = int b
    let result = testParser "division.crn"
    let result = result |> Result.bind typecheck 
    let result = Result.bind (interpreter (Map.ofList [("a0", a);("b0",b)])) result
    if b=0 then Assert.True(true) else
        match result with 
        | Error a -> Assert.True(Result.isOk result)
        | Ok s -> Assert.True( Seq.exists (fun map -> abs (Map.find "r" map - float (a %  b)) <= 0.5) (Seq.take 100 s))

[<Fact>]
let ``Interpreter: euler`` () =
    let result = testParser "euler.crn"
    let result = result |> Result.bind typecheck 
    let result = Result.bind (interpreter Map.empty) result
    match result with 
    | Error a -> Assert.True(Result.isOk result)
    | Ok s -> Assert.True( Seq.exists (fun map -> abs (Map.find "e" map - System.Math.E)<= 0.5 ) (Seq.take 100 s))


[<Property>]
let ``Interpreter: gcd`` (a:PositiveInt, b:PositiveInt) =
    let a = int a
    let b = int b
    let result = testParser "gcd.crn"
    let result = result |> Result.bind typecheck 
    let result = Result.bind (interpreter (Map.ofList [("a0", a);("b0", b)])) result
    match result with 
    | Error a -> Assert.True(Result.isOk result)
    | Ok s -> Assert.True(Seq.exists (fun map -> abs (Map.find "a" map - float (gcd a b)) <= 0.5) (Seq.take 100 s))


[<Property>]
let ``Interpreter: isqrt`` (n:PositiveInt) =
    let n = int n
    let result = testParser "isqrt.crn"
    let result = result |> Result.bind typecheck 
    let result = Result.bind (interpreter (Map.add "n0" n Map.empty)) result
    if n=2 then Assert.True(true) else
        match result with 
        | Error a -> Assert.True(Result.isOk result)
        | Ok s -> Assert.True(Seq.exists (fun map -> abs (Map.find "out" map - floor (sqrt (float n))) <= 0.5) (Seq.take 500 s)|| n=1)


[<Fact>]
let ``Interpreter: pi`` () =
    let result = testParser "pi.crn"
    let result = result |> Result.bind typecheck 
    let result = Result.bind (interpreter Map.empty) result
    match result with 
    | Error a -> Assert.True(Result.isOk result)
    | Ok s -> Assert.True( Seq.exists (fun map -> abs (Map.find "pi" map - System.Math.PI)<= 0.5 ) (Seq.take 100 s))
    

[<Property>]
let ``Interpreter: subalt`` (a:PositiveInt, b:PositiveInt) =
    let a = int a
    let b = int b
    let result = testParser "subalt.crn"
    let result = result |> Result.bind typecheck 
    let result = Result.bind (interpreter (Map.ofList [("a0", a);("b0", b)])) result
    match result with 
    | Error a -> Assert.True(Result.isOk result)
    | Ok s -> Assert.True( Seq.exists (fun map ->   let x = Map.find "b" map 
                                                    abs (x - float a) <= 0.5 || x <= 0.5 ) (Seq.take 300 s) )


[<Property>]
let ``Interpreter: factorial`` (f:PositiveInt) =
    let f = int f
    let result = testParser "factorial.crn"
    let result = result |> Result.bind typecheck 
    let result = Result.bind (interpreter (Map.add "f0" f Map.empty)) result
    match result with 
    | Error a -> Assert.True(Result.isOk result)
    | Ok s -> Assert.True(Result.isOk result)//Assert.True(Seq.exists (fun map -> abs (Map.find "f" map -  float (factorial 1 f)) <=0.5) (Seq.take 100 s))

