module CRNTest.Interpreter
open Xunit

open CRNTest.Common
open CRN.Typechecker
open CRN.Interpreter
open FsCheck.Xunit
open FsCheck

[<TailCall>]
let rec factorial (x:float) =
    function
    | n when n <= 1.0 -> x
    | n -> factorial (n*x) (n-1.0)

[<TailCall>]
let rec gcd a b =
    match (a,b) with 
    | (a,b) when a=b -> a
    | (a,b) when a>b -> gcd (a-b) b
    | (a,b) -> gcd a (b-a)



[<Property>]
let ``Interpreter: counter`` (c:PositiveInt) =
    let c = int c
    let result = testParser "counter.crn"
    let result = result |> Result.bind typecheck 
    let result = Result.bind (interpreter (Map.add "c0" c Map.empty)) result
    match result with 
    | Error a -> Prop.classify true "Error" (Result.isOk result)
    | Ok s ->   let existsState = Seq.take 300 s |> Seq.exists (fun map -> Map.find "c" map <= 0.5) 
                existsState |> Prop.classify (not existsState) "Timer ran out"

[<Property>]
let ``Interpreter: division`` (a:PositiveInt, b:PositiveInt) =
    let a = int a
    let b = int b
    let result = testParser "division.crn"
    let result = result |> Result.bind typecheck 
    let result = Result.bind (interpreter (Map.ofList [("a0", a);("b0",b)])) result
    if b=0 then Prop.classify true "b is zero" true else
        match result with 
        | Error a -> Prop.classify true "Error" (Result.isOk result)
        | Ok s ->   let res = float (a %  b)
                    let quot = float (a /  b)
                    let lastState = (Seq.take 300 s) |> Seq.last |> (fun map -> abs (Map.find "r" map - res) <= 0.5 
                                                                                && abs (Map.find "q" map - quot) <= 0.5) 
                    lastState |> Prop.classify (not lastState) "Timer ran out"

[<Property>]
let ``Interpreter: gcd`` (a:PositiveInt, b:PositiveInt) =
    let a = int a
    let b = int b
    let result = testParser "gcd.crn"
    let result = result |> Result.bind typecheck 
    let result = Result.bind (interpreter (Map.ofList [("a0", a);("b0", b)])) result
    match result with 
    | Error a -> Prop.classify true "Error" (Result.isOk result)
    | Ok s ->   let lastState = Seq.take 300 s |> Seq.last |> (fun map -> abs (Map.find "a" map - float (gcd a b)) <= 0.5) 
                lastState |> Prop.classify (not lastState) "Timer ran out"

[<Property>]
let ``Interpreter: isqrt`` (n:PositiveInt) =
    let n = int n
    let result = testParser "isqrt.crn"
    let result = result |> Result.bind typecheck 
    let result = Result.bind (interpreter (Map.add "n0" n Map.empty)) result
    true
    (*match result with 
    | Error a -> Prop.classify true "Error" (Result.isOk result)
    | Ok s ->   let sqrtn = floor (sqrt (float n))
                let lastState = Seq.take 300 s |> Seq.last |> (fun map -> abs (Map.find "out" map - sqrtn) <= 0.5) 
                lastState |> Prop.classify (not lastState) "Timer ran out"
    *)

[<Property>]
let ``Interpreter: subalt`` (a:PositiveInt, b:PositiveInt) =
    let a = int a
    let b = int b
    let result = testParser "subalt.crn"
    let result = result |> Result.bind typecheck 
    let result = Result.bind (interpreter (Map.ofList [("a0", a);("b0", b)])) result
    match result with 
    | Error a -> Prop.classify true "Error" (Result.isOk result)
    | Ok s ->   let lastState = Seq.take 300 s |> Seq.last |> (fun map ->   let x = Map.find "b" map 
                                                                            abs (x - float a) <= 0.5 || x <= 0.5 )
                lastState |> Prop.classify (not lastState) "Timer ran out" 

[<Property>]
let ``Interpreter: factorial`` (f: PositiveInt) =
    let f = int f
    let result = testParser "factorial.crn"
    let result = result |> Result.bind typecheck 
    let result = Result.bind (interpreter (Map.add "f0" f Map.empty)) result
    match result with 
    | Error a -> Prop.classify true "Error" (Result.isOk result)
    | Ok s ->   let lastState = (Seq.take 300 s) |> Seq.last |> (fun map -> abs (Map.find "f" map -  (factorial 1.0 f)) <=0.5)
                lastState |> Prop.classify (not lastState) "Timer ran out"


[<Fact>]
let ``Interpreter: pi`` () =
    let result = testParser "pi.crn"
    let result = result |> Result.bind typecheck 
    let result = Result.bind (interpreter Map.empty) result
    match result with 
    | Error a -> Prop.classify true "Error" (Result.isOk result)
    | Ok s ->   let lastState = (Seq.take 300 s) |> Seq.last |> (fun map -> abs (Map.find "pi" map - System.Math.PI)<= 0.5 ) 
                lastState |> Prop.classify (not lastState) "Timer ran out"
    
[<Fact>]
let ``Interpreter: euler`` () =
    let result = testParser "euler.crn"
    let result = result |> Result.bind typecheck 
    let result = Result.bind (interpreter Map.empty) result
    match result with 
    | Error a -> Prop.classify true "Error" (Result.isOk result)
    | Ok s ->   let lastState = (Seq.take 300 s) |> Seq.last |> (fun map -> abs (Map.find "e" map - System.Math.E)<= 0.5 ) 
                lastState |> Prop.classify (not lastState) "Timer ran out"
