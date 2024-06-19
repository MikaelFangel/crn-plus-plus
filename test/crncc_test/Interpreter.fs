module CRNTest.Interpreter
open Xunit

open CRNTest.Common
open CRN.Typechecker
open CRN.Interpreter



[<Fact>]
let ``Interpreter: counter`` () =
    let result = testParser "counter.crn"
    let result = result |> Result.bind typecheck 
    let result = Result.bind (interpreter (Map.add "c0" 3 Map.empty)) result
    Assert.True(Result.isOk result)

[<Fact>]
let ``Interpreter: division`` () =
    let result = testParser "division.crn"
    let result = result |> Result.bind typecheck 
    let result = Result.bind (interpreter (Map.ofList [("a0", 20);("b0",3)])) result
    Assert.True(Result.isOk result)


[<Fact>]
let ``Interpreter: euler`` () =
    let result = testParser "euler.crn"
    let result = result |> Result.bind typecheck 
    let result = Result.bind (interpreter Map.empty) result
    Assert.True(Result.isOk result)


[<Fact>]
let ``Interpreter: gcd`` () =
    let result = testParser "gcd.crn"
    let result = result |> Result.bind typecheck 
    let result = Result.bind (interpreter (Map.ofList [("a0", 32);("b0",12)])) result
    Assert.True(Result.isOk result)


[<Fact>]
let ``Interpreter: isqrt`` () =
    let result = testParser "isqrt.crn"
    let result = result |> Result.bind typecheck 
    let result = Result.bind (interpreter (Map.add "n0" 10 Map.empty)) result
    Assert.True(Result.isOk result)


[<Fact>]
let ``Interpreter: pi`` () =
    let result = testParser "pi.crn"
    let result = result |> Result.bind typecheck 
    let result = Result.bind (interpreter Map.empty) result
    Assert.True(Result.isOk result)
    

[<Fact>]
let ``Interpreter: subalt`` () =
    let result = testParser "subalt.crn"
    let result = result |> Result.bind typecheck 
    let result = Result.bind (interpreter (Map.ofList [("a0", 32);("b0",12)])) result
    Assert.True(Result.isOk result)


[<Fact>]
let ``Interpreter: factorial`` () =
    let result = testParser "factorial.crn"
    let result = result |> Result.bind typecheck 
    let result = Result.bind (interpreter (Map.add "f0" 5 Map.empty)) result
    Assert.True(Result.isOk result)

