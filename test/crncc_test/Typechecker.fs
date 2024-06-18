module CRNTest.Typechecker

open Xunit

open CRNTest.Common
open CRN.Typechecker

[<Fact>]
let ``Typechecker: counter`` () =
    let result = testParser "counter.crn"
    let result = result |> Result.bind typecheck 
    Assert.True(Result.isOk result, result.ToString())

[<Fact>]
let ``Typechecker: division`` () =
    let result = testParser "division.crn"
    let result = result |> Result.bind typecheck 
    Assert.True(Result.isOk result, result.ToString())


[<Fact>]
let ``Typechecker: euler`` () =
    let result = testParser "euler.crn"
    let result = result |> Result.bind typecheck 

    Assert.True(Result.isOk result, result.ToString())


[<Fact>]
let ``Typechecker: gcd`` () =
    let result = testParser "gcd.crn"
    let result = result |> Result.bind typecheck 

    Assert.True(Result.isOk result, result.ToString())


[<Fact>]
let ``Typechecker: isqrt`` () =
    let result = testParser "isqrt.crn"
    let result = result |> Result.bind typecheck 

    Assert.True(Result.isOk result, result.ToString())


[<Fact>]
let ``Typechecker: pi`` () =
    let result = testParser "pi.crn"
    let result = result |> Result.bind typecheck 
    Assert.True(Result.isOk result, result.ToString())
    

[<Fact>]
let ``Typechecker: subalt`` () =
    let result = testParser "subalt.crn"
    let result = result |> Result.bind typecheck 
    
    Assert.True(Result.isOk result, result.ToString())


[<Fact>]
let ``Typechecker: factorial`` () =
    let result = testParser "factorial.crn"
    let result = result |> Result.bind typecheck 

    Assert.True(Result.isOk result, result.ToString())

[<Fact>]
let ``Typechecker: Ill Formed multiple mutation`` () =
    let result = testParserIllFormed "multiple_mutation.crn"
    let result = result |> Result.bind typecheck 

    Assert.True(Result.isError result, result.ToString())

[<Fact>]
let ``Typechecker: Ill Formed repeat conditions`` () =
    let result = testParserIllFormed "repeat_conditions.crn"
    let result = result |> Result.bind typecheck 

    Assert.True(Result.isError result, result.ToString())

[<Fact>]
let ``Typechecker: Ill Formed repeat conditions 2`` () =
    let result = testParserIllFormed "repeat_conditions2.crn"
    let result = result |> Result.bind typecheck 

    Assert.True(Result.isError result, result.ToString())

[<Fact>]
let ``Typechecker: Ill Formed dangling condition`` () =
    let result = testParserIllFormed "dangling_condition.crn"
    let result = result |> Result.bind typecheck 

    Assert.True(Result.isError result, result.ToString())