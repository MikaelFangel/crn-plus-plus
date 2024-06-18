module Tests

open Xunit
open System
open CRN
open CRN.Parser


let getTestFile name =
    let path = System.IO.Path.Combine("../../../../../examples/", name)
    IO.File.ReadAllText path

let testParser name =
    let file = getTestFile name
    tryParse file


[<Fact>]
let ``Parser: counter.crn`` () =
    let result = testParser "counter.crn"
    Assert.True(Result.isOk result, result.ToString())

[<Fact>]
let ``Parser: division.crn`` () =
    let result = testParser "division.crn"
    Assert.True(Result.isOk result, result.ToString())


[<Fact>]
let ``Parser: euler.crn`` () =
    let result = testParser "euler.crn"
    Assert.True(Result.isOk result, result.ToString())


[<Fact>]
let ``Parser: gcd.crn`` () =
    let result = testParser "gcd.crn"
    Assert.True(Result.isOk result, result.ToString())


[<Fact>]
let ``Parser: isqrt.crn`` () =
    let result = testParser "isqrt.crn"
    Assert.True(Result.isOk result, result.ToString())


[<Fact>]
let ``Parser: pi.crn`` () =
    let result = testParser "pi.crn"
    Assert.True(Result.isOk result, result.ToString())
    

[<Fact>]
let ``Parser: subalt.crn`` () =
    let result = testParser "subalt.crn"
    Assert.True(Result.isOk result, result.ToString())


[<Fact>]
let ``Parser: factorial.crn`` () =
    let result = testParser "factorial.crn"
    Assert.True(Result.isOk result, result.ToString())


[<Fact>]
let ``Typechecker: counter.crn`` () =
    let result = testParser "counter.crn"
    let result = result |> Result.bind Typechecker.typecheck 
    Assert.True(Result.isOk result, result.ToString())

[<Fact>]
let ``Typechecker: division.crn`` () =
    let result = testParser "division.crn"
    let result = result |> Result.bind Typechecker.typecheck 
    Assert.True(Result.isOk result, result.ToString())


[<Fact>]
let ``Typechecker: euler.crn`` () =
    let result = testParser "euler.crn"
    let result = result |> Result.bind Typechecker.typecheck 

    Assert.True(Result.isOk result, result.ToString())


[<Fact>]
let ``Typechecker: gcd.crn`` () =
    let result = testParser "gcd.crn"
    let result = result |> Result.bind Typechecker.typecheck 

    Assert.True(Result.isOk result, result.ToString())


[<Fact>]
let ``Typechecker: isqrt.crn`` () =
    let result = testParser "isqrt.crn"
    let result = result |> Result.bind Typechecker.typecheck 

    Assert.True(Result.isOk result, result.ToString())


[<Fact>]
let ``Typechecker: pi.crn`` () =
    let result = testParser "pi.crn"
    let result = result |> Result.bind Typechecker.typecheck 
    Assert.True(Result.isOk result, result.ToString())
    

[<Fact>]
let ``Typechecker: subalt.crn`` () =
    let result = testParser "subalt.crn"
    let result = result |> Result.bind Typechecker.typecheck 
    
    Assert.True(Result.isOk result, result.ToString())


[<Fact>]
let ``Typechecker: factorial.crn`` () =
    let result = testParser "factorial.crn"
    let result = result |> Result.bind Typechecker.typecheck 

    Assert.True(Result.isOk result, result.ToString())

[<Fact>]
let ``Typechecker: illegal.crn`` () =
    let result = testParser "illegal.crn"
    let result = result |> Result.bind Typechecker.typecheck 

    Assert.True(Result.isError result, result.ToString())
