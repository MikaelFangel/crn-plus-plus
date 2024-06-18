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
let ``Parser: counter`` () =
    let result = testParser "counter.crn"
    Assert.True(Result.isOk result, result.ToString())

[<Fact>]
let ``Parser: division`` () =
    let result = testParser "division.crn"
    Assert.True(Result.isOk result, result.ToString())


[<Fact>]
let ``Parser: euler`` () =
    let result = testParser "euler.crn"
    Assert.True(Result.isOk result, result.ToString())


[<Fact>]
let ``Parser: gcd`` () =
    let result = testParser "gcd.crn"
    Assert.True(Result.isOk result, result.ToString())


[<Fact>]
let ``Parser: isqrt`` () =
    let result = testParser "isqrt.crn"
    Assert.True(Result.isOk result, result.ToString())


[<Fact>]
let ``Parser: pi`` () =
    let result = testParser "pi.crn"
    Assert.True(Result.isOk result, result.ToString())
    

[<Fact>]
let ``Parser: subalt`` () =
    let result = testParser "subalt.crn"
    Assert.True(Result.isOk result, result.ToString())


[<Fact>]
let ``Parser: factorial`` () =
    let result = testParser "factorial.crn"
    Assert.True(Result.isOk result, result.ToString())


[<Fact>]
let ``Typechecker: counter`` () =
    let result = testParser "counter.crn"
    let result = result |> Result.bind Typechecker.typecheck 
    Assert.True(Result.isOk result, result.ToString())

[<Fact>]
let ``Typechecker: division`` () =
    let result = testParser "division.crn"
    let result = result |> Result.bind Typechecker.typecheck 
    Assert.True(Result.isOk result, result.ToString())


[<Fact>]
let ``Typechecker: euler`` () =
    let result = testParser "euler.crn"
    let result = result |> Result.bind Typechecker.typecheck 

    Assert.True(Result.isOk result, result.ToString())


[<Fact>]
let ``Typechecker: gcd`` () =
    let result = testParser "gcd.crn"
    let result = result |> Result.bind Typechecker.typecheck 

    Assert.True(Result.isOk result, result.ToString())


[<Fact>]
let ``Typechecker: isqrt`` () =
    let result = testParser "isqrt.crn"
    let result = result |> Result.bind Typechecker.typecheck 

    Assert.True(Result.isOk result, result.ToString())


[<Fact>]
let ``Typechecker: pi`` () =
    let result = testParser "pi.crn"
    let result = result |> Result.bind Typechecker.typecheck 
    Assert.True(Result.isOk result, result.ToString())
    

[<Fact>]
let ``Typechecker: subalt`` () =
    let result = testParser "subalt.crn"
    let result = result |> Result.bind Typechecker.typecheck 
    
    Assert.True(Result.isOk result, result.ToString())


[<Fact>]
let ``Typechecker: factorial`` () =
    let result = testParser "factorial.crn"
    let result = result |> Result.bind Typechecker.typecheck 

    Assert.True(Result.isOk result, result.ToString())

[<Fact>]
let ``Typechecker: illegal`` () =
    let result = testParser "illegal.crn"
    let result = result |> Result.bind Typechecker.typecheck 

    Assert.True(Result.isError result, result.ToString())


[<Fact>]
let ``Interpreter: counter`` () =
    let cons = Map.add "c0" 5.0 Map.empty
    let result = testParser "counter.crn"
                 |> Result.bind Typechecker.typecheck
                 |> Result.defaultValue ([],{Species = Set []; Consts = Set []})
                 |> Interpreter.interpreter cons 

    Assert.True(Seq.length result > 0)


