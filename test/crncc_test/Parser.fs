module CRNTest.Parser

open Xunit

open CRNTest.Common

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