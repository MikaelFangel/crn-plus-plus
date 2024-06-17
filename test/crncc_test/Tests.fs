module Tests

open Xunit
open System
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
    Assert.True(result.IsSome, result.ToString())

[<Fact>]
let ``Parser: division.crn`` () =
    Assert.True((testParser "division.crn").IsSome)

[<Fact>]
let ``Parser: euler.crn`` () =
    Assert.True((testParser "euler.crn").IsSome)

[<Fact>]
let ``Parser: gcd.crn`` () =
    Assert.True((testParser "gcd.crn").IsSome)

[<Fact>]
let ``Parser: isqrt.crn`` () =
    Assert.True((testParser "isqrt.crn").IsSome)

[<Fact>]
let ``Parser: pi.crn`` () =
    let result = testParser "pi.crn"
    Assert.True(result.IsSome, result.ToString())

[<Fact>]
let ``Parser: subalt.crn`` () =
    Assert.True((testParser "subalt.crn").IsSome)

[<Fact>]
let ``Parser: factorial.crn`` () =
    Assert.True((testParser "factorial.crn").IsSome)
