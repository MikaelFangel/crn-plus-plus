module CRNTest.Parser

open Xunit

open CRNTest.Common
open CRN

open FsCheck
open FsCheck.Xunit

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
let ``Parser: reaction`` () =
    let result = testParser "reaction.crn"
    Assert.True(Result.isOk result, result.ToString() )

[<Fact>]
let ``Parser: Self test`` () =
    let result = testParser "factorial.crn"
    match result with
        | Ok (ast) -> 
            let ast2maybe = ast.ToString() |> Parser.tryParse
            match ast2maybe with
            Ok ast2 -> Assert.True ((ast = ast2))
            | Error _ -> Assert.Fail ""
        | Error (_) -> Assert.Fail ""

let speciesGen: Arbitrary<AST.SpeciesS>  =
    Arb.Default.String ()
    |> Arb.filter (fun str -> str <> null)
    |> Arb.filter (fun str -> str.Length > 0)
    |> Arb.mapFilter string 
        (fun str -> 
        str
        |> Seq.tryFind (fun c -> not(System.Char.IsLetter(c)))
        |> Option.isNone)
    
let pnumberGen: Gen<AST.PNumberS> =
    Gen.map NormalFloat.op_Explicit Arb.generate<NormalFloat>

let valueGen =
    let literal = Gen.map AST.ValueS.Literal speciesGen.Generator
    let number = Gen.map AST.ValueS.Number pnumberGen
    Gen.oneof [literal; number]

let concGen =
    Gen.map AST.ConcS.Conc (Gen.zip speciesGen.Generator valueGen)

type MyGenerators =
    static member float() = 
        { new Arbitrary<System.Double>() with
            override _.Generator = pnumberGen
            override _.Shrinker _ = Seq.empty
        }

    static member species() = speciesGen

    static member conc() =
        { new Arbitrary<CRN.AST.ConcS>() with
            override x.Generator = concGen
            override x.Shrinker f = seq []}


[<assembly: Properties( Arbitrary = [| typeof<MyGenerators> |] )>] do()

[<Property>]
let ``ToString is inverse of TryParse`` (ast: AST.UntypedAST) =
    ast.ToString() |> Parser.tryParse |> Result.bind (fun ast2 -> Ok (ast = ast2) ) |> Result.isOk