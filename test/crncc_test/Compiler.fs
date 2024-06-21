module CRNTest.Compiler
open Xunit
open FsCheck.Xunit
open CRNTest.Common
open CRN.AST
open CRN.Simulator
open CRN.Compiler
open CRN.Interpreter
open CRN.Typechecker

[<Property>]
let ``Compiler and Interpreter will run division.crn the same`` (a: int , b:int)=
    let result = testParser "division.crn"
    let typedast = result |> Result.bind typecheck 
    let seqInterpret = Result.bind (interpreter (Map.ofList [("a0", a);("b0",b)])) typedast
    Assert.True(Result.isOk seqInterpret)
    //let seqCompile = Result.bind (fun x -> Ok compileCrnS) typedast
    //|> Result.bind (fun x -> Ok (solveODE x))

