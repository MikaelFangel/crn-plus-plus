module CRNTest.Common

open System
open CRN.Parser

let getTestFile name =
    let path = IO.Path.Combine("../../../../../examples/", name)
    IO.File.ReadAllText path

let getTestFileIllFormed name =
    let path = IO.Path.Combine("../../../../../examples/ill_formed/", name)
    IO.File.ReadAllText path

let testParser name =
    let file = getTestFile name
    tryParse file

let testParserIllFormed name =
    let file = getTestFileIllFormed name
    tryParse file
