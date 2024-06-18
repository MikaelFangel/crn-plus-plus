module CRNTest.Common

open System
open CRN.Parser

let getTestFile name =
    let path = IO.Path.Combine("../../../../../examples/", name)
    IO.File.ReadAllText path

let testParser name =
    let file = getTestFile name
    tryParse file