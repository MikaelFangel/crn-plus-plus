module CRNTest.Interpreter
open Xunit
open CRN.AST
open CRN.Simulator

open CRNTest.Common

[<Property>]
let ``New simulator works like old simulator``(ast:CRN.AST)