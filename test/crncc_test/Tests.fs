module Tests

open Xunit
open CRN.Parser


let example =
    "crn = { 
    conc[c, 5.0], conc[cInitial, 4.0],
    conc[one, 1], conc[zero, 0],
    step[{
        sub[c, one, cnext],
        cmp[c, zero]
    }],
    step[{
        ifGT[{ ld[cnext, c] }],
        ifLE[{ ld[cInitial, c] }]
    }]
}"

[<Fact>]
let ``My test`` () =

    let maybeast = tryParse example
    Assert.True(maybeast.IsSome)
