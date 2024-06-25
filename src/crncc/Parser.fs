module CRN.Parser

open CRN.AST
open FParsec

let private ws = spaces
let private str_ws s = pstring s .>> ws
let private str_wso s = pstring s .>> pchar ' '

let private float_ws = pfloat .>> ws

let private reservedNames = 
    choice [
        str_wso "conc"
        str_wso "ld"
        str_wso "add"
        str_wso "sub"
        str_wso "mul"
        str_wso "div"
        str_wso "sqrt"
        str_wso "cmp"
        str_wso "ifEQ"
        str_wso "ifGT"
        str_wso "ifLT"
        str_wso "ifGE"
        str_wso "ifLE"
        str_wso "step"
        str_wso "crn"
    ]

let private identifier =
    let isFirstChar c = isLetter c
    let isChar c = isLetter c || isDigit c || c = '_'
    (notFollowedByL reservedNames "Unexpected reserved keyword") >>. many1Satisfy2L isFirstChar isChar "identifier" .>> ws

let private identifierOrReserved =
    let isFirstChar c = isLetter c || (c = '_')
    let isChar c = isLetter c || isDigit c || c = '_'
    (notFollowedBy reservedNames) >>. many1Satisfy2L isFirstChar isChar "identifier" .>> ws

let private pspecies = identifier |>> SpeciesS
let private pspeciesRes = identifierOrReserved |>> SpeciesS

let private pconst = identifier

let private pnumber = float_ws

let private pspecies2 = identifier |>> ExprSpecies.Species
let private pspecies2Reserved = identifierOrReserved |>> ExprSpecies.Species

let private pexpr = sepBy pspecies2 (str_ws "+")
let private pexprReserved = sepBy pspecies2Reserved (str_ws "+")

let private start_bracket bcopen start = str_ws start .>> str_ws bcopen

let private end_bracket bcclose = str_ws bcclose

let private comma = str_ws ","

let private brackets2 popen pclose t1 t2 cons =
    pipe2 (popen >>. t1) (comma >>. t2 .>> pclose) (fun v1 v2 -> cons (v1, v2))

let private brackets3 popen pclose t1 t2 t3 cons =
    pipe3 (popen >>. t1) (comma >>. t2) (comma >>. t3 .>> pclose) (fun v1 v2 v3 -> cons (v1, v2, v3))

let pvalue = choice [ pconst |>> ValueS.Literal; pnumber |>> ValueS.Number ]

let private pconc =
    brackets2 (start_bracket "[" "conc") (end_bracket "]") pspecies pvalue ConcS.Conc

let private pconcReserved =
    brackets2 (start_bracket "[" "conc") (end_bracket "]") pspeciesRes pvalue ConcS.Conc

let private brackets3species name =
    brackets3 (start_bracket "[" name) (end_bracket "]") pspecies pspecies pspecies

let private brackets2species name =
    brackets2 (start_bracket "[" name) (end_bracket "]") pspecies pspecies

let private pmoduleld = brackets2species "ld" ModuleS.Ld
let private pmoduleadd = brackets3species "add" ModuleS.Add
let private pmodulesub = brackets3species "sub" ModuleS.Sub
let private pmodulemul = brackets3species "mul" ModuleS.Mul
let private pmodulediv = brackets3species "div" ModuleS.Div
let private pmodulesqrt = brackets2species "sqrt" ModuleS.Sqrt
let private pmodulecmp = brackets2species "cmp" ModuleS.Cmp

let private pmodule =
    choice
        [ pmoduleld
          pmoduleadd
          pmodulesub
          pmodulemul
          pmodulediv
          pmodulesqrt
          pmodulecmp ]

let private prxn =
    brackets3 (start_bracket "[" "rxn") (end_bracket "]") pexpr pexpr pnumber ReactionS.Reaction
let private prxnReserved =
    brackets3 (start_bracket "[" "rxn") (end_bracket "]") pexprReserved pexprReserved pnumber ReactionS.Reaction



let private listparser popen pclose listelem =
    between popen pclose (sepBy listelem (str_ws ","))

let private pcon, pconref = createParserForwardedToRef<'a, 'u> ()

let private pcommand =
    choice
        [ pmodule |>> CommandS.Module
          prxn |>> CommandS.Reaction
          pcon |>> CommandS.Condition ]

let private commandopen start =
    str_ws start >>. str_ws "[" .>> str_ws "{"

let private commandclose = str_ws "}" .>> str_ws "]"

let private pcommandlist start =
    listparser (commandopen start) commandclose pcommand

let private pcmdif = pcommandlist "ifGT" |>> ConditionS.Gt
let private pcmdge = pcommandlist "ifGE" |>> ConditionS.Ge
let private pcmdeq = pcommandlist "ifEQ" |>> ConditionS.Eq
let private pcmdlt = pcommandlist "ifLT" |>> ConditionS.Lt
let private pcmdle = pcommandlist "ifLE" |>> ConditionS.Le

pconref.Value <- choice [ pcmdif; pcmdge; pcmdeq; pcmdlt; pcmdle ]

let private pstep = pcommandlist "step" |>> RootS.Step

let private proot = choice [ pstep; pconc |>> RootS.Conc ]

let private crnopen = str_ws "crn" >>. str_ws "=" .>> str_ws "{"

let private crnclose = str_ws "}"
let private curlyparser = listparser crnopen crnclose
let private pcrn = ws >>. curlyparser proot .>> eof |>> CrnS.Crn

/// Try and parse text into an untyped AST
let tryParse (str) : Result<UntypedAST, _> =
    match run pcrn str with
    | Success(output, _, _) -> Result.Ok output
    | Failure(errorMsg, _, _) -> Result.Error [ errorMsg ]

module RXN =
    let private rxncommand = prxnReserved |>> CommandS.Reaction

    let private pcommandlist start =
        listparser (commandopen start) commandclose rxncommand

    let private pstep = pcommandlist "step"

    let private proot = choice [ pstep |>> RootS.Step; pconcReserved |>> RootS.Conc ]

    let private pcrnrxn = ws >>. curlyparser proot .>> eof |>> CrnS.Crn
    /// Try and parse the text into an untyped AST but restrict to only
    /// reactions and concentration statements.
    let tryParse str : Result<UntypedAST, string> =
        match run pcrnrxn str with
        | Success(output, _, _) -> Result.Ok output
        | Failure(errorMsg, _, _) -> Result.Error errorMsg
