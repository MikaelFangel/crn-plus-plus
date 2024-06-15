#r "nuget:FParsec"

open FParsec

let test p str =
    match run p str with
    | Success(result, _, _) -> printfn "Success: %A" result
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg

test pfloat "1.25"

type SpeciesS = string
type PNumberS = PNumber of float

type ConcS = Conc of SpeciesS * PNumberS

type ExprS = Expr of list<SpeciesS>

type ReactionS = Reaction of ExprS * ExprS * PNumberS

type ModuleS =
    | Ld of SpeciesS * SpeciesS
    | Add of SpeciesS * SpeciesS * SpeciesS
    | Sub of SpeciesS * SpeciesS * SpeciesS
    | Mul of SpeciesS * SpeciesS * SpeciesS
    | Div of SpeciesS * SpeciesS * SpeciesS
    | Sqrt of SpeciesS * SpeciesS
    | Cmp of SpeciesS * SpeciesS

type ConditionS =
    | Gt of CommandS list
    | Ge of CommandS list
    | Eq of CommandS list
    | Lt of CommandS list
    | Le of CommandS list

and CommandS =
    | Reaction of ReactionS
    | Module of ModuleS
    | Condition of ConditionS

type StepS = Step of list<CommandS>

type RootS =
    | Conc of ConcS
    | Step of StepS

type CrnS = Crn of list<RootS>



let comment =
    let anythingExceptNewline = isNoneOf [ '\n'; '\r' ]
    skipString "//" >>. spaces >>. manySatisfy anythingExceptNewline

test comment "// hello world"

let ws = comment |>> ignore <|> spaces
let str_ws s = pstring s .>> ws

let float_ws = pfloat .>> ws

let identifier =
    let isFirstChar c = isLetter c || c = '_'
    let isChar c = isLetter c || isDigit c || c = '_'
    many1Satisfy2L isFirstChar isChar "identifier" .>> ws

test identifier "_blahblah123  "

let pspecies = identifier .>> ws |>> SpeciesS
let pnumber = float_ws |>> PNumber

let pexpr = sepBy pspecies (str_ws "+") .>> ws |>> Expr

let start_bracket bcopen start =
    str_ws start .>> str_ws bcopen

let end_bracket bcclose = str_ws bcclose

let comma = str_ws ","

let brackets2 popen pclose t1 t2 cons =
    pipe2 (popen >>. t1) (comma >>. t2 .>> pclose) (fun v1 v2 -> cons (v1, v2))

let brackets3 popen pclose t1 t2 t3 cons =
    pipe3 (popen >>. t1) (comma >>. t2) (comma >>. t3 .>> pclose) (fun v1 v2 v3 -> cons (v1, v2, v3))

let pconc =
    brackets2 (start_bracket "[" "conc") (end_bracket "]") pspecies pnumber ConcS.Conc

let brackets3species name =
    brackets3 (start_bracket "[" name) (end_bracket "]") pspecies pspecies pspecies

let brackets2species name =
    brackets2 (start_bracket "[" name) (end_bracket "]") pspecies pspecies

let pmoduleld = brackets2species "ld" Ld
let pmoduleadd = brackets3species "add" Add
let pmodulesub = brackets3species "sub" Sub
let pmodulemul = brackets3species "mul" Mul
let pmodulediv = brackets3species "div" Div
let pmodulesqrt = brackets2species "sqrt" Sqrt
let pmodulecmp = brackets2species "cmp" Cmp

let pmodule =
    choice
        [ pmoduleld
          pmoduleadd
          pmodulesub
          pmodulemul
          pmodulediv
          pmodulesqrt
          pmodulecmp ]

let prxn =
    brackets3 (start_bracket "[" "rxn") (end_bracket "]") pexpr pexpr pnumber ReactionS.Reaction

let listparser popen pclose listelem =
    between popen pclose (sepBy listelem (str_ws ","))

let pcon, pconref = createParserForwardedToRef<'a, 'u> ()

let pcommand =
    choice
        [ pmodule |>> CommandS.Module
          prxn |>> CommandS.Reaction
          pcon |>> CommandS.Condition ]

let commandopen start =
    str_ws start >>. str_ws "[" .>> str_ws "{"

let commandclose = str_ws "}" .>> str_ws "]"

let pcommandlist start =
    listparser (commandopen start) commandclose pcommand

let pcmdif = pcommandlist "ifGT" |>> Gt
let pcmdge = pcommandlist "ifGE" |>> Ge
let pcmdeq = pcommandlist "ifEQ" |>> Eq
let pcmdlt = pcommandlist "ifLT" |>> Lt
let pcmdle = pcommandlist "ifLE" |>> Le

pconref.Value <- choice [ pcmdif; pcmdge; pcmdeq; pcmdlt; pcmdle ]

let pstep = pcommandlist "step" |>> StepS.Step

let proot = choice [ pstep |>> RootS.Step; pconc |>> RootS.Conc ]

let crnopen =
    str_ws "crn" >>. str_ws "=" .>> str_ws "{"

let crnclose = str_ws "}"
let curlyparser = listparser crnopen crnclose
let pcrn = ws >>. curlyparser proot .>> eof |>> Crn

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

test pcrn example
