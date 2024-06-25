open CRN
open CRN.Parser
open CRN.Typechecker
open CRN.Simulator
open CRN.Interpreter
open CRN.Compiler
open CRN.Visualization.TreeView

open CommandLine

[<Verb("parse", HelpText = "Parse a .crn into an AST")>]
type ParserOptions = {
    [<Value(0, Required = true, HelpText = "Input file.")>]file: string
    [<Option('t', "tikz", Default = false, HelpText= "Convert AST to TikZ commands.")>]tikz: bool
}

let runParse opts = 
    let text = System.IO.File.ReadAllText opts.file
    let result = tryParse text

    match result with
    | Ok ast -> 
        if opts.tikz then
            Tikz.drawAST ast
            |> printfn "%s"
        else
            printfn "%A" result
        0
    | Error err -> 
        eprintfn "%A" err
        1

[<Verb("typecheck", HelpText = "Typecheck the given .crn file")>]
type TypecheckerOptions = {
    [<Value(0, Required = true, HelpText = "Input file.")>]file: string
}

let runTypecheck opts = 
    let text = System.IO.File.ReadAllText opts.file
    let result = tryParse text |> Result.bind typecheck
    printfn "%A" result
    match result with
    | Ok _ -> 0
    | Error _ -> 1

[<Verb("interpret", HelpText = "Interpret the given .crn file")>]
type InterpreterOptions = {
    [<Value(0, Required = true, HelpText = "Input file.")>]file: string
    [<Option('i', "inputs", HelpText= "Additional inputs for the interpreter.")>]inputs: Map<string, float>
}

let runInterpreter opts =
    let text = System.IO.File.ReadAllText opts.file
    let result = tryParse text |> Result.bind typecheck
    let constmap = opts.inputs
    match result with
    | Ok env -> 
        let result = interpreter constmap env
        match result with
        | Ok sequence -> 
            sequence |> Seq.take 1 |> printfn "%A"
            0
        | Error err -> 
            eprintfn "%A" err
            1
    | Error _ -> 1

[<Verb("compile", HelpText = "Compile the given .crn file into a reaction network")>]
type CompileOptions = {
    [<Value(0, Required = true, HelpText = "Input file.")>]file: string
}

[<Verb("simulate", HelpText = "Simulate the given .crn file as a reaction network")>]
type SimulatorOptions = {
    [<Value(0, Required = true, HelpText = "Input file.")>]file: string
}

[<EntryPoint>]
let main args =
    let result = Parser.Default.ParseArguments<ParserOptions, TypecheckerOptions, InterpreterOptions, CompileOptions, SimulatorOptions> args
    match result with
    | :? CommandLine.Parsed<obj> as command ->
        match command.Value with
        | :? ParserOptions as opts -> runParse opts
        | :? TypecheckerOptions as opts -> runTypecheck opts
        | :? InterpreterOptions as opts -> runInterpreter opts
        | :? CompileOptions as opts -> failwith "unimplemented"
        | :? SimulatorOptions as opts -> failwith "unimplemented"
    | :? CommandLine.NotParsed<obj> -> 1