open CRN
open CRN.Parser
open CRN.Typechecker
open CRN.Simulator
open CRN.Interpreter
open CRN.Compiler
open CRN.Visualization.Library
open CRN.Visualization.TreeView

open CommandLine

let readFile path =
    match System.IO.File.Exists path with
    | true -> System.IO.File.ReadAllText path
    | _ -> failwithf "File not found: %s" path

let parseFile path =
    let text = readFile path

    match tryParse text with
    | Ok ast -> ast
    | Error err -> failwithf "%A" err

let constmap (input: string) =
    input.Split(';')
    |> Array.map (fun s -> s.Split('='))
    |> Array.map (fun kv -> kv.[0], float kv.[1])
    |> Map.ofArray

[<Verb("parse", HelpText = "Parse a .crn into an AST")>]
type ParserOptions =
    { [<Value(0, Required = true, HelpText = "Input file.")>]
      file: string
      [<Option('t', "tikz", Default = false, HelpText = "Convert AST to TikZ commands.")>]
      tikz: bool }

let runParse opts =
    let ast = parseFile opts.file

    if opts.tikz then
        Tikz.drawAST ast |> printfn "%s"

    0

[<Verb("typecheck", HelpText = "Typecheck the given .crn file")>]
type TypecheckerOptions =
    { [<Value(0, Required = true, HelpText = "Input file.")>]
      file: string }

let runTypecheck opts =
    let result = parseFile opts.file |> typecheck
    printfn "%A" result

    match result with
    | Ok _ -> 0
    | Error _ -> 1

[<Verb("interpret", HelpText = "Interpret the given .crn file")>]
type InterpreterOptions =
    { [<Value(0, Required = true, HelpText = "Input file.")>]
      file: string
      [<Option('i', "inputs", HelpText = "Additional inputs for the interpreter.")>]
      inputs: string
      [<Option('s', "steps", Default = 100, HelpText = "Number of steps to simulate.")>]
      steps: int
      [<Option('p', "plot", Default = false, HelpText = "Plot the simulation.")>]
      plot: bool
      [<Option('n', "names", Default = "", HelpText = "Input names of the species to show in the plot.")>]
      names: string }

let runInterpreter opts =
    let result = parseFile opts.file |> typecheck

    let constmap =
        match opts.inputs = null with
        | true -> Map.empty
        | _ -> constmap opts.inputs

    match result with
    | Ok env ->
        let result = interpreter constmap env

        match result with
        | Ok sequence ->
            if opts.plot then
                let species = opts.names.Split() |> Seq.ofArray |> Seq.filter (fun s -> s <> "")

                plotState
                    (fun s ->
                        if Seq.isEmpty species then
                            true
                        else
                            species |> Seq.contains s)
                    opts.steps
                    sequence
            else
                sequence |> Seq.take opts.steps |> printfn "%A"

            0
        | Error err ->
            eprintfn "%A" err
            1
    | Error _ -> 1

[<Verb("compile", HelpText = "Compile the given .crn file into a reaction network")>]
type CompileOptions =
    { [<Value(0, Required = true, HelpText = "Input file.")>]
      file: string
      [<Option('i', "inputs", HelpText = "Additional inputs for the compiler.")>]
      inputs: string }

let runCompiler opts =
    let typedAst = parseFile opts.file |> typecheck

    let constmap =
        match opts.inputs = null with
        | true -> Map.empty
        | _ -> constmap opts.inputs

    match typedAst with
    | Ok env ->
        let result = compile constmap env
        printfn "%A" result
        0

    | Error _ -> 1

[<Verb("simulate", HelpText = "Simulate the given .crn file as a reaction network")>]
type SimulatorOptions =
    { [<Value(0, Required = true, HelpText = "Input file.")>]
      file: string
      [<Option('i', "inputs", HelpText = "Additional inputs for the compiler.")>]
      inputs: string
      [<Option('s', "steps", Default = 10000, HelpText = "Number of steps to simulate.")>]
      steps: int
      [<Option('p', "plot", Default = false, HelpText = "Plot the simulation.")>]
      plot: bool
      [<Option('n', "names", Default = "", HelpText = "Input names of the species to show in the plot.")>]
      names: string }

let runSimulator opts =
    let typedAst = parseFile opts.file |> typecheck

    let constmap =
        match opts.inputs = null with
        | true -> Map.empty
        | _ -> constmap opts.inputs

    match typedAst with
    | Ok env ->
        let (env, rxn) = compile constmap env
        let sim = solveODE env 0.1 rxn
        let species = opts.names.Split() |> Seq.ofArray |> Seq.filter (fun s -> s <> "")
        printfn "%A" (Seq.isEmpty species)

        if opts.plot then
            plotState
                (fun s ->
                    if Seq.isEmpty species then
                        true
                    else
                        species |> Seq.contains s)
                opts.steps
                sim
        else
            printfn "%A" sim

        0
    | Error _ -> 1

[<EntryPoint>]
let main args =
    let result =
        Parser.Default.ParseArguments<
            ParserOptions,
            TypecheckerOptions,
            InterpreterOptions,
            CompileOptions,
            SimulatorOptions
         >
            args

    match result with
    | :? CommandLine.Parsed<obj> as command ->
        match command.Value with
        | :? ParserOptions as opts -> runParse opts
        | :? TypecheckerOptions as opts -> runTypecheck opts
        | :? InterpreterOptions as opts -> runInterpreter opts
        | :? CompileOptions as opts -> runCompiler opts
        | :? SimulatorOptions as opts -> runSimulator opts
    | :? CommandLine.NotParsed<obj> -> 1

