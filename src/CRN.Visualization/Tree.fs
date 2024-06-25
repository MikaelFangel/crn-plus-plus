module CRN.Visualization.Tree

type Tree<'a> = Node of 'a * (list<Tree<'a>>)

type Extent = (float * float) list

let internal moveTree =
    function
    | (Node((label, x), subtrees), x': float) -> Node((label, x + x'), subtrees)

let internal moveextent (e: Extent, x) =
    List.map (fun (p, q) -> (p + x, q + x)) e

let rec internal merge (e1: Extent) (e2: Extent) =
    match (e1, e2) with
    | ([], qs) -> qs
    | (ps, []) -> ps
    | ((p, _) :: ps, (_, q) :: qs) -> (p, q) :: merge ps qs

let internal mergelist es = List.fold merge [] es

let internal rmax (p: float, q: float) = if p > q then p else q

let rec internal fit l1 l2 =
    match l1, l2 with
    | (((_, p) :: ps), ((q, _) :: qs)) -> rmax (fit ps qs, p - q + 1.0)
    | (_, _) -> 0.0

let rec internal fitlistl es =
    let rec fitlistl' acc l =
        match acc, l with
        | _, [] -> []
        | acc, (e :: es) ->
            let x = fit acc e
            x :: fitlistl' (merge acc (moveextent (e, x))) es

    fitlistl' [] es

let rec internal fitlistr es =
    let rec fitlistr' acc l =
        match acc, l with
        | _, [] -> []
        | acc, (e :: es) ->
            let x = -(fit e acc)
            x :: fitlistr' (merge (moveextent (e, x)) acc) es

    List.rev (fitlistr' [] (List.rev es))

let internal mean = fun (x, y) -> (x + y) / 2.0

let internal fitlist =
    fun es -> List.map mean (List.zip (fitlistl es) (fitlistr es))


/// Given a Tree<'a>, return a Tree<'a * float> where the second element of the tuple of 
/// each node indicates the horizontal position of the given node relative to its parent
let rec design =
    function
    | Node(label, subtrees) ->
        let (trees, extents) = List.unzip (List.map design subtrees)
        let positions = fitlist extents
        let ptrees = List.map moveTree (List.zip trees positions)
        let pextents = List.map moveextent (List.zip extents positions)
        let resultExtent = (0.0, 0.0) :: mergelist pextents
        let resultTree = Node((label, 0.0), ptrees)
        (resultTree, resultExtent)

open CRN.AST
open CRN

type private ASTNode = 
    | NSpeciesS of SpeciesS
    | NPNumberS of PNumberS
    | NValueS of ValueS
    | NConcS of ConcS
    | NExprS of ExprS
    | NReactionS of ReactionS
    | NModuleS of ModuleS
    | NConditionS of ConditionS
    | NCommandS of CommandS
    | NRootS of RootS
    | NCrnS of CrnS

let rec private astToTree' (node: ASTNode) =
    match node with
    | NCrnS(CrnS.Crn(crn)) -> 
        Node("crn", (List.map astToTree' (List.map NRootS crn)) )
    | NRootS(RootS.Conc(ConcS.Conc(spec, value))) -> 
        Node("conc", [astToTree' (NSpeciesS spec); astToTree' (NValueS value)])
    | NRootS(RootS.Step(commands)) ->
        let ncommands = List.map NCommandS commands
        let nodes = List.map astToTree' ncommands
        Node("step", nodes)
    | NCommandS(command) ->
        match command with
        | Reaction reaction -> Node("rxn", [astToTree' (NReactionS reaction)]) 
        | Module modl -> Node("mod", [astToTree' (NModuleS modl)])
        | Condition condition   -> Node("cond", [astToTree' (NConditionS condition)])
    | NConditionS (condition) ->
        let name =
            match condition with
            | ConditionS.Eq _ -> "eq" 
            | ConditionS.Ge _ -> "ge"
            | ConditionS.Gt _ -> "gt"
            | ConditionS.Lt _ -> "lt"
            | ConditionS.Le _ -> "le"
        match condition with
        | ConditionS.Eq commands 
        | ConditionS.Ge commands
        | ConditionS.Gt commands
        | ConditionS.Lt commands
        | ConditionS.Le commands ->
            let ncommands = List.map NCommandS commands
            let nodes = List.map astToTree' ncommands
            Node(name, nodes)
    | NModuleS(modl) ->
        let name =
            match modl with
            | ModuleS.Ld _   -> "ld" 
            | ModuleS.Add _  -> "add"
            | ModuleS.Sub _  -> "sub"
            | ModuleS.Mul _  -> "mul"
            | ModuleS.Div _  -> "div"
            | ModuleS.Sqrt _ -> "sqrt"
            | ModuleS.Cmp _  -> "cmp"
        match modl with
        | ModuleS.Ld(arg1, arg2)
        | ModuleS.Sqrt (arg1, arg2)
        | ModuleS.Cmp (arg1, arg2)  ->
            Node(name, [astToTree' (NSpeciesS arg1); astToTree' (NSpeciesS arg2)])
        | ModuleS.Add(arg1, arg2, arg3) 
        | ModuleS.Sub(arg1, arg2, arg3) 
        | ModuleS.Mul(arg1, arg2, arg3) 
        | ModuleS.Div(arg1, arg2, arg3) ->
            Node(name, [astToTree' (NSpeciesS arg1); astToTree' (NSpeciesS arg2); astToTree' (NSpeciesS arg3)])
    | NReactionS(ReactionS.Reaction(exp1, exp2, num)) ->
        Node("rxn", [astToTree' (NExprS exp1); astToTree' (NExprS exp2); astToTree' (NPNumberS num)])
    | NExprS(species) ->
        let nspecies = List.map (fun spec -> NSpeciesS spec) species
        let nodes = List.map astToTree' nspecies
        Node("expr", nodes)
    | NConcS(ConcS.Conc(spec, value)) ->
        Node("conc", [astToTree' (NSpeciesS spec); astToTree' (NValueS value)])
    | NValueS(value) ->
        match value with
        | ValueS.Literal lit -> Node("val", [Node("lit", [Node(lit, [])])])
        | ValueS.Number num -> Node("val", [Node("num", [Node($"{num}", [])])])
    | NPNumberS(number) ->
        Node("num", [Node($"{number}", [])])
    | NSpeciesS(spec) ->
        Node("spec", [Node($"{spec}", [])])
        

let astToTree (ast): Tree<string> =
    NCrnS ast |> astToTree'
