#r "nuget: Plotly.NET, 4.2.0"
#load "./Library.fs"

open CRN.Visualization

let state: State =
    seq {
        yield (1, Map.ofList [ ("a", 1.0); ("b", 2.0) ])
        yield (2, Map.ofList [ ("a", 3.0); ("b", 4.0) ])
        yield (3, Map.ofList [ ("a", 3.0); ("b", 4.0) ])
        yield (4, Map.ofList [ ("a", 3.0); ("b", 4.0) ])
        yield (5, Map.ofList [ ("a", 3.0); ("b", 4.0) ])
        yield (6, Map.ofList [ ("a", 3.0); ("b", 4.0) ])
        yield (7, Map.ofList [ ("a", 3.0); ("b", 4.0) ])
        yield (8, Map.ofList [ ("a", 3.0); ("b", 4.0) ])
    }

let limit = 4
plotState limit state
