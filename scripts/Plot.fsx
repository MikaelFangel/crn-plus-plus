#r "nuget: Plotly.NET, 4.2.0"
#load "../src/CRN.Visualization/Library.fs"

open CRN.Visualization

let state =
    seq {
        yield (Map.ofList [ ("a", 1.0); ("b", 2.0) ])
        yield (Map.ofList [ ("a", 3.0); ("b", 4.0) ])
        yield (Map.ofList [ ("a", 3.0); ("b", 4.0) ])
        yield (Map.ofList [ ("a", 3.0); ("b", 4.0) ])
        yield (Map.ofList [ ("a", 3.0); ("b", 4.0) ])
        yield (Map.ofList [ ("a", 3.0); ("b", 4.0) ])
        yield (Map.ofList [ ("a", 3.0); ("b", 4.0) ])
        yield (Map.ofList [ ("a", 3.0); ("b", 4.0) ])
    }

let limit = 4
plotState (fun _ -> true) limit state
