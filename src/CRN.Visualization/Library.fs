module CRN.Visualization

open Plotly.NET

type State = seq<int * Map<string, float>>

// Converts a state to a list of points for a given species
let internal convertState limit (state: State) species =
    let points =
        state
        |> Seq.takeWhile (fun s -> fst s <= limit)
        |> Seq.map (fun s -> (fst s, (snd s).[species]))
        |> Seq.toList

    (species, points)

// Creates a line plot for a species
let internal plotSpecies (species, points) =
    let x = points |> List.map fst
    let y = points |> List.map snd
    Chart.Line(x = x, y = y, Name = species, ShowMarkers = false)

// PLost the state of a CRN up to a given limit with a specific size
let plotStateWithSize (width: float) (height: float) limit (state: State) =
    match Seq.tryHead state with
    | Some(s) ->
        let species = s |> snd |> Map.keys |> Seq.toList

        species
        |> List.map (convertState limit state)
        |> List.map plotSpecies
        |> Chart.combine
        |> Chart.withSize (width, height)
        |> Chart.show
    | None -> failwith "State is empty"

// Plot the state of a CRN up to a given limit
let plotState limit state =
    plotStateWithSize 1200.0 800.0 limit state
