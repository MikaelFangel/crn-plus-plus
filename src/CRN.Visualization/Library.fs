module CRN.Visualization

open Plotly.NET
open Plotly.NET.LayoutObjects

type State = Map<string, float>
type Dimension = float * float

// Converts a state to a list of points for a given species
let private convertState limit (state: State seq) species =
    let points =
        state |> Seq.take limit |> Seq.mapi (fun i s -> (i, s.[species])) |> Seq.toList

    (species, points)

// Creates a line plot for a species
let private plotSpecies (species, points) =
    let x = points |> List.map fst
    let y = points |> List.map snd

    Chart.Line(x = x, y = y, Name = species, ShowMarkers = false)

// PLost the state of a CRN up to a given limit with a specific size
let plotStateWithSize f ((width, height): Dimension) limit (state: State seq) =
    match Seq.tryHead state with
    | Some(s) ->
        let species = s |> Map.keys |> Seq.filter f |> Seq.toList

        species
        |> List.map (convertState limit state)
        |> List.map plotSpecies
        |> Chart.combine
        |> Chart.withSize (width, height)
        |> Chart.withXAxis (LinearAxis.init (RangeMode = StyleParam.RangeMode.ToZero))
        |> Chart.withYAxis (
            LinearAxis.init (Title = Title.init (Text = "Concentration"), RangeMode = StyleParam.RangeMode.ToZero)
        )
        |> Chart.show
    | None -> failwith "State is empty"

// Plot the state of a CRN up to a given limit where the f function is used to filter species
let plotState f limit state =
    plotStateWithSize f (1200.0, 800.0) limit state
