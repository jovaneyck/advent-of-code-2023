#r "nuget: Unquote"
open Swensen.Unquote

let input =
    System.IO.File.ReadAllLines $"""{__SOURCE_DIRECTORY__}\input.txt"""
    |> List.ofSeq

let example =
    """ .|...\....
        |.-.\.....
        .....|-...
        ........|.
        ..........
        .........\
        ..../.\\..
        .-.-/..|..
        .|....-|.\
        ..//.|...."""
        .Split("\n")
    |> Array.map (fun s -> s.Trim())
    |> List.ofSeq

type Location = int * int
type Grid = Map<Location, char>

module Grid =
    let at location grid = grid |> Map.tryFind location

type Direction =
    | L
    | R
    | U
    | D

type Beam =
    { Location: Location
      Direction: Direction }

let parse (input: string list) : Grid =
    [ for (rowNum, row) in (input |> Seq.indexed) do
          for (colNum, cell) in row |> Seq.indexed -> ((colNum, rowNum), cell) ]
    |> Map.ofSeq

let nextLocation (beam: Beam) : Location =
    let (x, y) = beam.Location

    match beam.Direction with
    | R -> (x + 1, y)
    | L -> (x - 1, y)
    | D -> (x, y + 1)
    | U -> (x, y - 1)

let nextDirection (direction: Direction) mirror : Direction =
    match mirror, direction with
    | '/', U -> R
    | '/', D -> L
    | '/', R -> U
    | '/', L -> D
    | '\\', U -> L
    | '\\', D -> R
    | '\\', R -> D
    | '\\', L -> U
    | _, d -> d

let rec emitBeams grid visited beams =
    match beams with
    | [] -> visited
    | beam :: bs ->
        let next = nextLocation beam

        if visited |> Set.contains (next, beam.Direction) then
            emitBeams grid visited bs
        else
            match grid |> Grid.at next with
            | None -> emitBeams grid visited bs
            | Some '.' ->
                let nextVisited = visited |> Set.add (next, beam.Direction)
                emitBeams grid nextVisited ({ beam with Location = next } :: bs)
            | Some '/' ->
                let nextVisited = visited |> Set.add (next, beam.Direction)
                let nextDirection = nextDirection beam.Direction '/'

                emitBeams
                    grid
                    nextVisited
                    ({ Location = next
                       Direction = nextDirection }
                     :: bs)
            | Some '\\' ->
                let nextVisited = visited |> Set.add (next, beam.Direction)
                let nextDirection = nextDirection beam.Direction '\\'

                emitBeams
                    grid
                    nextVisited
                    ({ Location = next
                       Direction = nextDirection }
                     :: bs)
            | Some '|' ->
                let nextVisited = visited |> Set.add (next, beam.Direction)

                if [ U; D ] |> List.contains beam.Direction then
                    let nextVisited = visited |> Set.add (next, beam.Direction)
                    emitBeams grid nextVisited ({ beam with Location = next } :: bs)
                else
                    emitBeams
                        grid
                        nextVisited
                        ({ Location = next; Direction = U }
                         :: { Location = next; Direction = D } :: bs)
            | Some '-' ->
                let nextVisited = visited |> Set.add (next, beam.Direction)

                if [ L; R ] |> List.contains beam.Direction then
                    let nextVisited = visited |> Set.add (next, beam.Direction)
                    emitBeams grid nextVisited ({ beam with Location = next } :: bs)
                else
                    emitBeams
                        grid
                        nextVisited
                        ({ Location = next; Direction = L }
                         :: { Location = next; Direction = R } :: bs)
            | err -> failwithf "Unknown cell: %A" err

let calculateNbEnergizedTilesFor grid startingBeam =
    let visited = Set.empty
    let visitedCells = emitBeams grid visited [ startingBeam ]
    let uniqueVisitedLocations = visitedCells |> Set.map fst
    let nbEnergizedTiles = uniqueVisitedLocations |> Set.count
    nbEnergizedTiles

let solve input =
    let dimension = input |> Seq.length

    let leftEdge =
        [ 0 .. (dimension - 1) ]
        |> List.map (fun row -> { Location = (-1, row); Direction = R })

    let rightEdge =
        [ 0 .. (dimension - 1) ]
        |> List.map (fun row ->
            { Location = (dimension, row)
              Direction = L })

    let topEdge =
        [ 0 .. (dimension - 1) ]
        |> List.map (fun col -> { Location = (col, -1); Direction = D })

    let bottomEdge =
        [ 0 .. (dimension - 1) ]
        |> List.map (fun col ->
            { Location = (col, dimension)
              Direction = D })

    let startingBeams = leftEdge @ rightEdge @ topEdge @ bottomEdge
    let grid = parse input

    let nbEnergizedTiles =
        startingBeams
        |> List.map (calculateNbEnergizedTilesFor grid)
        |> List.max

    nbEnergizedTiles


let run () =
    printf "Testing.."
    test <@ nextLocation { Location = (0, 0); Direction = D } = (0, 1) @>
    test <@ solve example = 51 @>
    printfn "...done!"

run ()

#time
//Real: 00:00:13.828, CPU: 00:00:15.718, GC gen0: 186, gen1: 2, gen2: 1
solve input
