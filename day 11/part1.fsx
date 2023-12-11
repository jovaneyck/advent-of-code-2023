#r "nuget: Unquote"
open Swensen.Unquote

let input =
    System.IO.File.ReadAllLines $"""{__SOURCE_DIRECTORY__}\input.txt"""
    |> List.ofSeq

let example =
    """ ...#......
        .......#..
        #.........
        ..........
        ......#...
        .#........
        .........#
        ..........
        .......#..
        #...#....."""
        .Split("\n")
    |> Array.map (fun s -> s.Trim())
    |> List.ofSeq

//X = horizontal = column
//Y = vertical = row
type GalaxyMap = (int * int) list

let parse input : GalaxyMap =
    let galaxies =
        [ for (rownum, row) in input |> List.indexed do
              for (colnum, cell) in row |> Seq.toList |> List.indexed do
                  if cell = '#' then (colnum, rownum) ]

    galaxies

let findEmptyColsRows map =
    let min = 0

    let occupiedColumns = map |> List.map fst
    let maxCols = occupiedColumns |> List.max
    let emptyColumns = [ min..maxCols ] |> List.except occupiedColumns

    let occupiedRows = map |> List.map snd
    let maxRows = occupiedRows |> List.max
    let emptyRows = [ min..maxRows ] |> List.except occupiedRows


    (emptyColumns, emptyRows)

let expandCols empties (gc, gr) =
    let diff =
        empties
        |> List.filter (fun c -> c < gc)
        |> List.length

    (gc + diff, gr)

let expandRows empties (gc, gr) =
    let diff =
        empties
        |> List.filter (fun r -> r < gr)
        |> List.length

    (gc, gr + diff)

let combinations xs =
    [ for (i, x) in xs |> List.indexed do
          for y in xs |> List.skip (i + 1) do
              (x, y) ]

let distance (g1c, g1r) (g2c, g2r) = (abs (g1c - g2c)) + (abs (g1r - g2r))

let map = parse input
let (ec, er) = findEmptyColsRows map

let expandedMap =
    map
    |> List.map (expandCols ec)
    |> List.map (expandRows er)

let pairs =
    expandedMap
    |> combinations
    |> List.map (fun (one, other) -> distance one other)

let result = pairs |> List.sum

let run () =
    printf "Testing.."

    printfn "...done!"

run ()
