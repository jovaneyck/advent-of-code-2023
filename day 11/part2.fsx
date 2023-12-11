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
type GalaxyMap = (int64 * int64) list

let parse input : GalaxyMap =
    let galaxies =
        [ for (rownum, row) in input |> List.indexed do
              for (colnum, cell) in row |> Seq.toList |> List.indexed do
                  if cell = '#' then
                      (int64 colnum, int64 rownum) ]

    galaxies

let findEmptyColsRows map =
    let min = 0L

    let occupiedColumns = map |> List.map fst
    let maxCols = occupiedColumns |> List.max |> int64
    let emptyColumns = [ min..maxCols ] |> List.except occupiedColumns

    let occupiedRows = map |> List.map snd
    let maxRows = occupiedRows |> List.max |> int64
    let emptyRows = [ min..maxRows ] |> List.except occupiedRows


    (emptyColumns, emptyRows)

let offset = 1000000L

let expandCols empties (gc, gr) =
    let diff =
        empties
        |> List.filter (fun c -> c < gc)
        |> List.length
        |> int64

    let offset = (offset - 1L) * diff
    (gc + offset, gr)

let expandRows empties (gc, gr) =
    let diff =
        empties
        |> List.filter (fun r -> r < gr)
        |> List.length
        |> int64

    let offset = (offset - 1L) * diff
    (gc, gr + offset)

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
