#r "nuget: Unquote"
open Swensen.Unquote

let input =
    System.IO.File.ReadAllLines $"""{__SOURCE_DIRECTORY__}\input.txt"""
    |> List.ofSeq

let example =
    """ ...........
        .....###.#.
        .###.##..#.
        ..#.#...#..
        ....#.#....
        .##..S####.
        .##..#...#.
        .......##..
        .##.#.####.
        .##..##.##.
        ..........."""
        .Split("\n")
    |> Array.map (fun s -> s.Trim())
    |> List.ofSeq

type Cell =
    | Garden
    | Rock

module Array2D =
    let locationOf x arr =
        let [ hit ] =
            [ for r in 0 .. (Array2D.length1 arr - 1) do
                  for c in 0 .. (Array2D.length2 arr - 1) do
                      if arr[r, c] = x then yield (r, c) ]

        hit

    let tryGet (r, c) arr =
        if r < 0 then
            None
        else if c < 0 then
            None
        else if r > (Array2D.length1 arr - 1) then
            None
        else if c > (Array2D.length2 arr - 1) then
            None
        else
            (Some arr[r, c])

let parse input =
    let rawGrid = input |> array2D
    let start = rawGrid |> Array2D.locationOf 'S'

    let parsedGrid =
        rawGrid
        |> Array2D.map (function
            | '.' -> Garden
            | '#' -> Rock
            | 'S' -> Garden
            | err -> failwithf "Unknown cell: %A" err)

    start, parsedGrid

let reachable grid (r, c) =
    [ (r - 1, c)
      (r + 1, c)
      (r, c - 1)
      (r, c + 1) ]
    |> List.choose (fun loc ->
        match Array2D.tryGet loc grid with
        | Some Garden -> Some loc
        | _ -> None)


let rec steps (grid: Cell [,]) (curr: Set<int * int>) n =
    if n = 0 then
        curr
    else
        let next = curr |> Seq.collect (reachable grid) |> Set.ofSeq
        steps grid next (n - 1)

let solve input nsteps =
    let s, grid = parse input
    steps grid (Set.singleton s) nsteps |> Seq.length

let run () =
    printf "Testing.."

    test <@ solve example 6 = 16 @>

    printfn "...done!"

run ()
solve input 64
