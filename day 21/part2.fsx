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

    let wrapGet (r, c) (arr: 'a [,]) =
        let mr, mc = r % (Array2D.length1 arr), c % (Array2D.length2 arr)

        let rr =
            if mr >= 0 then
                mr
            else
                ((Array2D.length1 arr) - 1) + (mr + 1)

        let rc =
            if mc >= 0 then
                mc
            else
                ((Array2D.length2 arr) - 1) + (mc + 1)

        //printfn "%A" (((Array2D.length1 arr), (Array2D.length2 arr)), (r, c), (rr, rc))

        arr[rr, rc]

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
    |> List.filter (fun loc -> Garden = (grid |> Array2D.wrapGet loc))


let rec steps (grid: Cell [,]) (curr: Set<int * int> list) n max =
    if n = max + 1 then
        curr
    else
        let next =
            curr
            |> List.head
            |> Seq.collect (reachable grid)
            |> Set.ofSeq

        if [ 64; 65; 196; 327 ] |> List.contains n then
            printfn "%d: %d" n (next |> Seq.length)

        steps grid (next :: curr) (n + 1) max

let solve input nsteps =
    let s, grid = parse input

    steps grid [ Set.singleton s ] nsteps

let run () =
    printf "Testing.."

    test <@ 1 = Array2D.wrapGet (0, 0) (array2D [ [ 1; 2 ]; [ 3; 4 ] ]) @>
    test <@ 2 = Array2D.wrapGet (0, 1) (array2D [ [ 1; 2 ]; [ 3; 4 ] ]) @>
    test <@ 3 = Array2D.wrapGet (1, 0) (array2D [ [ 1; 2 ]; [ 3; 4 ] ]) @>
    test <@ 4 = Array2D.wrapGet (1, 1) (array2D [ [ 1; 2 ]; [ 3; 4 ] ]) @>

    test <@ 1 = Array2D.wrapGet (0, 2) (array2D [ [ 1; 2 ]; [ 3; 4 ] ]) @>
    test <@ 2 = Array2D.wrapGet (0, 3) (array2D [ [ 1; 2 ]; [ 3; 4 ] ]) @>
    test <@ 1 = Array2D.wrapGet (2, 0) (array2D [ [ 1; 2 ]; [ 3; 4 ] ]) @>
    test <@ 3 = Array2D.wrapGet (3, 0) (array2D [ [ 1; 2 ]; [ 3; 4 ] ]) @>
    test <@ 1 = Array2D.wrapGet (2, 2) (array2D [ [ 1; 2 ]; [ 3; 4 ] ]) @>

    test <@ 2 = Array2D.wrapGet (0, -1) (array2D [ [ 1; 2 ]; [ 3; 4 ] ]) @>
    test <@ 1 = Array2D.wrapGet (0, -2) (array2D [ [ 1; 2 ]; [ 3; 4 ] ]) @>
    test <@ 3 = Array2D.wrapGet (-1, 0) (array2D [ [ 1; 2 ]; [ 3; 4 ] ]) @>
    test <@ 1 = Array2D.wrapGet (-2, 0) (array2D [ [ 1; 2 ]; [ 3; 4 ] ]) @>
    test <@ 1 = Array2D.wrapGet (-2, -2) (array2D [ [ 1; 2 ]; [ 3; 4 ] ]) @>

    printfn "...done!"

run ()

let s, grid = parse input
let counts = steps grid [ Set.singleton s ] 1 327
//65, 196, 327
