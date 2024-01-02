#r "nuget: Unquote"
open Swensen.Unquote

let input =
    System.IO.File.ReadAllLines $"""{__SOURCE_DIRECTORY__}\input.txt"""
    |> List.ofSeq

let example =
    """ O....#....
        O.OO#....#
        .....##...
        OO.#O....O
        .O.....O#.
        O.#..O.#.#
        ..O..#O..O
        .......O..
        #....###..
        #OO..#...."""
        .Split("\n")
    |> Array.map (fun s -> s.Trim())
    |> List.ofSeq

module Array2D =
    let findIndices v m =
        [ for row in 0 .. (Array2D.length1 m - 1) do
              for col in 0 .. (Array2D.length2 m - 1) do
                  if m[row, col] = v then (row, col) ]

    let rotate (m: 'a [,]) =
        let nbRows = Array2D.length1 m
        let nbCols = Array2D.length2 m
        let rotated row col = m[nbRows - 1 - col, row]
        Array2D.init nbCols nbRows rotated

type Cell =
    | Empty
    | Wall
    | Rock

type Platform = Cell [,]

let parse =
    function
    | '.' -> Empty
    | '#' -> Wall
    | 'O' -> Rock
    | err -> failwithf "Could not parse <%c>" err

let tiltNorth platform =
    let tiltRockNorth (platform: Platform) (row, col) =
        let above =
            platform[0 .. row - 1, col]
            |> Array.rev
            |> Array.indexed

        let isBlocker =
            function
            | Wall
            | Rock -> true
            | Empty -> false

        let blocker =
            above
            |> Array.tryFind (fun (_, cell) -> isBlocker cell)

        match blocker with
        | None ->
            platform
            |> Array2D.mapi (fun pr pc v ->
                if pr = 0 && pc = col then Rock
                else if pr = row && pc = col then Empty
                else v)
        | Some (offset, _) ->
            platform
            |> Array2D.mapi (fun pr pc v ->
                if pr = (row - offset) && pc = col then
                    Rock
                else if pr = row && pc = col then
                    Empty
                else
                    v)

    let rockLocations = platform |> Array2D.findIndices Rock
    rockLocations |> List.fold tiltRockNorth platform

let cycle platform =
    platform
    |> tiltNorth
    |> Array2D.rotate
    |> tiltNorth
    |> Array2D.rotate
    |> tiltNorth
    |> Array2D.rotate
    |> tiltNorth
    |> Array2D.rotate

let rec findLoop visited n platform =
    let next = cycle platform

    match visited |> List.tryFind (fun (p, i) -> p = next) with
    | Some (p, i) -> next, n - i, i
    | None -> findLoop ((next, (n + 1)) :: visited) (n + 1) next

let rec run n platform =
    if n = 0 then
        platform
    else
        run (n - 1) (cycle platform)

let findLoad platform =
    let rockCounts =
        [ for row in 0 .. Array2D.length1 platform - 1 ->
              platform[row, *]
              |> Array.filter ((=) Rock)
              |> Array.length ]

    rockCounts
    |> List.rev
    |> List.mapi (fun i c -> i + 1, c)
    |> List.map (fun (i, c) -> i * c)
    |> List.sum

let platform: Platform =
    input
    |> Seq.map (fun row -> row |> Seq.map (fun cell -> parse cell))
    |> array2D

#time
//Using mutable array2D: Real: 00:00:52.462, CPU: 00:00:55.875, GC gen0: 425, gen1: 62, gen2: 2
//Using array2D copies:  Real: 00:01:36.792, CPU: 00:01:42.250, GC gen0: 2208, gen1: 118, gen2: 1
let (platformAfterLoop, loopLength, loopStart) =
    findLoop [ (platform, 0) ] 0 platform

let cyclesToRun = (1_000_000_000 - loopStart) % (loopLength + 1)
let finalPlatform = run cyclesToRun platformAfterLoop
finalPlatform |> findLoad

let runT () =
    printf "Testing.."

    test
        <@ array2D [ [ 1; 2 ]; [ 2; 3 ] ]
           |> Array2D.findIndices 2 = [ (0, 1); (1, 0) ] @>

    test
        <@ let m =
            array2D [ [ Rock ]
                      [ Empty ]
                      [ Rock ] ]

           let actual = m |> tiltNorth

           let expected =
               array2D [ [ Rock ]
                         [ Rock ]
                         [ Empty ] ]

           actual = expected @>

    test
        <@ let m = array2D [ [ 1; 2 ]; [ 3; 4 ] ]

           let actual = m |> Array2D.rotate

           let expected = array2D [ [ 3; 1 ]; [ 4; 2 ] ]

           actual = expected @>

    printfn "...done!"

runT ()
