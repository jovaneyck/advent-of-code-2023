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

type Cell =
    | Empty
    | Wall
    | Rock

type Platform = Cell [,]

type Direction =
    | North
    | East
    | South
    | West

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

let platform: Platform =
    input
    |> Seq.map (fun row -> row |> Seq.map (fun cell -> parse cell))
    |> array2D

#time
let tilted = platform |> tiltNorth

let rockCounts =
    [ for row in 0 .. Array2D.length1 tilted - 1 ->
          tilted[row, *]
          |> Array.filter ((=) Rock)
          |> Array.length ]

rockCounts
|> List.rev
|> List.mapi (fun i c -> i + 1, c)
|> List.map (fun (i, c) -> i * c)
|> List.sum

let run () =
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

    printfn "...done!"

run ()
