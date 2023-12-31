#r "nuget: Unquote"
open Swensen.Unquote

let input =
    (System.IO.File.ReadAllText $"""{__SOURCE_DIRECTORY__}\input.txt""")
        .Replace("\r\n", "\n")

let example =
    """#.##..##.
..#.##.#.
##......#
##......#
..#.##.#.
..##..##.
#.#.##.#.

#...##..#
#....#..#
..##..###
#####.##.
#####.##.
..##..###
#....#..#"""

type Pattern = char [,]

let parsePattern pattern : Pattern = pattern |> array2D

let parse (input: string) =
    let rawPatterns = input.Split("\n\n") |> List.ofSeq

    rawPatterns
    |> List.map (fun s -> s.Split("\n"))
    |> List.map parsePattern

let verticalSplits m =
    let nbCols = m |> Array2D.length2
    [ for col in 0 .. (nbCols - 2) -> m[*, 0..col], m[*, (col + 1) .. nbCols] ]

let horizontalSplits m =
    let nbRows = m |> Array2D.length1
    [ for row in 0 .. (nbRows - 2) -> m[0..row, *], m[(row + 1) .. nbRows, *] ]

let verticalFlip m =
    let nbRows = m |> Array2D.length1
    let nbCols = m |> Array2D.length2
    let initializer row col = m[row, nbCols - 1 - col]
    Array2D.init nbRows nbCols initializer

let horizontalFlip m =
    let nbRows = m |> Array2D.length1
    let nbCols = m |> Array2D.length2
    let initializer row col = m[nbRows - 1 - row, col]
    Array2D.init nbRows nbCols initializer

let trim (patternA, patternB) =
    let heightA = patternA |> Array2D.length1
    let heightB = patternB |> Array2D.length1
    let widthA = patternA |> Array2D.length2
    let widthB = patternB |> Array2D.length2

    let minHeight = min heightA heightB
    let minWidth = min widthA widthB

    (patternA[(heightA - minHeight) .. (heightA - 1), (widthA - minWidth) .. (widthA - 1)],
     patternB[0 .. (minHeight - 1), 0 .. (minWidth - 1)])

type MirrorLocation =
    | Horizontal of int
    | Vertical of int

let mirrorLocations pattern =
    let hor =
        pattern
        |> horizontalSplits
        |> List.map trim
        |> List.mapi (fun i (f, s) -> i + 1, (f, horizontalFlip s))
        |> List.filter (fun (_, (a, b)) -> a = b)
        |> List.map fst
        |> List.map Horizontal

    let vert =
        pattern
        |> verticalSplits
        |> List.map trim
        |> List.mapi (fun i (f, s) -> i + 1, (f, verticalFlip s))
        |> List.filter (fun (_, (a, b)) -> a = b)
        |> List.map fst
        |> List.map Vertical

    hor @ vert |> Set.ofSeq

let flip =
    function
    | '.' -> '#'
    | '#' -> '.'

let smudges m =
    [ for row in 0 .. (Array2D.length1 m - 1) do
          for col in 0 .. (Array2D.length2 m - 1) ->
              m
              |> Array2D.mapi (fun r c v -> if r = row && c = col then flip v else v) ]

let solve input =
    let patterns = input |> parse

    let locs =
        patterns
        |> List.map (fun p -> p, smudges p)
        |> List.map (fun (p, smuds) ->
            let mirror = p |> mirrorLocations
            let others = smuds |> List.map mirrorLocations |> Set.unionMany
            Set.difference others mirror |> Seq.head)

    let result =
        locs
        |> List.map (function
            | (Horizontal h) -> 100 * h
            | (Vertical v) -> v)
        |> List.sum

    result

let run () =
    let m =
        array2D [ [ 1; 2; 3 ]
                  [ 4; 5; 6 ]
                  [ 7; 8; 9 ] ]

    let smaller = array2D [ [ 1; 2 ]; [ 3; 4 ] ]

    printf "Testing.."

    test
        <@ horizontalSplits m = [ ([ [ 1; 2; 3 ] ] |> array2D, [ [ 4; 5; 6 ]; [ 7; 8; 9 ] ] |> array2D)
                                  ([ [ 1; 2; 3 ]; [ 4; 5; 6 ] ] |> array2D, [ [ 7; 8; 9 ] ] |> array2D) ] @>

    test
        <@ verticalSplits m = [ ([ [ 1 ]; [ 4 ]; [ 7 ] ] |> array2D, [ [ 2; 3 ]; [ 5; 6 ]; [ 8; 9 ] ] |> array2D)
                                ([ [ 1; 2 ]; [ 4; 5 ]; [ 7; 8 ] ] |> array2D, [ [ 3 ]; [ 6 ]; [ 9 ] ] |> array2D) ] @>

    test
        <@ horizontalFlip m = array2D [ [ 7; 8; 9 ]
                                        [ 4; 5; 6 ]
                                        [ 1; 2; 3 ] ] @>

    test
        <@ verticalFlip m = array2D [ [ 3; 2; 1 ]
                                      [ 6; 5; 4 ]
                                      [ 9; 8; 7 ] ] @>

    test <@ trim (smaller, m) = (array2D [ [ 1; 2 ]; [ 3; 4 ] ], array2D [ [ 1; 2 ]; [ 4; 5 ] ]) @>
    test <@ trim (m, smaller) = (array2D [ [ 5; 6 ]; [ 8; 9 ] ], array2D [ [ 1; 2 ]; [ 3; 4 ] ]) @>

    test <@ solve example = 400 @>

    printfn "...done!"

run ()

#time //Real: 00:00:00.592, CPU: 00:00:00.687, GC gen0: 9, gen1: 0, gen2: 0
solve input
