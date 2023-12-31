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
    let nbCols = m |> Array2D.length2
    let nbRows = m |> Array2D.length1
    let initializer row col = m[row, nbCols - 1 - col]
    Array2D.init nbRows nbCols initializer

let horizontalFlip m =
    let nbCols = m |> Array2D.length2
    let nbRows = m |> Array2D.length1
    let initializer row col = m[nbRows - 1 - row, col]
    Array2D.init nbRows nbCols initializer

let trimVertically (patternA, patternB) =
    let minWidth =
        [ patternA; patternB ]
        |> List.map Array2D.length2
        |> List.min

    let widthA = patternA |> Array2D.length2
    (patternA[*, (widthA - minWidth) .. (widthA - 1)], patternB[*, 0 .. (minWidth - 1)])

let trimHorizontally (patternA, patternB) =
    let minHeight =
        [ patternA; patternB ]
        |> List.map Array2D.length1
        |> List.min

    let heightA = patternA |> Array2D.length1
    (patternA[(heightA - minHeight) .. (heightA - 1), *], patternB[0 .. (minHeight - 1), *])

type MirrorLocation =
    | Horizontal of int
    | Vertical of int

let mirrorLocations pattern =
    let hor =
        pattern
        |> horizontalSplits
        |> List.indexed
        |> List.map (fun (i, f) -> i + 1, trimHorizontally f)
        |> List.map (fun (i, (f, s)) -> i, (f, horizontalFlip s))
        |> List.filter (fun (i, (a, b)) -> a = b)
        |> List.map fst
        |> List.map Horizontal



    let vert =
        pattern
        |> verticalSplits
        |> List.indexed
        |> List.map (fun (i, f) -> i + 1, trimVertically f)
        |> List.map (fun (i, (f, s)) -> i, (f, verticalFlip s))
        |> List.filter (fun (i, (a, b)) -> a = b)
        |> List.map fst
        |> List.map Vertical


    hor @ vert |> Set.ofSeq

let flip =
    function
    | '.' -> '#'
    | '#' -> '.'

let smudges m =
    [ for row in 0 .. (Array2D.length1 m - 1) do
          for col in 0 .. (Array2D.length2 m - 1) do
              yield
                  m
                  |> Array2D.mapi (fun r c v ->
                      if r = row && c = col then
                          flip m[r, c]
                      else
                          m[r, c]) ]

let patterns = input |> parse

let locs =
    patterns
    |> List.map (fun p -> p, smudges p)
    |> List.map (fun (p, smuds) ->
        printfn "looking at %A" p
        let mirror = p |> mirrorLocations
        printfn "original mirror at %A" mirror
        let others = smuds |> List.map mirrorLocations |> Set.unionMany
        printfn "other mirrors at %A" (others |> Set.map string |> String.concat ",")
        Set.difference others mirror |> Seq.head)

let result =
    locs
    |> List.map (function
        | (Horizontal h) -> 100 * h
        | (Vertical v) -> v)
    |> List.sum

let run () =
    let m =
        array2D [ [ 1; 2; 3 ]
                  [ 4; 5; 6 ]
                  [ 7; 8; 9 ] ]

    printf "Testing.."

    test
        <@ horizontalSplits m = [ ([ [ 1; 2; 3 ] ] |> array2D, [ [ 4; 5; 6 ]; [ 7; 8; 9 ] ] |> array2D)
                                  ([ [ 1; 2; 3 ]; [ 4; 5; 6 ] ] |> array2D, [ [ 7; 8; 9 ] ] |> array2D) ] @>

    test
        <@ verticalSplits m = [ ([ [ 1 ]; [ 4 ]; [ 7 ] ] |> array2D, [ [ 2; 3 ]; [ 5; 6 ]; [ 8; 9 ] ] |> array2D)
                                ([ [ 1; 2 ]; [ 4; 5 ]; [ 7; 8 ] ] |> array2D, [ [ 3 ]; [ 6 ]; [ 9 ] ] |> array2D) ] @>

    printfn "...done!"

run ()
