#r "nuget: Unquote"
open Swensen.Unquote

let input =
    System.IO.File.ReadAllLines $"""{__SOURCE_DIRECTORY__}\input.txt"""
    |> List.ofSeq

let example =
    """ #.##..##.
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
        .Split("\n")
    |> Array.map (fun s -> s.Trim())
    |> List.ofSeq

module Array2D =
    let numberRows = Array2D.length1
    let numberColumns = Array2D.length2

let rec parse input =
    match input with
    | [] -> []
    | input ->
        let pattern = input |> List.takeWhile ((<>) "")
        let rest = input |> List.skip (pattern |> List.length)
        let parsed = pattern |> array2D

        match rest with
        | "" :: r -> parsed :: (parse r)
        | r -> parsed :: (parse r)

let splitVerticallyAt (pattern: 'a [,]) index =
    pattern[*, 0 .. (index - 1)], pattern[*, index..]

let mirrorVertically (pattern: 'a [,]) : 'a [,] = pattern[*, *]

let patterns = parse example
let pattern = patterns[0]
let verticalSplits = [ 1 .. (Array2D.numberColumns pattern - 1) ]

splitVerticallyAt pattern 1
|> (fun (left, right) -> (mirrorVertically left, right))

verticalSplits
|> List.map (splitVerticallyAt pattern)

let run () =
    printf "Testing.."
    test <@ 1 + 1 = 2 @>
    printfn "...done!"

run ()
