#r "nuget: Unquote"
open Swensen.Unquote

//Helpful pointers:
//* Raycasting/Point-in-Polygon https://en.wikipedia.org/wiki/Point_in_polygon
//* Shoelace formula https://en.wikipedia.org/wiki/Shoelace_formula#Example
//* Pick's theorem https://en.wikipedia.org/wiki/Pick%27s_theorem

let input =
    System.IO.File.ReadAllLines $"""{__SOURCE_DIRECTORY__}\input.txt"""
    |> List.ofSeq

let example =
    """ R 6 (#70c710)
        D 5 (#0dc571)
        L 2 (#5713f0)
        D 2 (#d2c081)
        R 2 (#59c680)
        D 2 (#411b91)
        L 5 (#8ceee2)
        U 2 (#caa173)
        L 1 (#1b58a2)
        U 2 (#caa171)
        R 2 (#7807d2)
        U 3 (#a77fa3)
        L 2 (#015232)
        U 2 (#7a21e3)"""
        .Split("\n")
    |> Array.map (fun s -> s.Trim())
    |> List.ofSeq

let debug msg x =
    //printfn "%s: %A" msg x
    x

type Direction =
    | U
    | D
    | L
    | R

type Instruction = { Direction: Direction; Distance: int }

let parseInstruction (line: string) : Instruction =
    let tokens = line.Split(" ")

    let parseDirection =
        function
        | "L" -> L
        | "D" -> D
        | "U" -> U
        | "R" -> R

    { Direction = parseDirection (tokens[0])
      Distance = int tokens[1] }

let area points =
    //shoelace formula
    points
    |> List.pairwise
    |> List.map (fun ((x1, y1), (x2, y2)) -> x1 * y2 - y1 * x2)
    |> List.sum
    |> fun x -> (decimal x) / 2m

let nbTrenchCubes trench =
    let nbTrench =
        trench
        |> List.pairwise
        |> List.map (fun ((x, y), (a, b)) -> abs (x - a) + abs (y - b))
        |> List.sum

    nbTrench

let nbInternalCubes (nbOnBorder: int) (A: decimal) =
    //pick's theorem
    // A = i + b/2 - 1
    // i = A + 1 - b/2
    let i = A + 1m - (decimal nbOnBorder) / 2m |> int
    i

let totalCubes trench =
    let nbTrench = nbTrenchCubes trench |> debug "nbTrench"
    let totalArea = area trench |> debug "totalArea"

    let nbInternal =
        nbInternalCubes nbTrench totalArea
        |> debug "nbInternal"

    nbTrench + nbInternal

type Location = int * int
type Trench = Location list

let apply (i: Instruction) (x, y) =
    match i.Direction with
    | U -> (x, y + i.Distance)
    | D -> (x, y - i.Distance)
    | L -> (x - i.Distance, y)
    | R -> (x + i.Distance, y)

let folder (trench: Trench) (instruction: Instruction) : Trench =
    let nextLocation = apply instruction (trench |> List.head)
    nextLocation :: trench

let solve input =
    let instructions = input |> List.map parseInstruction
    let trench = instructions |> List.fold folder [ 0, 0 ]
    totalCubes trench

let run () =
    printf "Testing.."

    test
        <@ area [ (1, 6)
                  (3, 1)
                  (7, 2)
                  (4, 4)
                  (8, 5)
                  (1, 6) ] = 16.5m @>

    let square =
        [ (0, 0)
          (2, 0)
          (2, 2)
          (0, 2)
          (0, 0) ] //8 on trench, 1 internal

    test <@ nbTrenchCubes square = 8 @>
    test <@ area square = 4m @>
    test <@ nbInternalCubes 8 4m = 1 @>
    test <@ totalCubes square = 9 @>

    test <@ solve example = 62 @>
    printfn "...done!"

run ()

solve input
