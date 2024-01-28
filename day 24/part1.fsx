#r "nuget: Unquote"

open Swensen.Unquote
open System.Text.RegularExpressions

let input =
    System.IO.File.ReadAllLines $"""{__SOURCE_DIRECTORY__}\input.txt"""
    |> List.ofSeq

let example =
    """ 19, 13, 30 @ -2,  1, -2
        18, 19, 22 @ -1, -1, -2
        20, 25, 34 @ -2, -2, -4
        12, 31, 28 @ -1, -2, -1
        20, 19, 15 @  1, -5, -3"""
        .Split("\n")
    |> Array.map (fun s -> s.Trim())
    |> List.ofSeq

type ThreeDimensionalCoordinate = decimal * decimal * decimal

//y = ax + b
type TwoDimensionalLine = { A: decimal; B: decimal }

module TwoDimensionalLine =
    let X y line = (y - line.B) / line.A
    let Y x line = (line.A * x) + line.B

    let intersection { A = a; B = b } { A = c; B = d } : ThreeDimensionalCoordinate option =
        if (a - c) = 0m then
            None
        else
            let x = (d - b) / (a - c)
            let y = a * x + b
            Some(x, y, -1m)

type HailStone =
    { Location: ThreeDimensionalCoordinate
      Velocity: ThreeDimensionalCoordinate }

module HailStone =
    let to2DLine (hs: HailStone) : TwoDimensionalLine =
        let (x, y, _) = hs.Location
        let (dx, dy, _) = hs.Velocity

        let a = dy / dx
        let b = y - a * x
        { A = a; B = b }

let between (min, max) value = min <= value && value <= max
let inBox box (x, y, z) = x |> between box && y |> between box

let parse (line: string) : HailStone =
    let re = Regex("(?<x>.*), (?<y>.*), (?<z>.*) @ (?<dx>.*), (?<dy>.*), (?<dz>.*)")
    let m = re.Match(line)
    let get (key: string) = m.Groups[key].Value |> decimal

    { Location = (get "x", get "y", get "z")
      Velocity = (get "dx", get "dy", get "dz") }

let combinations (xs: 'a list) =
    let max = (xs |> List.length) - 1

    [ for i in 0..max do
          for j in (i + 1) .. max do
              xs[i], xs[j] ]

let run () =
    printf "Testing.."

    test
        <@ parse "229429688799267, 202127443693857, 379401089168938 @ 63, 129, -113" = { Location =
                                                                                             (229429688799267m,
                                                                                              202127443693857m,
                                                                                              379401089168938m)
                                                                                         Velocity = (63m, 129m, -113m) } @>

    test <@ combinations [ 1; 2; 3 ] = [ (1, 2); (1, 3); (2, 3) ] @>

    let testIntersection one other =
        let hone = (parse >> HailStone.to2DLine) one
        let hother = (parse >> HailStone.to2DLine) other
        let actual = TwoDimensionalLine.intersection hone hother
        actual

    let shouldEqual (expected: ThreeDimensionalCoordinate option) (actual: ThreeDimensionalCoordinate option) =
        match actual, expected with
        | None, None -> true
        | Some (x1, y1, z1), Some (x2, y2, z2) ->
            let eps = 0.001m

            abs (x1 - x2) < eps
            && abs (y1 - y2) < eps
            && abs (z1 - z2) < eps
        | _ -> false

    test
        <@ testIntersection "19, 13, 30 @ -2, 1, -2" "18, 19, 22 @ -1, -1, -2"
           |> shouldEqual (Some(14.333m, 15.333m, -1m)) @>

    test
        <@ testIntersection "19, 13, 30 @ -2, 1, -2" "20, 25, 34 @ -2, -2, -4"
           |> shouldEqual (Some(11.667m, 16.667m, -1m)) @>

    test
        <@ testIntersection "19, 13, 30 @ -2, 1, -2" "12, 31, 28 @ -1, -2, -1"
           |> shouldEqual (Some(6.2m, 19.4m, -1m)) @>

    test <@ inBox (7m, 27m) (14.333m, 15.333m, -1m) @>
    test <@ inBox (7m, 27m) (11.667m, 16.667m, -1m) @>
    test <@ inBox (7m, 27m) (6.2m, 19.4m, -1m) |> not @>
    printfn "...done!"

run ()

let hailstones = input |> List.map parse
let lines = hailstones |> List.map HailStone.to2DLine
let both = List.zip hailstones lines

let intersectionsInBox =
    both
    |> combinations
    |> List.choose (fun ((hs1, l1), (hs2, l2)) ->
        TwoDimensionalLine.intersection l1 l2
        |> Option.map (fun intersection -> intersection, hs1, l1, hs2, l2))
    |> List.filter
        //Only "move forward in time" aka apply velocity positively
        (fun ((x, y, z),
              { Location = hs1x, hs1y, hs1z
                Velocity = hs1dx, hs1dy, hs1dz },
              _,
              { Location = hs2x, hs2y, hs2z
                Velocity = hs2dx, hs2dy, hs2dz },
              _) ->
            (x - hs1x) * hs1dx >= 0m
            && (x - hs2x) * hs2dx >= 0m)
    |> List.filter (fun (intersection, hs1, l1, hs2, l2) -> inBox (200000000000000m, 400000000000000m) intersection)
    |> List.length
