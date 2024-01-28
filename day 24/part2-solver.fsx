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

type HailStone =
    { Location: ThreeDimensionalCoordinate
      Velocity: ThreeDimensionalCoordinate }

module HailStone =
    let parse (line: string) : HailStone =
        let re = Regex("(?<x>.*), (?<y>.*), (?<z>.*) @ (?<dx>.*), (?<dy>.*), (?<dz>.*)")
        let m = re.Match(line)
        let get (key: string) = m.Groups[key].Value |> decimal

        { Location = (get "x", get "y", get "z")
          Velocity = (get "dx", get "dy", get "dz") }

#r "nuget: Microsoft.Z3,4.8.11"
#load "lib/FsZ3/lib.fs"

open Microsoft.Z3.Real

let rx = Real("rx")
let ry = Real("ry")
let rz = Real("rz")
let rdx = Real("rdx")
let rdy = Real("rdy")
let rdz = Real("rdz")

let generateConstraintsFor n h =
    let ti = Real(sprintf "t%d" n)
    let (hs1x, hs1y, hs1z) = h.Location
    let (hs1dx, hs1dy, hs1dz) = h.Velocity

    [| rx + ti * rdx =. hs1x + ti * hs1dx
       ry + ti * rdy =. hs1y + ti * hs1dy
       rz + ti * rdz =. hs1z + ti * hs1dz |]

let constraints =
    input
    |> Seq.map HailStone.parse
    |> Seq.take 3
    |> Seq.indexed
    |> Seq.map (fun (i, hs) -> generateConstraintsFor i hs)
    |> Seq.collect id
    |> Seq.toArray

let (Microsoft.Z3.Api.SolveResult.Solution solution) =
    Microsoft.Z3.Bool.Z3.Solve constraints

let result (unknown: Real) =
    solution
    |> List.find (fun (symbol, _, _) -> symbol.ToString() = unknown.ToString())
    |> (fun (_, _, Microsoft.Z3.Api.Result.Const result) -> result.ToString() |> decimal)

let x = result rx
let y = result ry
let z = result rz

let part2 = x + y + z
