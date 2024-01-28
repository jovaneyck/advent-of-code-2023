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

type ThreeDimensionalCoordinate = double * double * double

type HailStone =
    { Location: ThreeDimensionalCoordinate
      Velocity: ThreeDimensionalCoordinate }

let parse (line: string) : HailStone =
    let re = Regex("(?<x>.*), (?<y>.*), (?<z>.*) @ (?<dx>.*), (?<dy>.*), (?<dz>.*)")
    let m = re.Match(line)
    let get (key: string) = m.Groups[key].Value |> double

    { Location = (get "x", get "y", get "z")
      Velocity = (get "dx", get "dy", get "dz") }

let hailstones = input |> List.map parse

(*
rock: x,y,z @ dx,dy,dz
hailstone_i = ai,bi,ci @ di,ei,fi
equations:

x,y: (e2 - e1)x + (d1 - d2)y    + 0z            + (b1 - b2)dx   + (a2 - a1) dy  + 0 dz          = a2e2 - b2d2 - a1e1 + b1d1
x,z: (f2 - f1)x + 0y            + (d1 - d2)z    + (c1 - c2)dx   + 0 dy          + (a2 - a1) dz  = a2f2 - c2d2 - a1f1 + c1d1
y,z: 0x         + (f2 - f1)y    + (e1 - e2)z    + 0 dx          + (c1 - c2)dy   + (b2 - b1) dz  = b2f2 - c2e2 - b1f1 + c1e1

Use 2 different sets of hailstones (h1,h2) to get 6 equations in 6 variables
Solve A*x=b

*)

let generateEquations (h1: HailStone) (h2: HailStone) =
    let { Location = a1, b1, c1
          Velocity = d1, e1, f1 } =
        h1

    let { Location = a2, b2, c2
          Velocity = d2, e2, f2 } =
        h2

    let A =
        [ [ (e2 - e1)
            (d1 - d2)
            0.0
            (b1 - b2)
            (a2 - a1)
            0.0 ]
          [ (f2 - f1)
            0.0
            (d1 - d2)
            (c1 - c2)
            0.0
            (a2 - a1) ]
          [ 0.0
            (f2 - f1)
            (e1 - e2)
            0.0
            (c1 - c2)
            (b2 - b1) ] ]

    let b =
        [ a2 * e2 - b2 * d2 - a1 * e1 + b1 * d1
          a2 * f2 - c2 * d2 - a1 * f1 + c1 * d1
          b2 * f2 - c2 * e2 - b1 * f1 + c1 * e1 ]

    A, b

let A1, b1 = generateEquations hailstones.[0] hailstones.[1]
let A2, b2 = generateEquations hailstones.[1] hailstones.[2]

#r "nuget: MathNet.Numerics.FSharp, 5.0.0"

open MathNet.Numerics.LinearAlgebra
let A = matrix (A1 @ A2)

let b = vector (b1 @ b2)
let (x :: y :: z :: _) = A.Solve(b) |> Seq.map decimal |> Seq.toList // 1;-2;-2
let part2 = x + y + z
