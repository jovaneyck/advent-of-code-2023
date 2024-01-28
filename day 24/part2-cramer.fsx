#r "nuget: Unquote"

open System.Numerics //BigInteger as determinant calculation flies over the int64 range :O
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

type ThreeDimensionalCoordinate = BigInteger * BigInteger * BigInteger

type HailStone =
    { Location: ThreeDimensionalCoordinate
      Velocity: ThreeDimensionalCoordinate }

let parse (line: string) : HailStone =
    let re = Regex("(?<x>.*), (?<y>.*), (?<z>.*) @ (?<dx>.*), (?<dy>.*), (?<dz>.*)")
    let m = re.Match(line)

    let get (key: string) =
        m.Groups[key].Value |> int64 |> BigInteger

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
            0I
            (b1 - b2)
            (a2 - a1)
            0I ]
          [ (f2 - f1)
            0I
            (d1 - d2)
            (c1 - c2)
            0I
            (a2 - a1) ]
          [ 0I
            (f2 - f1)
            (e1 - e2)
            0I
            (c1 - c2)
            (b2 - b1) ] ]

    let b =
        [ a2 * e2 - b2 * d2 - a1 * e1 + b1 * d1
          a2 * f2 - c2 * d2 - a1 * f1 + c1 * d1
          b2 * f2 - c2 * e2 - b1 * f1 + c1 * e1 ]

    A, b

let A1, b1 = generateEquations hailstones.[0] hailstones.[1]
let A2, b2 = generateEquations hailstones.[1] hailstones.[2]
let A = A1 @ A2 |> array2D
let b = b1 @ b2 |> List.map (fun x -> [ x ]) |> array2D
(*
https://en.wikipedia.org/wiki/Cramer%27s_rule
Ax = b
xi = det(Ai) / det(A) where Ai = A where column i is replaced by vector b
*)

///Smooshes 2 matrices together. Very scientific :)
let smoosh (a: 'a [,]) (b: 'a [,]) =
    [ for r in 0 .. (Array2D.length1 a - 1) do
          yield Array.append a[r, *] b[r, *] ]
    |> array2D

///Splices an array2D on column <col>, returning the submatrix to the left and to the right
let splice (a: 'a [,]) col =
    let left = a[*, 0 .. col - 1]
    let right = a[*, (col + 1) ..]
    left, right

let rec det B =
    (*
        https://en.wikipedia.org/wiki/Laplace_expansion
    *)
    if B |> Array2D.length1 = 2 then
        B[0, 0] * B[1, 1] - B[0, 1] * B[1, 0]
    else
        let topRow = B[0, *] |> Array.indexed
        let restB = B[1.., *]

        let expanded =
            [ for (tri, trx) in topRow do
                  let left, right = splice restB tri
                  let sign = if tri % 2 = 0 then 1I else -1I
                  yield sign * trx, smoosh left right ]

        expanded
        |> List.map (fun (x, B) -> x * (det B))
        |> List.sum

let runT () =
    printf "Testing..."

    let B =
        array2D [ [ 1I; 2I; 3I ]
                  [ 4I; 5I; 6I ]
                  [ 7I; 8I; 9I ] ]

    test <@ det B = 0I @>
    printfn "..done!"

runT ()

let detA = det A

let lAx, rAx = splice A 0
let Ax = (smoosh (smoosh lAx b) rAx)
let lAy, rAy = splice A 1
let Ay = (smoosh (smoosh lAy b) rAy)
let lAz, rAz = splice A 2
let Az = (smoosh (smoosh lAz b) rAz)

let x = det Ax / detA
let y = det Ay / detA
let z = det Az / detA

let result = x + y + z
