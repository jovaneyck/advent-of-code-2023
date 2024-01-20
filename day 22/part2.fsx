#r "nuget: Unquote"
open Swensen.Unquote
#r "nuget: Plotly.NET"
open Plotly.NET


let input =
    System.IO.File.ReadAllLines $"""{__SOURCE_DIRECTORY__}\input.txt"""
    |> List.ofSeq

let example =
    """ 1,0,1~1,2,1
        0,0,2~2,0,2
        0,2,3~2,2,3
        0,0,4~0,2,4
        2,0,5~2,2,5
        0,1,6~2,1,6
        1,1,8~1,1,9"""
        .Split("\n")
    |> Array.map (fun s -> s.Trim())
    |> List.ofSeq

type TwoDCoordinate = int * int
type Coordinate = int * int * int
type Id = int

type Brick =
    { Id: Id
      Coords: Coordinate * Coordinate }

module Brick =
    let bottom ({ Coords = ((_, _, z1), (_, _, z2)) }: Brick) = min z1 z2

    let project ({ Coords = ((x1, y1, _), (x2, y2, _)) }: Brick) : TwoDCoordinate list =
        let [ x1; x2 ] = [ x1; x2 ] |> List.sort
        let [ y1; y2 ] = [ y1; y2 ] |> List.sort

        [ for x in x1..x2 do
              for y in y1..y2 -> (x, y) ]

    let minHeight ({ Coords = (((x1, y1, z1), (x2, y2, z2))) }: Brick) =
        let [ z1; z2 ] = [ z1; z2 ] |> List.sort
        min z1 z2

    let maxHeightAt ((x, y): TwoDCoordinate) ({ Coords = ((x1, y1, z1), (x2, y2, z2)) }: Brick) =
        let [ x1; x2 ] = [ x1; x2 ] |> List.sort
        let [ y1; y2 ] = [ y1; y2 ] |> List.sort

        if x1 <= x && x <= x2 && y1 <= y && y <= y2 then
            max z1 z2 |> Some
        else
            None

    let lower z (b: Brick) : Brick =
        let { Coords = ((x1, y1, z1), (x2, y2, z2)) } = b

        if z1 < z2 then
            { b with Coords = ((x1, y1, z), (x2, y2, z2 - (z1 - z))) }
        else
            { b with Coords = ((x1, y1, z1 - (z2 - z)), (x2, y2, z)) }


    let points (({ Coords = ((x1, y1, z1), (x2, y2, z2)) }: Brick)) : Coordinate list =
        let [ x1; x2 ] = [ x1; x2 ] |> List.sort
        let [ y1; y2 ] = [ y1; y2 ] |> List.sort
        let [ z1; z2 ] = [ z1; z2 ] |> List.sort

        [ for x in x1..x2 do
              for y in y1..y2 do
                  for z in z1..z2 -> (x, y, z) ]

type Tower = Brick list

module Tower =
    let maxZ (tower: Tower) (xy: TwoDCoordinate) =
        match tower |> List.choose (Brick.maxHeightAt xy) with
        | [] -> 0
        | hs -> hs |> List.max

    let support tower brick : Brick list =
        let top = brick |> Brick.bottom
        let xys = brick |> Brick.project

        let supportLocs =
            xys
            |> List.map (fun (x, y) -> (x, y, top - 1))
            |> Set.ofSeq

        tower
        |> List.filter (fun b ->
            b
            |> Brick.points
            |> Set.ofSeq
            |> Set.intersect supportLocs
            |> (fun i -> i |> Set.isEmpty |> not))


let parse (id: Id) (line: string) : Brick =
    let [| p1; p2 |] = line.Split("~")
    let [| x1; y1; z1 |] = p1.Split(",") |> Array.map int
    let [| x2; y2; z2 |] = p2.Split(",") |> Array.map int

    { Id = id
      Coords = ((x1, y1, z1), (x2, y2, z2)) }

let dropBrick tower brick =
    let xys = brick |> Brick.project
    let zbelow = xys |> List.map (Tower.maxZ tower) |> List.max
    let b = brick |> Brick.lower (zbelow + 1)
    b :: tower

let rec drop tower bricks =
    match bricks with
    | [] -> tower
    | b :: bs ->
        let tower' = dropBrick tower b
        drop tower' bs

let run () =
    printf "Testing.."

    test
        <@ parse 1 "0,0,4~0,2,4" = { Id = 1
                                     Coords = ((0, 0, 4), (0, 2, 4)) } @>

    test
        <@ Brick.project
            { Id = 1
              Coords = ((1, 2, 3), (1, 2, 3)) } = [ (1, 2) ] @>

    test
        <@ Brick.project
            { Id = 1
              Coords = ((3, 2, 3), (1, 2, 3)) } = [ (1, 2); (2, 2); (3, 2) ] @>


    printfn "...done!"

run ()

let render (tower: Tower) =
    let lines =
        tower
        |> List.map (fun b -> [ fst b.Coords; snd b.Coords ])
        |> List.map Chart3D.Chart.Line3D

    lines
    |> Chart.combine
    |> Chart.withLineStyle (Width = 20)
    |> Chart.withTitle "Tower"
    |> Chart.withLegend false
    |> Chart.show

type Support = Map<Brick, Brick list> //Maps bricks to all bricks that support it directly.

module Support =
    let disintegrate (brick: Brick) (support: Support) =
        support
        |> Map.map (fun _ bricks -> bricks |> List.except [ brick ])
        |> Map.remove brick

    let hasFloatingBricks (support: Support) =
        support
        |> Map.filter (fun brick v -> brick |> Brick.minHeight <> 1)
        |> Map.toList
        |> List.map snd
        |> List.exists List.isEmpty

let toTower (s: Support) : Tower =
    s
    |> Map.keys
    |> Seq.toList
    |> List.sortBy Brick.bottom

let countDifferences (t1: Tower) (t2: Tower) =
    t1
    |> List.filter (fun b -> t2 |> List.contains b |> not)
    |> List.length

let countNumberOfBricksThatWouldFallAfterDisintegrating tower (support: Support) (b: Brick) =
    support
    |> Support.disintegrate b
    |> toTower
    |> (drop [])
    |> (countDifferences tower)
    |> fun diffs -> diffs - 1

#time
//Let's solve!
let bricks = input |> List.mapi parse
let sorted = bricks |> List.sortBy Brick.bottom

let tower =
    drop [] sorted
    |> List.sortByDescending Brick.bottom

let support: Support =
    tower
    |> List.map (fun brick -> (brick, Tower.support tower brick))
    |> Map.ofSeq

let nbBricks = tower |> List.length
//Real: 00:06:28.202, CPU: 00:07:32.453, GC gen0: 26504, gen1: 0, gen2: 0
let part2 =
    tower
    |> List.mapi (fun i b ->
        let diff =
            (countNumberOfBricksThatWouldFallAfterDisintegrating tower support b)
            |> int64

        printfn "Processing brick %d of %d. %d other bricks would fall!" (i + 1) nbBricks diff
        diff)
    |> List.sum
//60023 too low
