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
type Brick = Coordinate * Coordinate

module Brick =
    let bottom (((_, _, z1), (_, _, z2)): Brick) = min z1 z2

    let project (((x1, y1, _), (x2, y2, _)): Brick) : TwoDCoordinate list =
        let [ x1; x2 ] = [ x1; x2 ] |> List.sort
        let [ y1; y2 ] = [ y1; y2 ] |> List.sort

        [ for x in x1..x2 do
              for y in y1..y2 -> (x, y) ]

    let minHeight (((x1, y1, z1), (x2, y2, z2)): Brick) =
        let [ z1; z2 ] = [ z1; z2 ] |> List.sort
        min z1 z2

    let maxHeightAt ((x, y): TwoDCoordinate) (((x1, y1, z1), (x2, y2, z2)): Brick) =
        let [ x1; x2 ] = [ x1; x2 ] |> List.sort
        let [ y1; y2 ] = [ y1; y2 ] |> List.sort

        if x1 <= x && x <= x2 && y1 <= y && y <= y2 then
            max z1 z2 |> Some
        else
            None

    let lower z (((x1, y1, z1), (x2, y2, z2)): Brick) =
        if z1 < z2 then
            ((x1, y1, z), (x2, y2, z2 - (z1 - z)))
        else
            ((x1, y1, z1 - (z2 - z)), (x2, y2, z))


    let points (((x1, y1, z1), (x2, y2, z2)): Brick) : Coordinate list =
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


let parse (line: string) : Brick =
    let [| p1; p2 |] = line.Split("~")
    let [| x1; y1; z1 |] = p1.Split(",") |> Array.map int
    let [| x2; y2; z2 |] = p2.Split(",") |> Array.map int
    ((x1, y1, z1), (x2, y2, z2))

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
    test <@ parse "0,0,4~0,2,4" = ((0, 0, 4), (0, 2, 4)) @>

    test <@ Brick.project ((1, 2, 3), (1, 2, 3)) = [ (1, 2) ] @>
    test <@ Brick.project ((3, 2, 3), (1, 2, 3)) = [ (1, 2); (2, 2); (3, 2) ] @>

    test
        <@ drop [ ((1, 0, 1), (1, 2, 1)) ] [
            ((0, 0, 2), (2, 0, 2))
           ] = [ ((0, 0, 2), (2, 0, 2))
                 ((1, 0, 1), (1, 2, 1)) ] @>

    test
        <@ drop [ ((0, 1, 6), (2, 1, 6)) ] [
            ((1, 1, 8), (1, 1, 9))
           ] = [ ((1, 1, 7), (1, 1, 8))
                 ((0, 1, 6), (2, 1, 6)) ] @>

    printfn "...done!"

run ()

let bricks = example |> List.map parse
let sorted = bricks |> List.sortBy Brick.bottom

let tower =
    drop [] sorted
    |> List.sortByDescending Brick.bottom

let lines =
    tower
    |> List.map (fun b -> [ fst b; snd b ])
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
        |> Map.map (fun b bricks -> bricks |> List.except [ brick ])

    let hasFloatingBricks (support: Support) =
        support
        |> Map.filter (fun brick v -> brick |> Brick.minHeight <> 1)
        |> Map.toList
        |> List.map snd
        |> List.exists List.isEmpty

let support: Support =
    tower
    |> List.map (fun brick -> (brick, Tower.support tower brick))
    |> Map.ofSeq

let validDisintegrations =
    tower
    |> List.map (fun b -> b, support |> Support.disintegrate b)
    |> List.filter (fun (b, s) -> Support.hasFloatingBricks s |> not)

validDisintegrations |> List.length
