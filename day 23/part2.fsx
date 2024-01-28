#r "nuget: Unquote"
open Swensen.Unquote

let input =
    System.IO.File.ReadAllLines $"""{__SOURCE_DIRECTORY__}\input.txt"""
    |> List.ofSeq

let example =
    """ #.#####################
        #.......#########...###
        #######.#########.#.###
        ###.....#.>.>.###.#.###
        ###v#####.#v#.###.#.###
        ###.>...#.#.#.....#...#
        ###v###.#.#.#########.#
        ###...#.#.#.......#...#
        #####.#.#.#######.#.###
        #.....#.#.#.......#...#
        #.#####.#.#.#########v#
        #.#...#...#...###...>.#
        #.#.#v#######v###.###v#
        #...#.>.#...>.>.#.###.#
        #####v#.#.###v#.#.###.#
        #.....#...#...#.#.#...#
        #.#########.###.#.#.###
        #...###...#...#...#.###
        ###.###.#.###v#####v###
        #...#...#.#.>.>.#.>.###
        #.###.###.#.###.#.#v###
        #.....###...###...#...#
        #####################.#"""
        .Split("\n")
    |> Array.map (fun s -> s.Trim())
    |> List.ofSeq

type Cell =
    | Path
    | Forest
    | Slope of char

type Grid = Cell [,]

module Grid =
    let next grid (r, c) =
        [ if r > 0 then yield (-1, 0)
          if r < (Array2D.length1 grid - 1) then
              yield (1, 0)
          if c > 0 then yield (0, -1)
          if c < (Array2D.length2 grid - 1) then
              yield (0, 1) ]
        |> List.map (fun (dr, dc) -> (r + dr, c + dc))

    let nextCells (grid: Grid) (r, c) =
        match grid[r, c] with
        | Slope s -> failwith "Part 2 does not contain slopes!"
        | Forest -> []
        | Path ->
            next grid (r, c)
            |> List.filter (fun (pr, pc) ->
                match grid[pr, pc] with
                | Forest -> false
                | _ -> true)

let parse input : Grid =
    let parseCell =
        function
        | '.' -> Path
        | '#' -> Forest
        | slope -> Slope slope

    input |> array2D |> Array2D.map parseCell

type Stack = ((int * int) * Set<int * int> * int) list
type NeighbourMap = (Set<int * int>) [,]

let rec longestPath (neighbours: NeighbourMap) finish (acc: int) (q: Stack) : int =
    match q with
    | [] -> acc
    | (current, visited, length) :: rq ->
        if current = finish then
            let nextAcc =
                if length > acc then
                    printfn "%d;%d" length (q |> Seq.length)
                    length
                else
                    acc

            longestPath neighbours finish nextAcc rq
        else
            let next = Set.difference neighbours[fst current, snd current] visited
            let nlength = 1 + length
            let nvisited = visited |> Set.add current

            let nq: Stack =
                next
                |> Seq.map (fun n -> (n, nvisited, nlength))
                |> Seq.toList

            longestPath neighbours finish acc (nq @ rq)

let run () =
    printf "Testing.."

    let m =
        [ [ 1; 2; 3 ]
          [ 3; 4; 5 ]
          [ 5; 6; 7 ] ]
        |> array2D

    test <@ Grid.next m (1, 1) = [ (0, 1); (2, 1); (1, 0); (1, 2) ] @>
    test <@ Grid.next m (0, 0) = [ (1, 0); (0, 1) ] @>

    printfn "...done!"

run ()

let grid =
    parse input
    |> Array2D.map (function
        | Slope _ -> Path
        | x -> x)

let neighbours =
    grid
    |> Array2D.mapi (fun r c _ -> Grid.nextCells grid (r, c) |> Set.ofList)

let start = (0, 1)
let finish = (Array2D.length1 grid - 1), (Array2D.length2 grid - 2)

#time
//6434
let result = longestPath neighbours finish 0 [ (start, Set.singleton start, 0) ]
