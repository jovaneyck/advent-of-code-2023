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

type Cost = int
type Location = int * int
type NeighbourMap = ((Cost * Location) list) [,]

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

    let rec distancesTo visited g nodes node : (Cost * Location) list =
        let next = nextCells g node |> List.except visited

        let hits, misses =
            next
            |> List.partition (fun n -> nodes |> Seq.contains n)

        let nVisited = next @ visited

        let recHits =
            misses
            |> List.collect (fun m ->
                distancesTo nVisited g nodes m
                |> List.map (fun (w, l) -> (w + 1, l)))

        let weightedHits = hits |> List.map (fun n -> 1, n)
        weightedHits @ recHits

    let compact alreadyInterestingNodes (g: Grid) : NeighbourMap =
        let nodesWithChoice =
            [ for r in 0 .. (Array2D.length1 g - 1) do
                  for c in 0 .. (Array2D.length2 g - 1) do
                      if nextCells g (r, c) |> Seq.length > 2 then
                          yield (r, c) ]

        let nodes = alreadyInterestingNodes @ nodesWithChoice

        let distanceMap =
            nodes
            |> List.map (fun node -> node, distancesTo [ node ] g nodes node)
            |> Map.ofList

        let result =
            g
            |> Array2D.mapi (fun r c _ ->
                match distanceMap |> Map.tryFind (r, c) with
                | None -> []
                | Some weightedNodes -> weightedNodes)

        result

type Stack = (Location * Set<Location> * Cost) list

let rec longestPath (neighbours: NeighbourMap) finish (acc: int) (q: Stack) : int =
    match q with
    | [] -> acc
    | (current, visited, currentCost) :: rq ->
        if current = finish then
            let nextAcc =
                if currentCost > acc then
                    printfn "%d;%d" currentCost (q |> Seq.length)
                    currentCost
                else
                    acc

            longestPath neighbours finish nextAcc rq
        else
            let weightedNeighbours = neighbours[fst current, snd current]

            let next =
                weightedNeighbours
                |> List.filter (fun (_, n) -> visited |> Set.contains n |> not)

            let nvisited = visited |> Set.add current

            let nq: Stack =
                next
                |> Seq.map (fun (cost, n) -> (n, nvisited, currentCost + cost))
                |> Seq.toList

            longestPath neighbours finish acc (nq @ rq)

let grid =
    Grid.parse input
    |> Array2D.map (function
        | Slope _ -> Path
        | x -> x)

let start = (0, 1)
let finish = (Array2D.length1 grid - 1), (Array2D.length2 grid - 2)

let compacted = grid |> Grid.compact [ start; finish ]

#time
//example: 154
//Real: 00:00:46.480, CPU: 00:00:47.453, GC gen0: 931, gen1: 0, gen2: 0
//val result: int = 6874
let result = longestPath compacted finish 0 [ (start, Set.singleton start, 0) ]
