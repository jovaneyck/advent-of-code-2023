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
        let falldown =
            function
            | Slope '>' -> (r, c + 1)
            | Slope '<' -> (r, c - 1)
            | Slope 'v' -> (r + 1, c)
            | Slope '^' -> (r - 1, c)
            | err -> failwithf "Problem! %A" err

        match grid[r, c] with
        | Forest -> failwith "I'm standing in a forest, should never happen"
        | Slope s -> [ falldown (Slope s) ]
        | Path ->
            next grid (r, c)
            |> List.filter (fun (pr, pc) ->
                match grid[pr, pc] with
                | Forest -> false
                | _ -> true)

let parseCell =
    function
    | '.' -> Path
    | '#' -> Forest
    | slope -> Slope slope

let parse input : Grid =
    input |> array2D |> Array2D.map parseCell

type Queue = ((int * int) * Set<int * int> * int) list

let rec longestPath grid finish (acc: int list) (q: Queue) : int =
    match q with
    | [] -> acc |> List.max
    | (current, visited, length) :: rq ->
        if current = finish then
            longestPath grid finish (length :: acc) rq
        else
            let next =
                Grid.nextCells grid current
                |> List.except (visited |> Set.toList)

            if next |> List.isEmpty then
                longestPath grid finish (length :: acc) rq
            else
                let nq: Queue =
                    next
                    |> List.map (fun n -> (n, visited |> Set.add current, 1 + length))

                longestPath grid finish acc (nq @ rq)

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

let grid = parse input
let start = (0, 1)
let finish = (Array2D.length1 grid - 1), (Array2D.length2 grid - 2)
let visited = Set.singleton start
let result = longestPath grid finish [] [ (start, visited, 0) ]
