#r "nuget: Unquote"
open Swensen.Unquote

let input =
    System.IO.File.ReadAllLines $"""{__SOURCE_DIRECTORY__}\input.txt"""
    |> List.ofSeq

let trimExample (example: string) =
    example.Split("\n")
    |> Array.map (fun s -> s.Trim())
    |> List.ofSeq

//Array2D
//d1 = rows/vertical
//d2 = cols/horizontal
module Array2D =
    let findIndex el (arr: 'a [,]) =
        [ for d1 in 0 .. ((arr |> Array2D.length1) - 1) do
              for d2 in 0 .. ((arr |> Array2D.length2) - 1) do
                  if arr[d1, d2] = el then yield (d1, d2) ]
        |> Seq.head

type Pipe = char
type Grid = Pipe [,]

module Grid =
    let locationOf = Array2D.findIndex
    let at (r, c) grid = Array2D.get grid r c

let parse lines : Grid = array2D lines

type Direction =
    | North
    | South
    | West
    | East

let neighbours (row, col) (grid: Grid) =
    seq {
        if row > 0 then
            yield (row - 1, col), North, grid[row - 1, col]

        if row < ((grid |> Array2D.length1) - 1) then
            yield (row + 1, col), South, grid[row + 1, col]

        if col > 0 then
            yield (row, col - 1), West, grid[row, col - 1]

        if col < ((grid |> Array2D.length2) - 1) then
            yield (row, col + 1), East, grid[row, col + 1]
    }
    |> Seq.toList

let flip =
    function
    | North -> South
    | South -> North
    | West -> East
    | East -> West

let hasConnection (dir: Direction) (p: Pipe) =
    match p, dir with
    | 'S', _ -> false
    | '.', _ -> false
    | '-', West -> true
    | '-', East -> true
    | '|', North -> true
    | '|', South -> true
    | 'L', North -> true
    | 'L', East -> true
    | 'J', North -> true
    | 'J', West -> true
    | '7', West -> true
    | '7', South -> true
    | 'F', East -> true
    | 'F', South -> true
    | _ -> false

let nextLocation (x, y) direction =
    match direction with
    | East -> (x, y + 1)
    | West -> (x, y - 1)
    | North -> (x - 1, y)
    | South -> (x + 1, y)

type State =
    { Location: int * int
      Direction: Direction
      Cycle: ((int * int) * Pipe) list }

let nextDirection dir pipe =
    match pipe, dir with
    | 'S', _ -> failwithf "No no no, cannot figure out next direction for S, not enough information."
    | '.', _ -> failwithf "No no no, cannot figure out next direction for ., should never happen."
    | '-', West -> West
    | '-', East -> East
    | '|', North -> North
    | '|', South -> South
    | 'L', South -> East
    | 'L', West -> North
    | 'J', East -> North
    | 'J', South -> West
    | '7', East -> South
    | '7', North -> West
    | 'F', North -> East
    | 'F', West -> South
    | unknown -> failwithf "Unknown next direction: %A" unknown

let rec buildCycle startLoc (grid: Grid) (state: State) =
    let nextLoc = nextLocation state.Location state.Direction
    let nextPipe = grid |> Grid.at nextLoc

    if nextLoc = startLoc then
        state.Cycle
    else
        let nextDir = nextDirection state.Direction nextPipe

        let nextCycle = (nextLoc, nextPipe) :: state.Cycle

        let nextState =
            { Location = nextLoc
              Direction = nextDir
              Cycle = nextCycle }

        //printfn "I am at %A and will now go to %A" state nextState

        buildCycle startLoc grid nextState

let startingPiece neighbs =
    match neighbs |> List.sort with
    | [ North; South ] -> '|'
    | [ South; West ] -> '7'
    | [ South; East ] -> 'F'
    | err -> failwithf "TODO: %A" err

let findCycle input =
    let grid = parse input
    let start = grid |> Grid.locationOf 'S'

    let startingNeighbours =
        grid
        |> neighbours start
        |> List.filter (fun (_, dir, pipe) -> pipe |> hasConnection (flip dir))

    let startingDirection =
        startingNeighbours
        |> List.map (fun (_, dir, _) -> dir)
        |> List.head

    let startingPiece =
        startingNeighbours
        |> List.map (fun (_, dir, _) -> dir)
        |> startingPiece

    let cycle =
        buildCycle
            start
            grid
            { Location = start
              Direction = startingDirection
              Cycle = [ start, startingPiece ] }

    cycle

//Raycasting algorithm: cast a ray from loc to the right
//and count the number of times we hit "vertical parts" of the cycle.
type InOrOut =
    | In
    | Out

let raycast sortedCycle loc =
    let (row, col) = loc
    //printfn "Casting ray @ %A" loc

    let relevantPipesOnRay =
        sortedCycle
        |> List.filter (fun ((cr, cc), _) -> cr = row && col < cc)
        |> List.filter (fun (_, pipe) -> '-' <> pipe)
        |> List.map snd

    let (verticals, others) =
        relevantPipesOnRay
        |> List.partition (function
            | '|' -> true
            | _ -> false)

    let verticalCollisions = verticals |> List.length

    let isBendCollision =
        function
        | [ 'L'; '7' ]
        | [ 'F'; 'J' ] -> true
        | [ 'L'; 'J' ]
        | [ 'F'; '7' ] -> false
        | err -> failwithf "Unexpected bend pattern: %A" err

    let bendCollisions =
        others
        |> List.chunkBySize 2
        |> List.filter isBendCollision
        |> List.length

    let nbCollisions = verticalCollisions + bendCollisions
    if nbCollisions % 2 = 0 then Out else In

let solve (input: string list) =
    let cycle = findCycle input

    let dimension = input[0].Length

    let allCoords =
        [ for row in 0 .. (dimension - 1) do
              for col in 0 .. (dimension - 1) -> (row, col) ]

    let toCheck = allCoords |> List.except (cycle |> List.map fst)
    let sortedCycle = cycle |> List.sortBy fst

    toCheck
    |> List.map (fun c -> c, raycast sortedCycle c)
    |> List.filter (fun (_, io) ->
        match io with
        | In -> true
        | Out -> false)
    |> List.length

let run () =
    printf "Testing.."

    test
        <@ """  ...........
                .S-------7.
                .|F-----7|.
                .||.....||.
                .||.....||.
                .|L-7.F-J|.
                .|..|.|..|.
                .L--J.L--J.
                ..........."""
           |> trimExample
           |> solve = 4 @>

    test
        <@ """  .F----7F7F7F7F-7....
                .|F--7||||||||FJ....
                .||.FJ||||||||L7....
                FJL7L7LJLJ||LJ.L-7..
                L--J.L7...LJS7F-7L7.
                ....F-J..F7FJ|L7L7L7
                ....L7.F7||L7|.L7L7|
                .....|FJLJ|FJ|F7|.LJ
                ....FJL-7.||.||||...
                ....L---J.LJ.LJLJ..."""
           |> trimExample
           |> solve = 8 @>

    printfn "...done!"

run ()

#time
//Real: 00:00:02.624, CPU: 00:00:03.062, GC gen0: 21, gen1: 2, gen2: 1
solve input
