#r "nuget: Unquote"
open Swensen.Unquote

let input =
    System.IO.File.ReadAllLines $"""{__SOURCE_DIRECTORY__}\input.txt"""
    |> List.ofSeq

let example =
    """ FF7FSF7F7F7F7F7F---7
        L|LJ||||||||||||F--J
        FL-7LJLJ||||||LJL-77
        F--JF--7||LJLJ7F7FJ-
        L---JF-JLJ.||-FJLJJ7
        |F|F-JF---7F7-L7L|7|
        |FFJF7L7F-JF7|JL---7
        7-L-JL7||F7|L7F-7F7|
        L.L7LFJ|||||FJL7||LJ
        L7JLJL-JLJLJL--JLJ.L"""
        .Split("\n")
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
    | 'S', _ -> true //Smelly smell.
    | '.', _ -> false //Smelly smell.
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
      Direction: Direction }

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

let rec buildCycle (grid: Grid) cycle (state: State) =
    let nextLoc = nextLocation state.Location state.Direction
    let nextPipe = grid[fst nextLoc, snd nextLoc]

    if nextPipe = 'S' then
        cycle
    else
        let nextDir = nextDirection state.Direction nextPipe

        let nextState =
            { Location = nextLoc
              Direction = nextDir }

        //printfn "I am at %A and will now go to %A" state nextState

        let nextCycle = nextLoc :: cycle
        buildCycle grid nextCycle nextState

let figureOutStartingPipe dirs =
    match dirs |> List.sort with
    | [ North; South ] -> '|'
    | [ West; East ] -> '-'
    | [ North; West ] -> 'J'
    | [ North; East ] -> 'L'
    | [ South; West ] -> '7'
    | [ South; East ] -> 'F'
    | err -> failwithf "I can't figure out the starting pipe given connecting directions %A" err

let findLoop input =
    let grid = parse input
    let start = grid |> Array2D.findIndex 'S'

    let startingNeighbours =
        neighbours start grid
        |> List.filter (fun (_, dir, pipe) -> pipe |> hasConnection (flip dir))

    let startingDirection =
        startingNeighbours
        |> List.map (fun (_, dir, _) -> dir)
        |> List.head

    let startingPipe =
        startingNeighbours
        |> List.map (fun (_, dir, _) -> dir)
        |> figureOutStartingPipe

    let cycle =
        buildCycle
            grid
            [ start ]
            { Location = start
              Direction = startingDirection }

    //!!!MUTABLE UPDATE!!!
    Array2D.set grid (fst start) (snd start) startingPipe

    cycle, grid

let run () =
    printf "Testing.."

    test <@ figureOutStartingPipe [ North; South ] = '|' @>
    test <@ figureOutStartingPipe [ West; East ] = '-' @>
    test <@ figureOutStartingPipe [ West; North ] = 'J' @>
    test <@ figureOutStartingPipe [ East; South ] = 'F' @>
    test <@ figureOutStartingPipe [ North; East ] = 'L' @>
    test <@ figureOutStartingPipe [ West; South ] = '7' @>

    printfn "...done!"

run ()

#time

//Let's look to the "right" first

//let cycle,grid = findLoop input

let cycle, grid = findLoop example
