#r "nuget: Unquote"
open Swensen.Unquote
#r "nuget: FSharpx.Collections"
open FSharpx.Collections

let input =
    System.IO.File.ReadAllLines $"""{__SOURCE_DIRECTORY__}\input.txt"""
    |> List.ofSeq

let example =
    """ 2413432311323
        3215453535623
        3255245654254
        3446585845452
        4546657867536
        1438598798454
        4457876987766
        3637877979653
        4654967986887
        4564679986453
        1224686865563
        2546548887735
        4322674655533"""
        .Split("\n")
    |> Array.map (fun s -> s.Trim())
    |> List.ofSeq

///Dijkstra Priority Queue. Combines a (priority) Heap with a visited Set so we can update priorities of a node without having to iterate over the entire heap
module DPQ =
    type State<'t> when 't: comparison =
        { Heap: Heap<int * 't>
          Visited: Set<'t>
          Distances: Map<'t, int> }

    let private heapOf s = Heap.ofSeq false s

    let ofSeq s =
        { Heap = heapOf s
          Visited = Set.empty
          Distances = Map.empty }

    let rec tryUncons pq =
        pq.Heap
        |> Heap.tryUncons
        |> Option.bind (fun ((d, h), t) ->
            if pq.Visited |> Set.contains h then
                tryUncons { pq with Heap = t }
            else
                ((d, h),
                 { Visited = pq.Visited |> Set.add h
                   Distances = pq.Distances |> Map.add h d
                   Heap = t })
                |> Some)

    let visited x pq = pq.Visited |> Set.contains x

    let updateDistances updates pq =
        let unvisited =
            updates
            |> List.filter (fun n -> pq |> visited (snd n) |> not)

        { pq with Heap = pq.Heap |> Heap.merge (heapOf unvisited) }


///Immutable version of Dijkstra's shortest path algorithm
///https://en.wikipedia.org/wiki/Dijkstra%27s_algorithm#Pseudocode
let rec dijkstra successors cost (pq: DPQ.State<'a>) =
    match pq |> DPQ.tryUncons with
    | None -> pq
    | Some ((dist, coord), pqrest) ->
        let neighbours = successors coord
        let costed = neighbours |> List.map (fun n -> (cost dist n, n))
        let nextpq = pqrest |> DPQ.updateDistances costed

        dijkstra successors cost nextpq

type Grid = int [,]
module Grid = 
    let boundaries grid = ((0,Array2D.length1 grid - 1),(0, Array2D.length2 grid - 1))
    
    let at (x,y) (grid : Grid) = grid[x,y]
    
    let contains (grid : Grid) (x,y) = 
        let ((minx,maxx),(miny,maxy)) = boundaries grid
        minx <= x && x <= maxx
        && miny <= y && y <= maxy
    
type Direction =
    | U
    | D
    | L
    | R

type Node =
    { Location: int * int
      Direction: Direction
      StepsInDirection: int }

let parse input : Grid =
    input
    |> List.map (fun line -> line |> Seq.map (string >> int))
    |> array2D

let shift (dx, dy) (x,y) = x+dx,y+dy

let fwd (node : Node) =
    let delta =
        match node.Direction with
        | U -> (-1,0)
        | D -> (1,0)
        | L -> (0,-1)
        | R -> (0,1)
    { node with StepsInDirection = node.StepsInDirection + 1; Location = shift delta node.Location }

let left (node : Node) = 
    let rotate = function
    | U -> L
    | L -> D
    | D -> R
    | R -> U
    { node with Direction = rotate node.Direction; StepsInDirection = 0 }
    |> fwd
let right (node : Node) = 
    let rotate = function
    | U -> R
    | R -> D
    | D -> L
    | L -> U
    { node with Direction = rotate node.Direction; StepsInDirection = 0 }
    |> fwd

let successors grid (node : Node) = 
    //at most 3 blocks in a single direction
    // no reversing
    [
        if node.StepsInDirection < 3 then yield (fwd node)
        yield left node
        yield right node
    ]
    |> List.filter (_.Location >> (grid |> Grid.contains))

let cost (grid: Grid) cumulDist (node : Node) = 
    let c = grid |> Grid.at node.Location
    cumulDist + c

let grid = parse input
let start = (0, 0)

let queue: DPQ.State<Node> =
    [ (0,
       { Location = start
         Direction = R
         StepsInDirection = 0 })
      (0,
       { Location = start
         Direction = D
         StepsInDirection = 0 }) ]
    |> DPQ.ofSeq

let result = dijkstra (successors grid) (cost grid) queue
let ((_,xmax),(_,ymax)) = grid |> Grid.boundaries
//Smelly: distances has the nodes instead of the locations
//AKA multiple results per location (once per entered direction)
let minDist = 
    result.Distances 
    |> Map.filter (fun n _ -> n.Location = (xmax,ymax))
    |> Map.values
    |> Seq.min

let run () =
    printf "Testing.."
    test <@ 1 + 1 = 2 @>
    printfn "...done!"

run ()
