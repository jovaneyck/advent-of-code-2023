#r "nuget: Unquote"
open Swensen.Unquote

let input =
    System.IO.File.ReadAllLines $"""{__SOURCE_DIRECTORY__}\input.txt"""
    |> List.ofSeq

let example =
    """LR
    
    11A = (11B, XXX)
    11B = (XXX, 11Z)
    11Z = (11B, XXX)
    22A = (22B, XXX)
    22B = (22C, 22C)
    22C = (22Z, 22Z)
    22Z = (22B, 22B)
    XXX = (XXX, XXX)"""
        .Split("\n")
    |> Array.map (fun s -> s.Trim())
    |> List.ofSeq

type Instruction =
    | L
    | R

type Node =
    { Name: string
      Left: string
      Right: string }

let parseInstruction =
    function
    | 'L' -> L
    | 'R' -> R
    | error -> failwithf "Unknown instruction %A" error

let parseNode (node: string) =
    let re = System.Text.RegularExpressions.Regex("(\w\w\w) = \((\w\w\w), (\w\w\w)\)")
    let m = re.Match(node)
    let name = m.Groups[1]
    let left = m.Groups[2]
    let right = m.Groups[3]

    { Name = name.Value
      Left = left.Value
      Right = right.Value }

let parse (input: string list) =
    let instructions = input[0] |> Seq.map parseInstruction |> Seq.toList

    let nodes =
        input[2..]
        |> List.map parseNode
        |> List.map (fun n -> n.Name, n)
        |> Map.ofSeq

    (instructions, nodes)

type State =
    { Locations: string list
      CurrentInstruction: int }

let itemAt index (list: 'a list) = list[index % list.Length]

let generator (nodes, instructions) state =
    let instruction = instructions |> itemAt state.CurrentInstruction

    let nextLocations =
        state.Locations
        |> List.map (fun location ->
            let node = nodes |> Map.find location

            let nextNode =
                match instruction with
                | L -> node.Left
                | R -> node.Right

            nextNode)

    //printfn "We're at %A and turning %A" state.Location instruction

    let nextState =
        { Locations = nextLocations
          CurrentInstruction = state.CurrentInstruction + 1 }

    Some(state.Locations, nextState)

let solve input =
    let (instructions, map) = parse input

    let starts =
        map
        |> Map.keys
        |> Seq.filter (fun id -> id.EndsWith('A'))
        |> Seq.toList

    let initialState =
        { Locations = starts
          CurrentInstruction = 0 }

    let path = Seq.unfold (generator (map, instructions)) initialState

    let (length, _) =
        path
        |> Seq.indexed
        |> Seq.map (fun (idx, nodes) ->
            (if nodes |> List.exists (fun n -> n.EndsWith('Z')) then
                 printfn "Step %d - %A" idx nodes
             else
                 ())

            (idx, nodes))
        |> Seq.find (fun (_, nodes) -> nodes |> List.forall (fun n -> n.EndsWith('Z')))

    length

#time
solve input

let run () =
    printf "Testing.."
    test <@ 1 + 1 = 2 @>
    printfn "...done!"

run ()
