#r "nuget: Unquote"
open Swensen.Unquote

let input =
    System.IO.File.ReadAllLines $"""{__SOURCE_DIRECTORY__}\input.txt"""
    |> List.ofSeq

let example =
    """LLR
    
    AAA = (BBB, BBB)
    BBB = (AAA, ZZZ)
    ZZZ = (ZZZ, ZZZ)"""
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
    { Location: string
      CurrentInstruction: int }

let start = "AAA"
let finish = "ZZZ"

let itemAt index (list: 'a list) = list[index % list.Length]

let generator (nodes, instructions) state =
    let node = nodes |> Map.find state.Location

    let instruction = instructions |> itemAt state.CurrentInstruction

    let nextNode =
        match instruction with
        | L -> node.Left
        | R -> node.Right

    //printfn "We're at %A and turning %A" state.Location instruction

    let nextState =
        { Location = nextNode
          CurrentInstruction = state.CurrentInstruction + 1 }

    Some(state.Location, nextState)

let solve input =
    let (instructions, map) = parse input

    let initialState =
        { Location = start
          CurrentInstruction = 0 }

    let path = Seq.unfold (generator (map, instructions)) initialState

    let (length, _) =
        path
        |> Seq.indexed
        |> Seq.find (fun (_, node) -> node = "ZZZ")

    length

#time
solve input

let run () =
    printf "Testing.."
    test <@ 1 + 1 = 2 @>
    printfn "...done!"

run ()
