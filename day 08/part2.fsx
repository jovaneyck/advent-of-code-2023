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

let solvePath map instructions start =
    let initialState =
        { Location = start
          CurrentInstruction = 0 }

    let path = Seq.unfold (generator (map, instructions)) initialState

    let (length, _) =
        path
        |> Seq.indexed
        |> Seq.find (fun (_, node) -> node.EndsWith('Z'))

    length

let solve input =
    let (instructions, map) = parse input

    let starts =
        map
        |> Map.keys
        |> Seq.filter (fun node -> node.EndsWith('A'))
        |> List.ofSeq

    let cycleLengths =
        starts
        |> List.map (fun start -> solvePath map instructions start)

    cycleLengths

let rec gcd (x: int64) (y: int64) = if y = 0 then abs x else gcd y (x % y)
let lcm x y = x * y / (gcd x y)

let cycleLengths = solve input

cycleLengths
|> List.map int64
|> List.reduce (fun x y -> lcm x y)

#time

let run () =
    printf "Testing.."
    test <@ 1 + 1 = 2 @>
    printfn "...done!"

run ()
