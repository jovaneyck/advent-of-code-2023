#r "nuget: Unquote"
open Swensen.Unquote

let input =
    System.IO.File.ReadAllLines $"""{__SOURCE_DIRECTORY__}\input.txt"""
    |> List.ofSeq

let example =
    """Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
    Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
    Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
    Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
    Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"""
        .Split("\n")
    |> Array.map (fun s -> s.Trim())
    |> List.ofSeq

type Colour =
    | Blue
    | Green
    | Red

type Hand = (Colour * int) list
type Game = { Id: int; Hands: Hand list }

let parse (line: string) =
    let parsecolour (colourCount: string) : (Colour * int) =
        let [| rawCount; rawColour |] = colourCount.Split(" ")
        let count = rawCount |> int

        let colour =
            match rawColour with
            | "blue" -> Blue
            | "red" -> Red
            | "green" -> Green

        (colour, count)

    let parseHand (hand: string) =
        let colourCounts = hand.Split(", ")
        colourCounts |> Seq.map parsecolour |> Seq.toList

    let [| game; handfuls |] = line.Split(": ")
    let gameID = game.Split(" ")[1] |> int

    let hands =
        handfuls.Split("; ")
        |> Array.map parseHand
        |> List.ofSeq

    { Id = gameID; Hands = hands }

let isPossible (game: Game) =
    //only 12 red cubes, 13 green cubes, and 14 blue cubes?
    let isPossibleHand (hand: Hand) =
        let (_, rc) =
            hand
            |> List.tryFind (fun (c, _) -> c = Red)
            |> Option.defaultValue (Red, 0)

        let (_, gc) =
            hand
            |> List.tryFind (fun (c, _) -> c = Green)
            |> Option.defaultValue (Green, 0)

        let (_, bc) =
            hand
            |> List.tryFind (fun (c, _) -> c = Blue)
            |> Option.defaultValue (Blue, 0)

        rc <= 12 && gc <= 13 && bc <= 14

    game.Hands |> List.forall isPossibleHand

let solve input = 
    let games = input |> List.map parse
    let possibles = games |> List.filter isPossible
    let solution = possibles |> Seq.sumBy _.Id
    solution

solve input

let run () =
    printf "Testing.."
    test <@ solve example = 8 @>
    printfn "...done!"

run ()
