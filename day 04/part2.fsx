#r "nuget: Unquote"
open Swensen.Unquote

let input =
    System.IO.File.ReadAllLines $"""{__SOURCE_DIRECTORY__}\input.txt"""
    |> List.ofSeq

let example =
    """Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
    Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
    Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
    Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
    Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
    Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"""
        .Split("\n")
    |> Array.map (fun s -> s.Trim())
    |> List.ofSeq

type Card =
    { Id: int
      WinningNumbers: Set<int>
      Numbers: Set<int> }

let parseCard (line: string) =
    let [| cardNo; numberPart |] = line.Split(": ")

    let cardId =
        cardNo.Split(" ", System.StringSplitOptions.RemoveEmptyEntries)[1]
        |> int

    let [| winning; numbers |] = numberPart.Split(" | ")

    let winningNumbers =
        winning.Split(" ", System.StringSplitOptions.RemoveEmptyEntries)
        |> Array.map int

    let cardNumbers =
        numbers.Split(" ", System.StringSplitOptions.RemoveEmptyEntries)
        |> Array.map int

    { Id = cardId
      WinningNumbers = Set.ofSeq winningNumbers
      Numbers = Set.ofSeq cardNumbers }

let points c =
    let hits =
        Set.intersect c.Numbers c.WinningNumbers
        |> Seq.length

    hits

type CardId = int

type State =
    { Cards: Map<CardId, Card>
      NumberOfCopies: Map<CardId, int> }

let solve input =
    let parsed = input |> List.map parseCard

    let initial =
        { Cards =
            parsed
            |> List.map (fun c -> (c.Id, c))
            |> Map.ofList
          NumberOfCopies =
            parsed
            |> List.map (fun c -> (c.Id, 1))
            |> Map.ofSeq }

    let scoreAndCopy (state: State) card =
        let p = points card
        let nbCopies = state.NumberOfCopies |> Map.find card.Id
        //printfn "Points for card %A: %A" card p
        let idsToCopy = [ (card.Id + 1) .. (card.Id + p) ]

        let bumpCount (state: State) id =
            //printfn "Bumping count for card %A in state %A" id state

            { state with
                NumberOfCopies =
                    state.NumberOfCopies
                    |> Map.change id (fun (Some count) -> Some(count + nbCopies)) }

        let newCounts = idsToCopy |> List.fold bumpCount state

        newCounts

    let folded = parsed |> List.fold scoreAndCopy initial

    folded.NumberOfCopies
    |> Map.toList
    |> List.sumBy snd

let run () =
    printf "Testing.."
    test <@ solve input = 11827296 @>
    printfn "...done!"

run ()
