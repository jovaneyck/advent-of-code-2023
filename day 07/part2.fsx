#r "nuget: Unquote"
open Swensen.Unquote

let input =
    System.IO.File.ReadAllLines $"""{__SOURCE_DIRECTORY__}\input.txt"""
    |> List.ofSeq

let example =
    """32T3K 765
    T55J5 684
    KK677 28
    KTJJT 220
    QQQJA 483"""
        .Split("\n")
    |> Array.map (fun s -> s.Trim())
    |> List.ofSeq

//Order of these cases is mucho importante for comparison!
type Card =
    | Joker
    | Two
    | Three
    | Four
    | Five
    | Six
    | Seven
    | Eight
    | Nine
    | Ten
    | Queen
    | King
    | Ace

type Hand = { Cards: Card list; Bid: int }

let parseCard =
    function
    | '2' -> Two
    | '3' -> Three
    | '4' -> Four
    | '5' -> Five
    | '6' -> Six
    | '7' -> Seven
    | '8' -> Eight
    | '9' -> Nine
    | 'T' -> Ten
    | 'J' -> Joker
    | 'Q' -> Queen
    | 'K' -> King
    | 'A' -> Ace
    | unknown -> failwithf "Unknown card rank: %c" unknown

let parseHand (line: string) =
    let [| cards; bid |] = line.Split(" ")
    let parsedCards = cards |> Seq.map parseCard |> Seq.toList
    { Cards = parsedCards; Bid = int bid }

type HandType =
    | FiveOfAKind
    | FourOfAKind
    | FullHouse
    | ThreeOfAKind
    | TwoPairs
    | OnePair
    | HighCard

let (|IsFiveOfAKind|_|) (cards: Card list) =
    if 1 = (cards |> Set.ofSeq |> Seq.length) then
        Some()
    else
        None

let (|IsFourOfAKind|_|) (cards: Card list) =
    let groups = cards |> Seq.groupBy id

    let hit =
        groups
        |> Seq.exists (fun (_, is) -> is |> Seq.length = 4)

    if hit then Some() else None

let (|IsFullHouse|_|) (cards: Card list) =
    let groups = cards |> Seq.groupBy id

    let threeOfAKind =
        groups
        |> Seq.exists (fun (_, is) -> is |> Seq.length = 3)

    if threeOfAKind && groups |> Seq.length = 2 then
        Some()
    else
        None

let (|IsThreeOfAKind|_|) (cards: Card list) =
    let groups = cards |> Seq.groupBy id

    let threeOfAKind =
        groups
        |> Seq.exists (fun (_, is) -> is |> Seq.length = 3)

    if threeOfAKind then Some() else None

let (|IsTwoPair|_|) (cards: Card list) =
    let groups = cards |> Seq.groupBy id

    let numberOfPairs =
        groups
        |> Seq.filter (fun (_, is) -> is |> Seq.length = 2)
        |> Seq.length

    if numberOfPairs = 2 then
        Some()
    else
        None

let (|IsOnePair|_|) (cards: Card list) =
    let groups = cards |> Seq.groupBy id

    let numberOfPairs =
        groups
        |> Seq.filter (fun (_, is) -> is |> Seq.length = 2)
        |> Seq.length

    if numberOfPairs = 1 then
        Some()
    else
        None

let potentialHands cards =
    let exceptJoker =
        [ Two
          Three
          Four
          Five
          Six
          Seven
          Eight
          Nine
          Ten
          Queen
          King
          Ace ]

    let explodeJoker cards card =
        match card with
        | Joker ->
            exceptJoker
            |> List.collect (fun card -> cards |> List.map (fun cs -> card :: cs))
        | _ -> cards |> List.map (fun cs -> card :: cs)

    cards
    |> List.fold (fun s c -> explodeJoker s c) [ [] ]
    |> List.map List.rev
    |> List.distinct

let handType =
    function
    | IsFiveOfAKind -> FiveOfAKind
    | IsFourOfAKind -> FourOfAKind
    | IsFullHouse -> FullHouse
    | IsThreeOfAKind -> ThreeOfAKind
    | IsTwoPair -> TwoPairs
    | IsOnePair -> OnePair
    | _ -> HighCard

let defineHandType cards =
    cards
    |> potentialHands
    |> List.map handType
    |> List.sort
    |> List.head

let tiebreaker (h1: Card list) (h2: Card list) = compare h1 h2

//Sorts in REVERSE ORDER. YOUR'RE WELCOME.
let compareHands (h1: Hand, t1: HandType) (h2, t2) =
    if t1 = t2 then
        tiebreaker h1.Cards h2.Cards
    else
        let bigger = 1
        let smaller = -1

        match t1, t2 with
        | FiveOfAKind, _ -> bigger
        | _, FiveOfAKind -> smaller
        | FourOfAKind, _ -> bigger
        | _, FourOfAKind -> smaller
        | FullHouse, _ -> bigger
        | _, FullHouse -> smaller
        | ThreeOfAKind, _ -> bigger
        | _, ThreeOfAKind -> smaller
        | TwoPairs, _ -> bigger
        | _, TwoPairs -> smaller
        | OnePair, _ -> bigger
        | _, OnePair -> smaller
        | HighCard, _ -> bigger

let solve input =
    input
    |> List.map parseHand
    |> List.map (fun hand -> (hand, defineHandType hand.Cards))
    |> List.sortWith compareHands
    |> List.mapi (fun i x -> i + 1, x)
    |> List.map (fun (rank, (hand, _)) -> rank * hand.Bid)
    |> List.sum

solve example

let run () =
    printf "Testing.."

    test
        <@ defineHandType [ Two
                            Two
                            Two
                            Two
                            Two ] = FiveOfAKind @>

    test
        <@ defineHandType [ Two
                            Two
                            Three
                            Two
                            Two ] = FourOfAKind @>

    test
        <@ defineHandType [ Two
                            Two
                            Three
                            Four
                            Four ] = TwoPairs @>

    test
        <@ defineHandType [ Three
                            Two
                            Ten
                            Three
                            King ] = OnePair @>

    test <@ handType [ King; Ten; Ten; Ten; Ten ] = FourOfAKind @>
    test <@ solve example = 5905 @>
    printfn "...done!"

run ()

#time
//Real: 00:00:07.286, CPU: 00:00:08.140, GC gen0: 5, gen1: 3, gen2: 1
//val it: int = 254837398

solve input
