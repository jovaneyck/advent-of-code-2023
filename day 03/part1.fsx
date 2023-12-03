#r "nuget: Unquote"
open Swensen.Unquote

let input =
    System.IO.File.ReadAllLines $"""{__SOURCE_DIRECTORY__}\input.txt"""
    |> List.ofSeq

let example =
    """467..114..
    ...*......
    ..35..633.
    ......#...
    617*......
    .....+.58.
    ..592.....
    ......755.
    ...$.*....
    .664.598.."""
        .Split("\n")
    |> Array.map (fun s -> s.Trim())
    |> List.ofSeq

type Location = int * int

type Entry =
    | Digit of string
    | Symbol of char
    | Empty

let parse input =
    let parseCharacter =
        function
        | c when System.Char.IsDigit(c) -> Digit(c |> string)
        | '.' -> Empty
        | c -> Symbol c

    let parsed =
        [ for (rownum, line) in input |> Seq.indexed do
              [ for (colnum, character) in line |> Seq.indexed -> (colnum, rownum), parseCharacter character ] ]

    parsed

type Bundle =
    | Empty
    | Symbol of char
    | Number of int

let digit (Digit d) = d

let rec bundle (line: (Location * Entry) list) : (Location list * Bundle) list =
    match line with
    | [] -> []
    | (_, Digit _) :: t ->
        let bundled =
            line
            |> List.takeWhile (fun (loc, entry) ->
                match entry with
                | Digit _ -> true
                | _ -> false)

        let rest = line |> List.skip (bundled |> Seq.length)

        let number =
            bundled
            |> List.map (snd >> digit)
            |> String.concat ""
            |> int
            |> Number

        let locations = bundled |> List.map fst

        (locations, number) :: (bundle rest)
    | (pos, otherwise) :: t ->
        let entry =
            match otherwise with
            | Entry.Empty -> ([ pos ], Bundle.Empty)
            | Entry.Symbol s -> ([ pos ], Bundle.Symbol s)

        entry :: (bundle t)

let numbers bundled =
    bundled
    |> List.choose (fun (loc, d) ->
        match d with
        | Number n -> Some(n, loc)
        | _ -> None)

let symbols bundled =
    bundled
    |> List.filter (fun (loc, d) ->
        match d with
        | Symbol _ -> true
        | _ -> false)

let hasNeighbourIn locations (x, y) =
    let neighbs =
        [ (x - 1, y - 1)
          (x, y - 1)
          (x + 1, y - 1)
          (x - 1, y)
          (x + 1, y)
          (x - 1, y + 1)
          (x, y + 1)
          (x + 1, y + 1) ]

    (Set.intersect (Set.ofList neighbs) (Set.ofList locations))
    |> Set.isEmpty
    |> not

let isPartNumber symbolLocations (_, locations) =
    locations
    |> List.exists (hasNeighbourIn symbolLocations)

let solve input =
    let bundled = input |> parse |> List.collect bundle
    let symbolss = symbols bundled
    let symbolLocations = symbolss |> List.collect fst
    let numberss = numbers bundled

    let partNumbers =
        numberss
        |> List.filter (isPartNumber symbolLocations)

    let result = partNumbers |> List.sumBy fst
    result

solve input

let run () =
    printf "Testing.."
    test <@ 1 + 1 = 2 @>
    printfn "...done!"

run ()
