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

let rec bundle (line: (Location * Entry) list) : (Location list * Bundle) list =
    match line with
    | [] -> []
    | (_, Digit _) :: t ->
        let bundled =
            line
            |> List.takeWhile (fun (_, entry) ->
                match entry with
                | Digit _ -> true
                | _ -> false)

        let rest = line |> List.skip (bundled |> Seq.length)

        let digit =
            function
            | Digit d -> Some d
            | _ -> None

        let number =
            bundled
            |> List.choose (snd >> digit)
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

let gearLocations bundled =
    bundled
    |> List.choose (fun (loc, d) ->
        match d with
        | Symbol '*' -> Some loc
        | _ -> None)
    |> List.collect id

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

let overlaps locs (_, numberLocs) =
    (Set.intersect (Set.ofList locs) (Set.ofList numberLocs))
    |> Set.isEmpty
    |> not

let neighbouringPartNumbers partNumbers (x, y) =
    let neighbs =
        [ (x - 1, y - 1)
          (x, y - 1)
          (x + 1, y - 1)
          (x - 1, y)
          (x + 1, y)
          (x - 1, y + 1)
          (x, y + 1)
          (x + 1, y + 1) ]

    partNumbers
    |> List.filter (fun pn -> overlaps neighbs pn)

let solve input =
    let bundled = input |> parse |> List.collect bundle
    let symbolss = symbols bundled
    let symbolLocations = symbolss |> List.collect fst
    let numberss = numbers bundled

    let partNumbers =
        numberss
        |> List.filter (isPartNumber symbolLocations)

    let possibleGearLocations = gearLocations bundled

    let withNumbers =
        possibleGearLocations
        |> List.map (fun gl -> gl, neighbouringPartNumbers partNumbers gl)

    let actualGears =
        withNumbers
        |> List.filter (fun (gl, other) -> 2 = (other |> Seq.length))

    let ratios =
        actualGears
        |> List.map snd
        |> List.map (fun numbers -> numbers |> List.map fst |> List.reduce (*))

    let rationSum = ratios |> List.sum

    rationSum

#time
solve input

let run () =
    printf "Testing.."
    test <@ 1 + 1 = 2 @>
    printfn "...done!"

run ()
