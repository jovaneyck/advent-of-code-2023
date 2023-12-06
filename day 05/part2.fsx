//Part 2 IDEA: instead of transforming seed ids
//Let's lift the problem and transform entire RANGES instead

#r "nuget: Unquote"
open Swensen.Unquote

//let input =
//    System.IO.File.ReadAllLines $"""{__SOURCE_DIRECTORY__}\input.txt"""
//    |> List.ofSeq

let example =
    """seeds: 79 14 55 13

    seed-to-soil map:
    50 98 2
    52 50 48

    soil-to-fertilizer map:
    0 15 37
    37 52 2
    39 0 15

    fertilizer-to-water map:
    49 53 8
    0 11 42
    42 0 7
    57 7 4

    water-to-light map:
    88 18 7
    18 25 70

    light-to-temperature map:
    45 77 23
    81 45 19
    68 64 13

    temperature-to-humidity map:
    0 69 1
    1 0 69

    humidity-to-location map:
    60 56 37
    56 93 4"""
        .Split("\n")
    |> Array.map (fun s -> s.Trim())
    |> List.ofSeq

/// <summary>
/// Represents a head-inclusive tail-exclusive range
/// </summary>
type Range =
    { Start: int64
      Length: int64 }

    member this.End = this.Start + this.Length

/// <summary>
/// Represents a head-inclusive tail-exclusive range
/// </summary>
module Range =
    let create start length = { Start = start; Length = length }

    let contains number range =
        range.Start <= number && number < range.End

    let isEmpty range = range.Length < 0

    let overlap (one: Range) (other: Range) =
        let start = max one.Start other.Start
        let ending = min one.End other.End
        create start (ending - start)

type Entry = { Range: Range; Offset: int64 }
type Map = Entry list

let parseEntry (entry: string) =
    let [| dest; src; length |] = entry.Split(" ") |> Array.map int64

    { Range = Range.create src length
      Offset = dest - src }

let rec chunkBy sep lines =
    if lines |> Seq.isEmpty then
        []
    else
        let nextChunk = lines |> List.skip 1 |> List.takeWhile ((<>) sep)
        let rest = lines |> List.skip (nextChunk.Length + 1)
        nextChunk :: (chunkBy sep rest)

let seeds =
    (example[0].Split("seeds: ")[1]).Split(" ")
    |> Array.chunkBySize 2
    |> Array.map (fun [| start; length |] ->
        { Start = int64 start
          Length = int64 length })
    |> Seq.toList

let chunks = chunkBy "" example

let seedToSoil: Map =
    chunks[1]
    |> List.skip 1
    |> List.map parseEntry
    |> List.sortBy (fun e -> e.Range.Start)

let soilToFertilizer: Map =
    chunks[2]
    |> List.skip 1
    |> List.map parseEntry
    |> List.sortBy (fun e -> e.Range.Start)

let fertilizerToWater: Map =
    chunks[3]
    |> List.skip 1
    |> List.map parseEntry
    |> List.sortBy (fun e -> e.Range.Start)

let waterToLight: Map =
    chunks[4]
    |> List.skip 1
    |> List.map parseEntry
    |> List.sortBy (fun e -> e.Range.Start)

let lightToTemperature: Map =
    chunks[5]
    |> List.skip 1
    |> List.map parseEntry
    |> List.sortBy (fun e -> e.Range.Start)

let temperatureToHumidity: Map =
    chunks[6]
    |> List.skip 1
    |> List.map parseEntry
    |> List.sortBy (fun e -> e.Range.Start)

let humidityToLocation: Map =
    chunks[7]
    |> List.skip 1
    |> List.map parseEntry
    |> List.sortBy (fun e -> e.Range.Start)

let rec transform (map: Map) (entry: Entry) : Map =
    if entry.Range |> Range.isEmpty then
        [] //Fully transformed the range
    else
        match map with
        | [] -> [ entry ] //no more entries, identity
        | h :: t ->
            let overlap = Range.overlap entry.Range h.Range

            if overlap |> Range.isEmpty then
                transform t entry //no overlap, proceed to next entry
            else if entry.Range = overlap then //exact match, apply offset
                [ { Range = overlap
                    Offset = entry.Offset + h.Offset } ]
            else
                []

//let transformed = seeds |> List.collect (transform seedToSoil)
//let seedToLocation seeds =
//    [ seedToSoil
//      soilToFertilizer
//      fertilizerToWater
//      waterToLight
//      lightToTemperature
//      temperatureToHumidity
//      humidityToLocation ]
//    |> List.fold (fun x map -> transform map x) seeds

//let transformed = [ seeds ] |> List.collect seedToLocation

#time

let run () =
    printf "Testing.."
    test <@ chunkBy "" [ ""; "a"; "b"; ""; "c"; "d" ] = [ [ "a"; "b" ]; [ "c"; "d" ] ] @>

    test <@ { Start = 8L; Length = 3L } |> Range.contains 10L @>
    test <@ { Start = 8L; Length = 2L } |> Range.contains 10L |> not @>
    test <@ { Start = 10L; Length = 2L } |> Range.contains 10L @>
    test <@ { Start = 11L; Length = 2L } |> Range.contains 10L |> not @>

    test <@ Range.create 2 2 = Range.overlap (Range.create 1 3) (Range.create 2 3) @>

    printfn "...done!"

run ()
