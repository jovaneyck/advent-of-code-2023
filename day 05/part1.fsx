#r "nuget: Unquote"
open Swensen.Unquote

let input =
    System.IO.File.ReadAllLines $"""{__SOURCE_DIRECTORY__}\input.txt"""
    |> List.ofSeq

//let example =
//    """seeds: 79 14 55 13

//    seed-to-soil map:
//    50 98 2
//    52 50 48

//    soil-to-fertilizer map:
//    0 15 37
//    37 52 2
//    39 0 15

//    fertilizer-to-water map:
//    49 53 8
//    0 11 42
//    42 0 7
//    57 7 4

//    water-to-light map:
//    88 18 7
//    18 25 70

//    light-to-temperature map:
//    45 77 23
//    81 45 19
//    68 64 13

//    temperature-to-humidity map:
//    0 69 1
//    1 0 69

//    humidity-to-location map:
//    60 56 37
//    56 93 4"""
//        .Split("\n")
//    |> Array.map (fun s -> s.Trim())
//    |> List.ofSeq

type Entry =
    { SourceStart: uint64
      DestinationStart: uint64
      RangeLength: uint64 }

type Map = Entry list

let parseEntry (entry: string) =
    let [| dest; src; length |] = entry.Split(" ") |> Array.map uint64

    { SourceStart = src
      DestinationStart = dest
      RangeLength = length }

let rec chunkBy sep lines =
    if lines |> Seq.isEmpty then
        []
    else
        let nextChunk = lines |> List.skip 1 |> List.takeWhile ((<>) sep)
        let rest = lines |> List.skip (nextChunk.Length + 1)
        nextChunk :: (chunkBy sep rest)

let seeds =
    (input[ 0 ].Split("seeds: ")[1]).Split(" ")
    |> Seq.map uint64
    |> Seq.toList

let chunks = chunkBy "" input

let seedToSoil: Map = chunks[1] |> List.skip 1 |> List.map parseEntry
let soilToFertilizer: Map = chunks[2] |> List.skip 1 |> List.map parseEntry
let fertilizerToWater: Map = chunks[3] |> List.skip 1 |> List.map parseEntry
let waterToLight: Map = chunks[4] |> List.skip 1 |> List.map parseEntry
let lightToTemperature: Map = chunks[5] |> List.skip 1 |> List.map parseEntry
let temperatureToHumidity: Map = chunks[6] |> List.skip 1 |> List.map parseEntry
let humidityToLocation: Map = chunks[7] |> List.skip 1 |> List.map parseEntry

let inSourceRange number (entry: Entry) =
    entry.SourceStart <= number
    && number <= entry.SourceStart + entry.RangeLength

let corresponding number (entry: Entry) =
    let offset = number - entry.SourceStart
    entry.DestinationStart + offset

let lookup (map: Map) number =
    let entry = map |> List.tryFind (inSourceRange number)

    match entry with
    | None -> number
    | Some entry -> (corresponding number entry)

let debug msg x =
    //printfn msg
    //printfn "%A" x
    x

let lookupchain =
    debug "\n let's go"
    >> (lookup seedToSoil)
    >> debug "seed to soil"
    >> (lookup soilToFertilizer)
    >> debug "soil to fert"
    >> (lookup fertilizerToWater)
    >> debug "fert to water"
    >> (lookup waterToLight)
    >> debug "water to light"
    >> (lookup lightToTemperature)
    >> debug "light to temp"
    >> (lookup temperatureToHumidity)
    >> debug "temp to hum"
    >> (lookup humidityToLocation)
    >> debug "humidity to loc"

let soils = seeds |> List.map lookupchain
soils |> List.min

let run () =
    printf "Testing.."
    test <@ chunkBy "" [ ""; "a"; "b"; ""; "c"; "d" ] = [ [ "a"; "b" ]; [ "c"; "d" ] ] @>

    test
        <@ inSourceRange
            10UL
            { SourceStart = 8UL
              RangeLength = 2UL
              DestinationStart = 1UL } @>

    test
        <@ inSourceRange
            10UL

            { SourceStart = 10UL
              RangeLength = 2UL
              DestinationStart = 1UL } @>

    test
        <@ inSourceRange
            10UL

            { SourceStart = 7UL
              RangeLength = 2UL
              DestinationStart = 1UL }
           |> not @>

    test
        <@ inSourceRange
            10UL

            { SourceStart = 11UL
              RangeLength = 2UL
              DestinationStart = 1UL }
           |> not @>

    test <@ lookup seedToSoil 53UL = 55UL @>
    test <@ lookup seedToSoil 1337UL = 1337UL @>
    printfn "...done!"

run ()
