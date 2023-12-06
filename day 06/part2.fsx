#r "nuget: Unquote"
open Swensen.Unquote

let input =
    System.IO.File.ReadAllLines $"""{__SOURCE_DIRECTORY__}\input.txt"""
    |> List.ofSeq

type Race = { Time: uint64; Record: uint64 }

let example =
    """Time:      7  15   30
    Distance:  9  40  200"""
        .Split("\n")
    |> Array.map (fun s -> s.Trim())
    |> List.ofSeq

let totalDistance (race: Race) pressDuration =
    let speed = pressDuration
    let time = race.Time - pressDuration
    let result = speed * time
    //printfn "%A" (race, pressDuration, result)

    result

let totalDistances (race: Race) =
    [ 0UL .. race.Time ]
    |> List.map (totalDistance race)

let beatsRecord race distance = distance > race.Record

let parse (input: string list) =
    let numbers =
        input
        |> List.map (fun line ->
            line.Split(" ", System.StringSplitOptions.RemoveEmptyEntries)
            |> Array.skip 1
            |> String.concat ""
            |> uint64)

    { Time = uint64 numbers[0]
      Record = uint64 numbers[1] }



let solve input =
    let race = input |> parse

    let winningScenarios =
        race
        |> (fun race -> totalDistances race)
        |> (fun distances -> distances |> List.filter (beatsRecord race))

    winningScenarios |> Seq.length

solve input

let run () =
    printf "Testing.."
    test <@ solve example = 71503 @>
    printfn "...done!"

run ()
