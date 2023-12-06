#r "nuget: Unquote"
open Swensen.Unquote

let input =
    System.IO.File.ReadAllLines $"""{__SOURCE_DIRECTORY__}\input.txt"""
    |> List.ofSeq

type Race = { Time: int; Record: int }

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
    printfn "%A" (race, pressDuration, result)

    result

let totalDistances (race: Race) =
    [ 0 .. race.Time ]
    |> List.map (totalDistance race)

let beatsRecord race distance = distance > race.Record

let parse (input: string list) =
    let lines =
        input
        |> List.map (fun line -> line.Split(" ", System.StringSplitOptions.RemoveEmptyEntries))

    let parsed =
        lines[0]
        |> Array.mapi (fun i first -> first, lines.[1].[i])
        |> Array.skip 1
        |> Array.map (fun (time, dist) -> { Time = int time; Record = int dist })

    parsed

let solve input =
    let winningScenarios =
        input
        |> parse
        |> Array.map (fun race -> (race, totalDistances race))
        |> Array.map (fun (race, distances) -> (race, distances |> List.filter (beatsRecord race)))

    winningScenarios
    |> Array.map (snd >> Seq.length)
    |> Seq.reduce (*)

solve input

let run () =
    printf "Testing.."
    test <@ solve example = 288 @>
    printfn "...done!"

run ()
