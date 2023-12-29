#r "nuget: Unquote"

open Swensen.Unquote

let input =
    System.IO.File.ReadAllLines $"""{__SOURCE_DIRECTORY__}\input.txt"""
    |> List.ofSeq

let example =
    """ ???.### 1,1,3
        .??..??...?##. 1,1,3
        ?#?#?#?#?#?#?#? 1,3,1,6
        ????.#...#... 4,1,1
        ????.######..#####. 1,6,5
        ?###???????? 3,2,1"""
        .Split("\n")
    |> Array.map (fun s -> s.Trim())
    |> List.ofSeq

type Char =
    | Unknown
    | Empty
    | Spring

let parse (line: string) =
    let parseChar =
        function
        | '.' -> Empty
        | '?' -> Unknown
        | '#' -> Spring
        | err -> failwithf "Unknown character: %c" err

    let [| chars; sizes |] = line.Split(" ")
    let parsedChars = chars |> Seq.map parseChar |> Seq.toList
    let parsedGroupSizes = sizes.Split(",") |> Seq.map int |> Seq.toList

    (parsedChars, parsedGroupSizes)

let replicate (chars, sizes) =
    let replicatedChars =
        chars :: List.replicate 4 (Unknown :: chars)
        |> List.collect id

    let replicatedSizes = List.replicate 5 sizes |> List.collect id
    (replicatedChars, replicatedSizes)

let rec countCombos (chars, sizes) = 
    match sizes with
    | [] -> 
        if chars |> List.forall (fun c -> c = Empty || c = Unknown) then 1 else 0
    | s :: ss ->
        match chars with
        | [] -> 0
        | Empty :: cs -> countCombos (cs, sizes)
        | Spring :: cs when cs |> List.length >= (s-1) ->
            let (sub, rest) = cs |> List.splitAt (s - 1)
            let allSprings = sub |> List.forall (fun c -> c = Spring || c = Unknown)
            if allSprings then 
                match rest with
                | [] -> if ss |> List.isEmpty then 1 else 0
                | Spring :: _ -> 0
                | Empty :: rs -> countCombos (rs, ss)
                | Unknown :: rs -> countCombos (rs, ss)
            else 0
        | Unknown :: cs ->
            let asSpring = countCombos ((Spring :: cs),sizes)
            let asEmpty = countCombos (cs,sizes)
            asSpring + asEmpty
        | _ -> 0

let solve records =
    records |> List.map countCombos |> List.sum

let run () =
    printf "Testing.."
    test <@ 1 + 1 = 2 @>
    printfn "...done!"

run ()

let records = input |> List.map parse
let replicated = records |> List.map replicate

#time
let result = replicated |> solve