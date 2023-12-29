#r "nuget: Unquote"

open Swensen.Unquote
open System.Collections.Generic

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

type Memo = Dictionary<Char list * int list,int64>

let countCombos (chars, sizes) = 
    let rec calc (memo : Memo) (chars, sizes) =
        let key = (chars, sizes)

        match memo.TryGetValue(key) with
        | (true, result) -> result
        | (false, _) ->
            let result = 
                match sizes with
                | [] -> 
                    if chars |> List.forall (fun c -> c = Empty || c = Unknown) then 1L else 0L
                | s :: ss ->
                    match chars with
                    | [] -> 0L
                    | Empty :: cs -> calc memo (cs, sizes)
                    | Spring :: cs when cs |> List.length >= (s-1) ->
                        let (sub, rest) = cs |> List.splitAt (s - 1)
                        let allSprings = sub |> List.forall (fun c -> c = Spring || c = Unknown)
                        if allSprings then 
                            match rest with
                            | [] -> if ss |> List.isEmpty then 1L else 0L
                            | Spring :: _ -> 0L
                            | Empty :: rs -> calc memo (rs, ss)
                            | Unknown :: rs -> calc memo (rs, ss)
                        else 0L
                    | Unknown :: cs ->
                        let asSpring = calc memo ((Spring :: cs),sizes)
                        let asEmpty = calc memo (cs,sizes)
                        asSpring + asEmpty
                    | _ -> 0L
            memo.Add(key, result)
            result

    calc (new Memo()) (chars,sizes)

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
