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

let rec generateCombinations chars =
    match chars with
    | [] -> [ [] ]
    | Empty :: cs ->
        (generateCombinations cs
         |> List.map (fun c -> Empty :: c))
    | Spring :: cs ->
        (generateCombinations cs
         |> List.map (fun c -> Spring :: c))
    | Unknown :: cs ->
        let replacedByEmpty = generateCombinations (Empty :: cs)
        let replacedBySpring = generateCombinations (Spring :: cs)
        replacedByEmpty @ replacedBySpring

//MUTABLE dictionary, immutable alternatives exist but are a hassle
//to thread through recursive functions
//TODO: investigate whether a state monad could help here?
type Memo = Dictionary<int list * Char list, bool>

let rec validCombination (memo: Memo) sizes chars =
    let key = (sizes, chars)

    match memo.TryGetValue(key) with
    | (true, result) -> result
    | (false, _) ->
        let result =
            if (sizes |> List.sum) > (chars |> Seq.length) then
                false
            else
                match sizes with
                | [] -> chars |> List.forall ((=) Empty)
                | s :: ss ->
                    match chars with
                    | [] -> false
                    | Unknown :: _ -> failwith "Did not expect an unkown character in here."
                    | Empty :: cs -> validCombination memo sizes cs
                    | Spring :: cs ->
                        if cs |> List.length < (s - 1) then
                            false
                        else
                            let otherSprings = cs |> List.take (s - 1)
                            let areAllSprings = (otherSprings |> List.forall ((=) Spring))

                            if areAllSprings |> not then
                                false
                            else
                                let remainder = cs |> List.skip (s - 1)

                                match remainder, ss with
                                | Unknown :: _, _ -> failwith "Did not expect an unkown character in here."
                                | Spring :: _, _ -> false
                                | Empty :: rs, ss -> validCombination memo ss rs
                                | [], [] -> true
                                | [], _ -> false

        memo.Add(key, result)
        result

let solve records =
    let allCombos =
        records
        |> List.map (fun (chars, sizes) -> (generateCombinations chars, sizes))

    let memo = new Memo(10_000_000)

    let validCombos =
        allCombos
        |> List.map (fun (combos, sizes) ->
            (combos
             |> List.filter (validCombination memo sizes)
             |> List.length))
        |> List.sum

    validCombos

let run () =
    printf "Testing.."
    test <@ 1 + 1 = 2 @>
    printfn "...done!"

run ()

let replicate (chars, sizes) =
    let replicatedChars =
        chars :: List.replicate 4 (Unknown :: chars)
        |> List.collect id

    let replicatedSizes = List.replicate 5 sizes |> List.collect id
    (replicatedChars, replicatedSizes)

let records = input |> List.map parse
//let replicated = records |> List.map replicate

#time
let result = records |> solve
