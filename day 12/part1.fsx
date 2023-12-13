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

let rec validCombination sizes chars =
    match sizes with
    | [] -> chars |> List.forall ((=) Empty)
    | s :: ss ->
        match chars with
        | [] -> false
        | Unknown :: _ -> failwith "Did not expect an unkown character in here."
        | Empty :: cs -> validCombination sizes cs
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
                    | Empty :: rs, ss -> validCombination ss rs
                    | [], [] -> true
                    | [], _ -> false

let records = input |> List.map parse

let allCombos =
    records
    |> List.map (fun (chars, sizes) -> (generateCombinations chars, sizes))

let validCombos =
    allCombos
    |> List.collect (fun (combos, sizes) -> (combos |> List.filter (validCombination sizes)))
    |> List.length

let run () =
    printf "Testing.."
    test <@ 1 + 1 = 2 @>
    printfn "...done!"

run ()
