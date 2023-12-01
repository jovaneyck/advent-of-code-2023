#r "nuget: Unquote"
open Swensen.Unquote

let input =
    System.IO.File.ReadAllLines $"""{__SOURCE_DIRECTORY__}\input"""
    |> List.ofSeq

let example =
    """two1nine
    eightwothree
    abcone2threexyz
    xtwone3four
    4nineeightseven2
    zoneight234
    7pqrstsixteen"""
        .Split("\n")
    |> Array.map (fun s -> s.Trim())
    |> List.ofSeq

let lookup =
    [ ("1", 1)
      ("2", 2)
      ("3", 3)
      ("4", 4)
      ("5", 5)
      ("6", 6)
      ("7", 7)
      ("8", 8)
      ("9", 9)
      ("one", 1)
      ("two", 2)
      ("three", 3)
      ("four", 4)
      ("five", 5)
      ("six", 6)
      ("seven", 7)
      ("eight", 8)
      ("nine", 9) ]
    |> Map.ofList

[<TailCall>]
let rec findDigits acc (line: string) =
    match line with
    | "" -> acc |> List.rev
    | l ->
        let digit = lookup |> Map.keys |> Seq.tryFind l.StartsWith

        match digit with
        | None -> findDigits acc (l.Substring(1))
        | Some (digit) ->
            let rest = line.Substring(1)
            let parsed = lookup |> Map.find digit
            findDigits (parsed :: acc) rest

let parseCalibrationValue (line: string) =
    let digits = line |> findDigits []
    let first = digits |> Seq.head
    let last = digits |> Seq.last
    let combined = sprintf "%d%d" first last
    let result = combined |> int
    result

let solve input =
    input |> List.map parseCalibrationValue |> Seq.sum

solve example
solve input

let run () =
    printf "Testing.."
    test <@ findDigits [] "1" = [ 1 ] @>
    test <@ findDigits [] "2" = [ 2 ] @>
    test <@ findDigits [] "3" = [ 3 ] @>
    test <@ findDigits [] "4" = [ 4 ] @>
    test <@ findDigits [] "5" = [ 5 ] @>
    test <@ findDigits [] "6" = [ 6 ] @>
    test <@ findDigits [] "7" = [ 7 ] @>
    test <@ findDigits [] "8" = [ 8 ] @>
    test <@ findDigits [] "9" = [ 9 ] @>
    test <@ findDigits [] "one" = [ 1 ] @>
    test <@ findDigits [] "two" = [ 2 ] @>
    test <@ findDigits [] "three" = [ 3 ] @>
    test <@ findDigits [] "four" = [ 4 ] @>
    test <@ findDigits [] "five" = [ 5 ] @>
    test <@ findDigits [] "six" = [ 6 ] @>
    test <@ findDigits [] "seven" = [ 7 ] @>
    test <@ findDigits [] "eight" = [ 8 ] @>
    test <@ findDigits [] "nine" = [ 9 ] @>
    test <@ findDigits [] "13" = [ 1; 3 ] @>
    test <@ findDigits [] "a1b3c" = [ 1; 3 ] @>
    test <@ findDigits [] "fivedsix" = [ 5; 6 ] @>
    test <@ findDigits [] "threeab2onezza6d" = [ 3; 2; 1; 6 ] @>
    test <@ solve [ "xlh1" ] = 11 @>

    test <@ solve [ "sevenine" ] = 79 @>
    printfn "...done!"

run ()
