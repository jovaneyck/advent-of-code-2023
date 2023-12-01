#r "nuget: Unquote"
open Swensen.Unquote

let input =
    System.IO.File.ReadAllLines $"""{__SOURCE_DIRECTORY__}\input"""
    |> List.ofSeq

let example =
    """1abc2
    pqr3stu8vwx
    a1b2c3d4e5f
    treb7uchet"""
        .Split("\n")
    |> Array.map (fun s -> s.Trim())
    |> List.ofSeq

let parseCalibrationValue (line: string) =
    let digits = line |> Seq.filter (System.Char.IsDigit)
    let first = digits |> Seq.head
    let last = digits |> Seq.last
    let combined = sprintf "%c%c" first last
    combined |> int

let solve input =
    input |> List.map parseCalibrationValue |> Seq.sum

solve example
solve input

let run () =
    printf "Testing.."
    test <@ 1 + 1 = 2 @>
    printfn "...done!"

run ()
