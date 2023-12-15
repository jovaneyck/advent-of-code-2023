#r "nuget: Unquote"
open Swensen.Unquote

let input =
    System.IO.File.ReadAllText $"""{__SOURCE_DIRECTORY__}\input.txt"""

let example =
    """rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7"""

let hashStep (step : char seq) : int = 
    let hasher currentValue character = 
        let asciiValue = character |> int
        ((currentValue + asciiValue) * 17) % 256

    step
    |> Seq.fold hasher 0

let solve (input : string) =
    let steps = input.Split(",") |> List.ofSeq
    let hashes = steps |> List.map hashStep
    let result = hashes |> List.sum
    result

let run () =
    printf "Testing.."
    test <@ hashStep "H" = 200 @>
    test <@ hashStep "HASH" = 52 @>
    test <@ solve example = 1320 @>
    printfn "...done!"

run ()

solve input
