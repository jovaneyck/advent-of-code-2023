#r "nuget: Unquote"
open Swensen.Unquote

let input =
    System.IO.File.ReadAllLines $"""{__SOURCE_DIRECTORY__}\input.txt"""
    |> List.ofSeq

let example =
    """0 3 6 9 12 15
    1 3 6 10 15 21
    10 13 16 21 30 45"""
        .Split("\n")
    |> Array.map (fun s -> s.Trim())
    |> List.ofSeq

type History = int64 list //REVERSE ORDER

let parse (line: string) : History =
    line.Split(" ") |> Seq.map int64 |> Seq.toList

let diffs history =
    history
    |> List.pairwise
    |> List.map (fun (x, y) -> y - x)

let allDerivatives history =
    let rec allDerivatives acc history =
        let ds = diffs history

        if ds |> List.forall ((=) 0L) then
            ds :: acc
        else
            allDerivatives (ds :: acc) ds

    allDerivatives [ history ] history

let nextNumber history =
    let derivatives = history |> allDerivatives

    let next =
        derivatives
        |> List.map List.head
        |> List.reduce (fun a b -> b - a)

    next

let parsed = input |> List.map parse
parsed |> List.map nextNumber |> List.sum

let run () =
    printf "Testing.."
    test <@ 1 + 1 = 2 @>
    printfn "...done!"

run ()
