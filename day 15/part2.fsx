#r "nuget: Unquote"
open Swensen.Unquote

let input = System.IO.File.ReadAllText $"""{__SOURCE_DIRECTORY__}\input.txt"""

let example = """rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7"""

let hashh (step: char seq) : int =
    let hasher currentValue character =
        let asciiValue = character |> int
        ((currentValue + asciiValue) * 17) % 256

    step |> Seq.fold hasher 0

type Hash = int
type Label = string
type FocalLength = int

type LabeledLens =
    { Label: Label
      FocalLength: FocalLength }

type HashMap = Map<Hash, LabeledLens list>

module HashMap =
    let addOrReplace (lens: LabeledLens) (map: HashMap) =
        let key = hashh lens.Label

        let rec addToList (lens: LabeledLens) (lenses: LabeledLens list) =
            match lenses with
            | [] -> [ lens ]
            | h :: t when h.Label = lens.Label -> lens :: t
            | h :: t -> h :: (addToList lens t)

        let add lenses =
            match lenses with
            | None -> Some [ lens ]
            | Some lenses -> Some(lenses |> addToList lens)

        map |> Map.change key add

    let remove (label: Label) (map: HashMap) =
        let key = hashh label

        let removeFromList label (lenses: LabeledLens list) =
            lenses
            |> List.filter (fun labeledLens -> labeledLens.Label <> label)

        map
        |> Map.change key (fun lenses ->
            lenses
            |> Option.map (fun lenses -> lenses |> removeFromList label))

type Step =
    | Remove of Label
    | Add of LabeledLens

let parseStep (step: string) =
    if step.Contains("=") then
        let [| label; rawFocal |] = step.Split("=")
        let focal = rawFocal |> int
        Add { Label = label; FocalLength = focal }
    else
        let [| label; _ |] = step.Split("-")
        Remove label

let applyStep (hashmap: HashMap) (step: Step) =
    match step with
    | Add lens -> hashmap |> HashMap.addOrReplace lens
    | Remove label -> hashmap |> HashMap.remove label

let focusingPower (box, slot, focalLength) = (1 + box) * slot * focalLength

let solve (input: string) =
    let steps = input.Split(",") |> List.ofSeq

    let hashmap =
        steps
        |> List.map parseStep
        |> List.fold applyStep Map.empty

    let lenses =
        hashmap
        |> Map.toList
        |> List.collect (fun (box, lenses) ->
            lenses
            |> List.mapi (fun idx lens -> (box, idx + 1, lens.FocalLength)))

    let focusingPowers = lenses |> List.map focusingPower
    let result = focusingPowers |> List.sum
    result

let run () =
    printf "Testing.."
    test <@ hashh "H" = 200 @>
    test <@ hashh "HASH" = 52 @>
    test <@ hashh "rn" = 0 @>
    test <@ parseStep "abc=123" = Add { Label = "abc"; FocalLength = 123 } @>
    test <@ parseStep "abc-" = Remove "abc" @>

    test
        <@ [ "rn=1" ]
           |> List.map parseStep
           |> List.fold applyStep Map.empty = ([ (0, [ { Label = "rn"; FocalLength = 1 } ]) ]
                                               |> Map.ofSeq) @>

    test
        <@ [ "qp=3"; "qp-" ]
           |> List.map parseStep
           |> List.fold applyStep Map.empty = ([ (1, []) ] |> Map.ofSeq) @>

    test <@ solve example = 145 @>

    printfn "...done!"

run ()

solve input
