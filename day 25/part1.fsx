#r "nuget: Unquote"
open Swensen.Unquote

let input =
    System.IO.File.ReadAllLines $"""{__SOURCE_DIRECTORY__}\input.txt"""
    |> List.ofSeq

let example =
    """ jqt: rhn xhk nvd
        rsh: frs pzl lsr
        xhk: hfx
        cmg: qnr nvd lhk bvb
        rhn: xhk bvb hfx
        bvb: xhk hfx
        pzl: lsr hfx nvd
        qnr: nvd
        ntq: jqt hfx bvb xhk
        nvd: lhk
        lsr: lhk
        rzs: qnr cmg lsr rsh
        frs: qnr lhk lsr"""
        .Split("\n")
    |> Array.map (fun s -> s.Trim())
    |> List.ofSeq

input
|> List.collect (fun line ->
    let [| l; r |] = line.Split(": ")
    let rs = r.Split(" ") |> List.ofArray
    rs |> List.map (fun r -> sprintf "%s -> %s" l r))
|> Seq.iter (printfn "%s")
//https://www.devtoolsdaily.com/graphviz
//https://boxy-svg.com/
let totalNodes = 1423
let left = 717
let right = totalNodes - left
let result = left * right //506202

let run () =
    printf "Testing.."
    test <@ 1 + 1 = 2 @>
    printfn "...done!"

run ()
