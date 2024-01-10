#r "nuget: Unquote"
open Swensen.Unquote

let input =
    System.IO.File.ReadAllLines $"""{__SOURCE_DIRECTORY__}\input.txt"""
    |> List.ofSeq

type Pulse =
    | High
    | Low

type ModuleId = string
type Outputs = ModuleId list
type ConjunctionInputs = Map<ModuleId, Pulse>

type FlipFlopState =
    | On
    | Off

type Module =
    | Broadcaster of {| Outputs: Outputs |}
    | FlipFlop of
        {| Id: ModuleId
           State: FlipFlopState
           Outputs: Outputs |}
    | Conjunction of
        {| Id: ModuleId
           Inputs: ConjunctionInputs
           Outputs: Outputs |}

module Module =
    let id =
        function
        | Broadcaster _ -> "broadcaster"
        | FlipFlop f -> f.Id
        | Conjunction c -> c.Id

    let outputs =
        function
        | Broadcaster m -> m.Outputs
        | FlipFlop m -> m.Outputs
        | Conjunction m -> m.Outputs

let example =
    """ broadcaster -> a, b, c
        %a -> b
        %b -> c
        %c -> inv
        &inv -> a"""
        .Split("\n")
    |> Array.map (fun s -> s.Trim())
    |> List.ofSeq

let example2 =
    """ broadcaster -> a
        %a -> inv, con
        &inv -> b
        %b -> con
        &con -> output"""
        .Split("\n")
    |> Array.map (fun s -> s.Trim())
    |> List.ofSeq

let parseModule (line: string) =
    let [| left; right |] = line.Split(" -> ")
    let outputs = right.Split(", ") |> List.ofSeq

    let m =
        match left with
        | "broadcaster" -> Broadcaster {| Outputs = outputs |}
        | _ when left[0] = '%' ->
            FlipFlop
                {| Id = left[1..]
                   State = Off
                   Outputs = outputs |}
        | _ when left[0] = '&' ->
            Conjunction
                {| Id = left[1..]
                   Inputs = Map.empty
                   Outputs = outputs |}

    m

let connectModules modules =
    let findModulesThatOutputTo modules input =
        modules
        |> List.filter (fun m ->
            m
            |> Module.outputs
            |> List.contains (input |> Module.id))

    modules
    |> List.map (fun m ->
        match m with
        | Conjunction c ->
            let inputs = findModulesThatOutputTo modules m

            Conjunction
                {| c with
                    Inputs =
                        inputs
                        |> List.map (fun i -> (Module.id i, Low))
                        |> Map.ofSeq |}
        | _ -> m)

let parse input =
    input |> List.map parseModule |> connectModules

type PulsePropagation =
    { From: ModuleId
      To: ModuleId
      Pulse: Pulse }

type State =
    { Modules: Map<ModuleId, Module>
      Queue: PulsePropagation list
      Propagated: PulsePropagation list }

let rec send (state: State) : State =
    match state.Queue with
    | [] -> state
    | propagation :: rest ->
        let dest = propagation.To
        //{rv vp dc cq}
        if propagation.From = "cq"
           && propagation.Pulse = High then
            failwith "FOUND YOU!"

        let m = state.Modules |> Map.tryFind dest

        match m with
        | None ->
            send
                { state with
                    Queue = rest
                    Propagated = propagation :: state.Propagated }
        | Some (Broadcaster b) ->
            let tasks =
                b.Outputs
                |> List.map (fun output ->
                    { From = dest
                      To = output
                      Pulse = propagation.Pulse })

            send
                { state with
                    Queue = rest @ tasks
                    Propagated = propagation :: state.Propagated }
        | Some (FlipFlop f) ->
            match propagation.Pulse with
            | High ->
                send
                    { state with
                        Queue = rest
                        Propagated = propagation :: state.Propagated }
            | Low ->
                let (pulseToSend, newState) =
                    match f.State with
                    | Off -> High, On
                    | On -> Low, Off

                let flipped =
                    state.Modules
                    |> Map.add dest (FlipFlop {| f with State = newState |})

                let tasks =
                    f.Outputs
                    |> List.map (fun output ->
                        { From = dest
                          To = output
                          Pulse = pulseToSend })

                send
                    { Modules = flipped
                      Queue = rest @ tasks
                      Propagated = propagation :: state.Propagated }
        | Some (Conjunction c) ->
            let updatedMemory =
                {| c with
                    Inputs =
                        c.Inputs
                        |> Map.add propagation.From propagation.Pulse |}

            let remembered =
                state.Modules
                |> Map.add dest (Conjunction updatedMemory)

            let pulseToSend =
                if
                    updatedMemory.Inputs |> Map.values
                    |> Seq.forall ((=) High)
                then
                    Low
                else
                    High

            let tasks =
                c.Outputs
                |> List.map (fun output ->
                    { From = c.Id
                      To = output
                      Pulse = pulseToSend })

            send
                { Modules = remembered
                  Queue = rest @ tasks
                  Propagated = propagation :: state.Propagated }

let init mods =
    { Modules = mods
      Propagated = []
      Queue =
        [ { From = "button"
            To = "broadcaster"
            Pulse = Low } ] }

let rec sendMany n s =
    printfn "press # %d" n

    sendMany
        (n + 1L)
        (send
            { s with
                Queue =
                    [ { From = "button"
                        To = "broadcaster"
                        Pulse = Low } ] })

let solve input =
    let mods =
        input
        |> parse
        |> List.map (fun m -> (m |> Module.id, m))
        |> Map.ofSeq

    let s = mods |> init |> sendMany 1L

    let [ highs; lows ] =
        s.Propagated
        |> List.groupBy (fun prop -> prop.Pulse)

    let (nbhighs, nblows) = highs |> snd |> Seq.length, lows |> snd |> Seq.length
    let solution = nbhighs * nblows
    solution

let toDotGraph (m: Module) =
    let outs =
        (Module.outputs m)
        |> Seq.map string
        |> String.concat " "

    sprintf "%s -> {%s}" (Module.id m) outs

input
|> parse
|> List.map toDotGraph
|> String.concat "\n"
|> printfn "%s"

//rx <- ns <- {rv vp dc cq}
solve input
// rv: 4051
// vp: 3847
// dc: 3797
// cq: 3877
4051L * 3847L * 3797L * 3877L
