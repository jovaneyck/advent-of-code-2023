#r "nuget: Unquote"

open Swensen.Unquote

//https://dev.to/shimmer/the-state-monad-in-f-3ik0
//S: state (our memo)
//a: result (nbCombinations)
//Stateful computation: S -> (a,S)
type Stateful<'state, 'result> =
    Stateful of ('state -> 'result * 'state)

module Stateful =
    let run state (Stateful f) =
        f state

    let execute state sf =
        let result, _ = run state sf
        result

    let ret result =
        Stateful (fun state -> (result, state))

    let bind binder stateful =
        Stateful (fun state ->
            let result, state' = stateful |> run state
            binder result |> run state')

type StatefulBuilder() =
    let (>>=) stateful binder = Stateful.bind binder stateful
    member __.Return(result) = Stateful.ret result
    member __.ReturnFrom(stateful) = stateful
    member __.Bind(stateful, binder) = stateful >>= binder
    member __.Zero() = Stateful.ret ()
    member __.Combine(statefulA, statefulB) =
        statefulA >>= (fun _ -> statefulB)
    member __.Delay(f) = f ()

let state = StatefulBuilder()

type Stack = int list

module Stack = 
    let empty : Stack = []
    let pop stack : int * Stack = 
        let h::t = stack
        h,t
    let push x stack : Stack = 
        x :: stack

module SStack =
    let pop = Stateful Stack.pop
    let push x = Stateful (fun s -> (), Stack.push x s)

type Memo<'tk, 'tv> = System.Collections.Generic.Dictionary<'tk,'tv>
module Memo =
    let empty<'k when 'k : equality> = new Memo<'k,'v>()
    let add<'k,'v> key v (memo : Memo<'k,'v>) = memo.Add(key, v)
    let tryFind key (memo : Memo<'k,'v>) = 
        match memo.TryGetValue(key) with
        | true, v -> Some v
        | _ -> None

module SMemo =
    let add k v = Stateful (fun memo -> Memo.add k v memo; (),memo) //hidden side effect here-o
    let tryFind k = Stateful (fun memo -> Memo.tryFind k memo, memo)

let runTests () =
    printf "Testing.."
    test <@ state {
                do! SStack.push 1337
                let! x = SStack.pop
                return x
            } |> Stateful.execute Stack.empty = 1337 @>

    test <@ state {
                do! SMemo.add "key" "value"
                let! v = SMemo.tryFind "key"

                let res = 
                    match v with
                    | None -> "nope we did not find the key in our memo state"
                    | Some v -> v

                return res
            } |> Stateful.execute Memo.empty = "value" @>


    printfn "...done!"

runTests ()
