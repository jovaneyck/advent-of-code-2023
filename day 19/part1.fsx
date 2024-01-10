#r "nuget: Unquote"
open Swensen.Unquote

let input = System.IO.File.ReadAllText $"""{__SOURCE_DIRECTORY__}\input.txt"""

let example =
    """px{a<2006:qkq,m>2090:A,rfg}
pv{a>1716:R,A}
lnx{m>1548:A,A}
rfg{s<537:gd,x>2440:R,A}
qs{s>3448:A,lnx}
qkq{x<1416:A,crn}
crn{x>2662:A,R}
in{s<1351:px,qqz}
qqz{s>2770:qs,m<1801:hdj,R}
gd{a>3333:R,R}
hdj{m>838:A,pv}

{x=787,m=2655,a=1222,s=2876}
{x=1679,m=44,a=2067,s=496}
{x=2036,m=264,a=79,s=2244}
{x=2461,m=1339,a=466,s=291}
{x=2127,m=1623,a=2188,s=1013}"""

type Part = { x: int; m: int; a: int; s: int }

let parsePart (part: string) : Part =
    let [| x; m; a; s |] = part.Substring(1, part.Length - 2).Split(",")
    let parseInt (t: string) = t.Substring(2, t.Length - 2) |> int

    { x = parseInt x
      m = parseInt m
      a = parseInt a
      s = parseInt s }

type Outcome =
    | Accept
    | Reject

type Output =
    | Outcome of Outcome
    | NextRule of string

type Criterium =
    | X
    | M
    | A
    | S

type Operator =
    | LT
    | GT

type Condition =
    { Criterium: Criterium
      Operator: Operator
      Operand: int }

type Rule =
    | Condition of Condition * Output
    | Output of Output

let parseRule (rule: string) : Rule =
    let parseOutput =
        function
        | "A" -> Outcome Accept
        | "R" -> Outcome Reject
        | output -> NextRule output

    let parseCriterium =
        function
        | "x" -> X
        | "m" -> M
        | "a" -> A
        | "s" -> S

    let parseCondition (c: string) =
        if c.Contains('<') then
            let [| crit; operand |] = c.Split('<')

            { Criterium = parseCriterium crit
              Operator = LT
              Operand = int operand }

        else if c.Contains('>') then
            let [| crit; operand |] = c.Split('>')

            { Criterium = parseCriterium crit
              Operator = GT
              Operand = int operand }

        else
            failwith "Unknown operand"

    let parseConditionRule (rule: string) : Rule =
        let [| cond; output |] = rule.Split(":")
        let o = parseOutput output
        let c = parseCondition cond
        Condition(c, o)

    if rule.Contains(":") then
        parseConditionRule rule
    else
        Rule.Output(parseOutput rule)

type Workflow = { Name: string; Rules: Rule list }

let parseWorkflow (workflow: string) : Workflow =
    let [| name; rem |] =
        workflow
            .Substring(0, workflow.Length - 1)
            .Split("{")

    let rawrules = rem.Split(",")
    let rules = rawrules |> Array.map parseRule |> Array.toList
    { Name = name; Rules = rules }

let applyWorkflow (wf: Workflow) (p: Part) : Output =
    let get (p: Part) =
        function
        | X -> p.x
        | M -> p.m
        | A -> p.a
        | S -> p.s

    let applies p (c: Condition) =
        //crit * operator * operand
        let crit = c.Criterium |> get p

        match c.Operator with
        | LT -> crit < c.Operand
        | GT -> c.Operand < crit

    let apply (r: Rule) p =
        match r with
        | Output o -> Some o
        | Condition (c, o) -> if c |> applies p then Some o else None

    let rec applyRules rules p =
        let nextRule = rules |> List.head
        let r = apply nextRule p

        match r with
        | Some output -> output
        | None -> applyRules (rules |> List.tail) p

    applyRules wf.Rules p

let rec runWorkflow workflows part start =
    let wf = workflows |> Map.find start
    let result = applyWorkflow wf part

    match result with
    | Outcome o -> o
    | NextRule nr -> runWorkflow workflows part nr

let apply workflows part = runWorkflow workflows part "in"
let rating (part: Part) = part.x + part.m + part.a + part.s

let parse (input: string) =
    let [| rawworkflows; rawparts |] = input.Replace("\r\n", "\n").Split("\n\n")

    let parts =
        rawparts.Split("\n")
        |> List.ofSeq
        |> List.map parsePart

    let workflows =
        rawworkflows.Split("\n")
        |> List.ofSeq
        |> List.map parseWorkflow
        |> List.map (fun w -> w.Name, w)
        |> Map.ofList

    parts, workflows

let parts, workflows = parse example

let ratings =
    parts
    |> List.map (fun p -> p, apply workflows p)
    |> List.filter (
        snd
        >> (function
        | Accept -> true
        | Reject -> false)
    )
    |> List.map fst
    |> List.map rating

let result = ratings |> List.sum

let run () =
    printf "Testing.."

    test
        <@ parsePart "{x=787,m=2655,a=1222,s=2876}" = { x = 787
                                                        m = 2655
                                                        a = 1222
                                                        s = 2876 } @>

    test
        <@ let wf = parseWorkflow "qqz{s>2770:qs,m<1801:hdj,R}"

           wf = { Name = "qqz"
                  Rules =
                    [ Condition(
                          { Criterium = S
                            Operator = GT
                            Operand = 2770 },
                          NextRule "qs"
                      )
                      Condition(
                          { Criterium = M
                            Operator = LT
                            Operand = 1801 },
                          NextRule "hdj"
                      )
                      Output(Outcome Reject) ] } @>

    test
        <@ rating
            { x = 787
              m = 2655
              a = 1222
              s = 2876 } = 7540 @>

    printfn "...done!"

run ()
