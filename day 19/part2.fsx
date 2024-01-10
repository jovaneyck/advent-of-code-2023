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

type InclusiveRange = { Start: int; End: int }

type Part =
    { x: InclusiveRange
      m: InclusiveRange
      a: InclusiveRange
      s: InclusiveRange }

type Outcome =
    | Accept
    | Reject

type Output =
    | Outcome of Outcome
    | NextWorkflow of string

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

type Workflow = { Name: string; Rules: Rule list }

let parseRule (rule: string) : Rule =
    let parseOutput =
        function
        | "A" -> Outcome Accept
        | "R" -> Outcome Reject
        | output -> NextWorkflow output

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

let parseWorkflow (workflow: string) : Workflow =
    let [| name; rem |] =
        workflow
            .Substring(0, workflow.Length - 1)
            .Split("{")

    let rawrules = rem.Split(",")
    let rules = rawrules |> Array.map parseRule |> Array.toList
    { Name = name; Rules = rules }

let parse (input: string) =
    let [| rawworkflows; _ |] = input.Replace("\r\n", "\n").Split("\n\n")

    let workflows =
        rawworkflows.Split("\n")
        |> List.ofSeq
        |> List.map parseWorkflow
        |> List.map (fun w -> w.Name, w)
        |> Map.ofList

    workflows

type Split = { True: Part; False: Part }

let apply workflows (spec: Part) =
    let split (spec: Part) (condition: Condition) : Split =
        match condition.Criterium, condition.Operator, condition.Operand with
        | X, LT, o ->
            { True = { spec with x.End = o - 1 }
              False = { spec with x.Start = o } }
        | M, LT, o ->
            { True = { spec with m.End = o - 1 }
              False = { spec with m.Start = o } }
        | A, LT, o ->
            { True = { spec with a.End = o - 1 }
              False = { spec with a.Start = o } }
        | S, LT, o ->
            { True = { spec with s.End = o - 1 }
              False = { spec with s.Start = o } }
        | X, GT, o ->
            { True = { spec with x.Start = o + 1 }
              False = { spec with x.End = o } }
        | M, GT, o ->
            { True = { spec with m.Start = o + 1 }
              False = { spec with m.End = o } }
        | A, GT, o ->
            { True = { spec with a.Start = o + 1 }
              False = { spec with a.End = o } }
        | S, GT, o ->
            { True = { spec with s.Start = o + 1 }
              False = { spec with s.End = o } }

    let rec applyRules workflows (spec: Part) (rules: Rule list) =
        let (r :: rs) = rules

        match r with
        | Output (Outcome (Reject)) -> [] //We don't care
        | Output (Outcome (Accept)) -> [ spec ] //We found a range that's accepted!
        | Output (NextWorkflow next) -> applyWorkflow workflows spec next
        | Condition (c, o) ->
            //We need to treat both evaluations of the predicate
            let splits = split spec c

            let trues =
                match o with
                | Outcome Accept -> [ splits.True ]
                | Outcome Reject -> []
                | NextWorkflow next -> applyWorkflow workflows splits.True next

            let falses = applyRules workflows splits.False rs
            List.append trues falses

    and applyWorkflow workflows (spec: Part) name : Part list =
        let wf: Workflow = workflows |> Map.find name
        applyRules workflows spec wf.Rules

    applyWorkflow workflows spec "in"

//We could have simply not generated invalid ranges, but this is simpler :)
let partSize (p: Part) : int64 =
    let size (r: InclusiveRange) =
        if r.Start <= r.End then
            1L + (int64 (r.End - r.Start))
        else
            0L //shortcut: if one of a part's ranges is 0 the entire part represents 0

    [ p.x; p.m; p.a; p.s ]
    |> List.map size
    |> List.reduce (*)

let solve input =

    let workflows = parse input

    let init =
        { x = { Start = 1; End = 4000 }
          m = { Start = 1; End = 4000 }
          a = { Start = 1; End = 4000 }
          s = { Start = 1; End = 4000 } }

    init
    |> apply workflows
    |> List.map partSize
    |> List.sum

solve example
solve input
