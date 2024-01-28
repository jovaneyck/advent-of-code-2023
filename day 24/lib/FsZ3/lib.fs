//TODO: revive fsz3 github, the nuget was painfully out of date, ended up monkeypatching:
module public Microsoft

module Z3 =
    open System
    open System.Numerics
    open Microsoft.Z3

    [<AutoOpen>]
    module public Api =
        module Context =
            open System.Collections.Generic

            /// Make a new context
            let create () = new Context(Dictionary())

            /// Get a list of tactics for the context
            let tactics (ctx: Context) = ctx.TacticNames

        /// Globals / global state
        module Gs =
            let mutable private globalCtx = Context.create ()
            let context () = globalCtx

        let inline (++) xs ys = Array.append xs ys

        module BoolSort =
            let create (ctx: Context) = ctx.MkBoolSort()

        module IntSort =
            let create (ctx: Context) = ctx.MkIntSort()

        module RealSort =
            let create (ctx: Context) = ctx.MkRealSort()

        module BitVecSort =
            let create (ctx: Context) i = ctx.MkBitVecSort i

        module ArraySort =
            let create (ctx: Context) (domain: Sort) (range: Sort) = ctx.MkArraySort(domain, range)

        [<AbstractClass>]
        type Theory() =
            abstract member Expr: Expr

        /// Implement IEnumerable interface to support for..in..do construct
        type Microsoft.Z3.Statistics with
            member x.GetEnumerator() =
                (Seq.map (fun k -> k, x.[k]) x.Keys)
                    .GetEnumerator()

        type Result =
            | Const of Expr
            | Func of FuncInterp
            override x.ToString() =
                match x with
                | Const expr -> sprintf "%O" expr
                | Func f -> sprintf "%O" f

        /// Multiple indexers for evaluating formulas
        type Microsoft.Z3.Model with
            member x.Item(index: Expr) = x.Eval(index, true)

            member x.Item(index: FuncDecl) =
                // Taking care of array declaration
                if index.DomainSize = 0u
                   && index.Range.SortKind <> Z3_sort_kind.Z3_ARRAY_SORT then
                    x.ConstInterp(index) |> Const
                else
                    x.FuncInterp(index) |> Func

            member x.Evaluate(v: Theory, ?modelCompletion) =
                x.Evaluate(v.Expr, defaultArg modelCompletion false)

        type SolveResult =
            | NoSolution
            | Unknown
            | Solution of (Symbol * FuncDecl * Result) list

        module Solver =

            /// Make a new solver with an optional tactic
            let create (context: Context) = context.MkSolver()

            let create_tactic (context: Context) (tactic: string) = context.MkSolver tactic

            let private wrap_status (solver: Solver) =
                function
                | Status.SATISFIABLE ->
                    let m = solver.Model
                    let results = m.Decls |> Array.map (fun d -> d.Name, d, m.[d])
                    Solution(results |> List.ofArray)
                | Status.UNKNOWN -> Unknown
                | Status.UNSATISFIABLE -> NoSolution
                | x -> failwithf "unknown enum value %O" x

            let check (solver: Solver) = solver.Check() |> wrap_status solver

            let checkAssumptions (solver: Solver) (assumptions: Expr list) =
                solver.Check(Array.ofList assumptions)
                |> wrap_status solver

    module public Bool =
        type Bool(e: BoolExpr) =
            inherit Theory()
            override x.Expr = e :> Expr
            override x.ToString() = sprintf "%O" e
            static member FromExpr(e: Expr) = Bool(e :?> BoolExpr)

        let BoolExpr expr = Bool(expr)
        let (|BoolExpr|) (b: Bool) = b.Expr :?> BoolExpr

        module Solver =

            let ``assert`` (x: Solver) (BoolExpr expr) = x.Assert expr

            let assert_range (x: Solver) (xs: _ []) =
                for BoolExpr e in xs do
                    x.Assert e

        [<AutoOpen>]
        module internal BoolUtils =
            let inline createBool b = Gs.context().MkBool(b)
            let inline createAnd x y = Gs.context().MkAnd(x, y) |> BoolExpr
            let inline createOr x y = Gs.context().MkOr(x, y) |> BoolExpr
            let inline createNot x = Gs.context().MkNot(x) |> BoolExpr

            let inline createImplies x y =
                Gs.context().MkImplies(x, y) |> BoolExpr

            let inline createEquiv x y = Gs.context().MkEq(x, y) |> BoolExpr
            let inline createTrue () = Gs.context().MkTrue() |> BoolExpr
            let inline createFalse () = Gs.context().MkFalse() |> BoolExpr
            let inline createDistinct (xs: Expr []) = Gs.context().MkDistinct xs |> BoolExpr

            let inline createITE b expr1 expr2 =
                Gs.context().MkITE(b, expr1, expr2) :?> BoolExpr
                |> BoolExpr

        type Bool with
            static member (&&.)(BoolExpr p, BoolExpr q) = createAnd p q
            static member (&&.)(BoolExpr p, q) = createAnd p (createBool q)
            static member (&&.)(p, BoolExpr q) = createAnd (createBool p) q
            static member (||.)(BoolExpr p, BoolExpr q) = createOr p q
            static member (||.)(BoolExpr p, q) = createOr p (createBool q)
            static member (||.)(p, BoolExpr q) = createOr (createBool p) q
            static member (!.)(BoolExpr p) = createNot p
            static member (=>.)(BoolExpr p, BoolExpr q) = createImplies p q
            static member (=>.)(BoolExpr p, q) = createImplies p (createBool q)
            static member (=>.)(p, BoolExpr q) = createImplies (createBool p) q
            static member (=.)(BoolExpr p, BoolExpr q) = createEquiv p q
            static member (=.)(BoolExpr p, q) = createEquiv p (createBool q)
            static member (=.)(p, BoolExpr q) = createEquiv (createBool p) q

            static member Distinct xs =
                Array.map (fun (BoolExpr expr) -> expr :> Expr) xs
                |> createDistinct

            static member If(BoolExpr b, BoolExpr expr1, BoolExpr expr2) = createITE b expr1 expr2

        /// Return a bool const with supplied name
        let Bool (s: string) =
            let context = Gs.context ()
            context.MkBoolConst s |> BoolExpr

        let True = createTrue ()
        let False = createFalse ()

        let And (args: Bool []) = Array.reduce (&&.) args
        let Or (args: Bool []) = Array.reduce (||.) args
        let Implies (arg1: Bool, arg2: Bool) = arg1 =>. arg2
        let Not (arg: Bool) = !.arg

        let inline Distinct (xs: ^T []) =
            (^T: (static member Distinct: ^T [] -> Bool) (xs))

        let inline If (b: Bool, expr1: ^T, expr2: ^T) =
            (^T: (static member If: Bool * ^T * ^T -> Bool) (b, expr1, expr2))

        type Val =
            | Bool of bool
            | UInt of uint32
            | Double of float

        type Overloads = Overloads
            with
                static member ($)(Overloads, b) = fun (s: string) -> s, Bool b
                static member ($)(Overloads, i) = fun (s: string) -> s, UInt i
                static member ($)(Overloads, f) = fun (s: string) -> s, Double f

        let inline (=>) k v = (Overloads $ v) k

        let simplify (f: Expr) (options: (string * _) []) =
            let p = Gs.context().MkParams()

            for (k, v) in options do
                match v with
                | Bool b -> p.Add(k, b)
                | UInt i -> p.Add(k, i)
                | Double f -> p.Add(k, f)
                |> ignore

            f.Simplify(p)

        let internal set_option (options: (string * _) []) =
            let c = Gs.context ()
            let p = c.MkParams()

            for (k, v) in options do
                match v with
                | Bool b -> c.UpdateParamValue(k, sprintf "%O" b)
                | UInt i -> c.UpdateParamValue(k, sprintf "%O" i)
                | Double f -> c.UpdateParamValue(k, sprintf "%O" f)

        type Z3 =
            static member Solve([<ParamArray>] xs: _ []) =
                let solver = Gs.context () |> Solver.create
                xs |> Solver.assert_range solver
                let sol = Solver.check solver

                match sol with
                | NoSolution
                | Unknown -> printfn "no solution"
                | Solution solutions ->
                    let m = solver.Model
                    printfn "["

                    solutions
                    |> Seq.map (fun (name, decl, value) -> sprintf " %O = %O" name value)
                    |> fun s -> String.Join(",\n", s)
                    |> printfn "%s"

                    printfn "]"

                sol

            static member SolveResults([<ParamArray>] xs: _ []) =
                let solver = Gs.context () |> Solver.create
                xs |> Solver.assert_range solver
                Solver.check solver

            static member inline Simplify
                (
                    f: ^T when ^T :> Theory and ^T: (static member FromExpr: Expr -> ^T),
                    [<ParamArray>] options: (string * _) []
                ) =
                (^T: (static member FromExpr: Expr -> ^T) (simplify f.Expr options))

            static member SetOption([<ParamArray>] options: (string * _) []) = set_option options


    module public Int =
        open Bool

        type Int(expr: IntExpr) =
            inherit Theory()
            override x.Expr = expr :> Expr
            override x.ToString() = sprintf "%O" expr
            static member FromExpr(e: Expr) = Int(e :?> IntExpr)

        let IntExpr expr = Int(expr)
        let (|IntExpr|) (i: Int) = i.Expr :?> IntExpr

        [<AutoOpen>]
        module internal IntUtils =
            let inline createInt (x: bigint) = Gs.context().MkInt(string x)

            let inline add x y =
                Gs.context().MkAdd(x, y) :?> IntExpr |> IntExpr

            let inline subtract x y =
                Gs.context().MkSub(x, y) :?> IntExpr |> IntExpr

            let inline multiply x y =
                Gs.context().MkMul(x, y) :?> IntExpr |> IntExpr

            let inline divide x y =
                Gs.context().MkDiv(x, y) :?> IntExpr |> IntExpr

            let inline exp x y =
                let rec loop i acc =
                    if i = 0I then
                        acc
                    else
                        loop (i - 1I) (Gs.context().MkMul(acc, x))

                if y = 0I then
                    (createInt 0I) :> IntExpr |> IntExpr
                elif y > 0I then
                    loop (y - 1I) x :?> IntExpr |> IntExpr
                else
                    failwith "Coefficient should be a non-negative integer"

            let inline gt x y = Gs.context().MkGt(x, y) |> BoolExpr
            let inline eq x y = Gs.context().MkEq(x, y) |> BoolExpr
            let inline ge x y = Gs.context().MkGe(x, y) |> BoolExpr
            let inline lt x y = Gs.context().MkLt(x, y) |> BoolExpr

            let inline ueq x y =
                Gs.context().MkDistinct(x, y) |> BoolExpr

            let inline le x y = Gs.context().MkLe(x, y) |> BoolExpr
            let inline distinct xs = Gs.context().MkDistinct xs |> BoolExpr

            let inline createITE b expr1 expr2 =
                Gs.context().MkITE(b, expr1, expr2) :?> IntExpr
                |> IntExpr

        type Int with
            static member (+)(IntExpr x, IntExpr y) = add x y
            static member (+)(IntExpr x, y) = add x (createInt y)
            static member (+)(x, IntExpr y) = add (createInt x) y
            static member (-)(IntExpr x, IntExpr y) = subtract x y
            static member (-)(IntExpr x, y) = subtract x (createInt y)
            static member (-)(x, IntExpr y) = subtract (createInt x) y
            static member (*)(IntExpr x, IntExpr y) = multiply x y
            static member (*)(IntExpr x, y) = multiply x (createInt y)
            static member (*)(x, IntExpr y) = multiply (createInt x) y
            static member (/)(IntExpr x, IntExpr y) = divide x y
            static member (/)(IntExpr x, y) = divide x (createInt y)
            static member (/)(x, IntExpr y) = divide (createInt x) y
            static member Pow(IntExpr x, y) = exp x y // use this name instead of ( ** )
            static member (>.)(IntExpr x, IntExpr y) = gt x y
            static member (>.)(IntExpr x, y) = gt x (createInt y)
            static member (>.)(x, IntExpr y) = gt (createInt x) y
            static member (=.)(IntExpr x, IntExpr y) = eq x y
            static member (=.)(IntExpr x, y) = eq x (createInt y)
            static member (=.)(x, IntExpr y) = eq (createInt x) y
            static member (>=.)(IntExpr x, IntExpr y) = ge x y
            static member (>=.)(IntExpr x, y) = ge x (createInt y)
            static member (>=.)(x, IntExpr y) = ge (createInt x) y
            static member (<.)(IntExpr x, IntExpr y) = lt x y
            static member (<.)(IntExpr x, y) = lt x (createInt y)
            static member (<.)(x, IntExpr y) = lt (createInt x) y
            static member (<>.)(IntExpr x, IntExpr y) = ueq x y
            static member (<>.)(IntExpr x, y) = ueq x (createInt y)
            static member (<>.)(x, IntExpr y) = ueq (createInt x) y
            static member (<=.)(IntExpr x, IntExpr y) = le x y
            static member (<=.)(IntExpr x, y) = le x (createInt y)
            static member (<=.)(x, IntExpr y) = le (createInt x) y

            static member Distinct xs =
                Array.map (fun (IntExpr expr) -> expr :> Expr) xs
                |> createDistinct

            static member If(BoolExpr b, IntExpr expr1, IntExpr expr2) = createITE b expr1 expr2

        /// Return an int const with supplied name
        let Int (s: string) =
            let context = Gs.context ()
            context.MkIntConst s |> IntExpr

        let IntVal (i: bigint) =
            let context = Gs.context ()
            context.MkInt(i.ToString()) :> IntExpr |> IntExpr

    module public Real =
        open Bool

        type Real(expr: RealExpr) =
            inherit Theory()
            override x.Expr = expr :> Expr
            override x.ToString() = sprintf "%O" expr
            static member FromExpr(e: Expr) = Real(e :?> RealExpr)

        let RealExpr expr = Real(expr)
        let (|RealExpr|) (r: Real) = r.Expr :?> RealExpr

        [<AutoOpen>]
        module public RealUtils =
            let inline fromFloat (x: float) = Gs.context().MkReal(string x)
            let inline fromDecimal (x: decimal) = Gs.context().MkReal(string x)

            let inline add x y =
                Gs.context().MkAdd(x, y) :?> RealExpr |> RealExpr

            let inline subtract x y =
                Gs.context().MkSub(x, y) :?> RealExpr |> RealExpr

            let inline multiply x y =
                Gs.context().MkMul(x, y) :?> RealExpr |> RealExpr

            let inline divide x y =
                Gs.context().MkDiv(x, y) :?> RealExpr |> RealExpr

            let inline exp x y =
                let rec loop i acc =
                    if i = 0I then
                        acc
                    else
                        loop (i - 1I) (Gs.context().MkMul(acc, x))

                if y = 0I then
                    (fromFloat 0.) :> RealExpr |> RealExpr
                elif y > 0I then
                    loop (y - 1I) x :?> RealExpr |> RealExpr
                else
                    failwith "Coefficient should be a non-negative integer"

            let inline gt x y = Gs.context().MkGt(x, y) |> BoolExpr
            let inline eq x y = Gs.context().MkEq(x, y) |> BoolExpr
            let inline ge x y = Gs.context().MkGe(x, y) |> BoolExpr
            let inline lt x y = Gs.context().MkLt(x, y) |> BoolExpr

            let inline ueq x y =
                Gs.context().MkDistinct(x, y) |> BoolExpr

            let inline le x y = Gs.context().MkLe(x, y) |> BoolExpr
            let inline distinct (xs: Expr []) = Gs.context().MkDistinct xs |> BoolExpr

            let inline createITE b expr1 expr2 =
                Gs.context().MkITE(b, expr1, expr2) :?> RealExpr
                |> RealExpr

        type Real with
            static member (+)(RealExpr x, RealExpr y) = add x y
            static member (+)(RealExpr x, y) = add x (fromFloat y)
            static member (+)(x, RealExpr y) = add (fromFloat x) y
            static member (+)(RealExpr x, y) = add x (fromDecimal y)
            static member (+)(x, RealExpr y) = add (fromDecimal x) y
            static member (-)(RealExpr x, RealExpr y) = subtract x y
            static member (-)(RealExpr x, y) = subtract x (fromFloat y)
            static member (-)(x, RealExpr y) = subtract (fromFloat x) y
            static member (-)(RealExpr x, y) = subtract x (fromDecimal y)
            static member (-)(x, RealExpr y) = subtract (fromDecimal x) y
            static member (*)(RealExpr x, RealExpr y) = multiply x y
            static member (*)(RealExpr x, y) = multiply x (fromFloat y)
            static member (*)(x, RealExpr y) = multiply (fromFloat x) y
            static member (*)(RealExpr x, y) = multiply x (fromDecimal y)
            static member (*)(x, RealExpr y) = multiply (fromDecimal x) y
            static member (/)(RealExpr x, RealExpr y) = divide x y
            static member (/)(RealExpr x, y) = divide x (fromFloat y)
            static member (/)(x, RealExpr y) = divide (fromFloat x) y
            static member (/)(RealExpr x, y) = divide x (fromDecimal y)
            static member (/)(x, RealExpr y) = divide (fromDecimal x) y
            static member Pow(RealExpr x, y) = exp x y
            static member (>.)(RealExpr x, RealExpr y) = gt x y
            static member (>.)(RealExpr x, y) = gt x (fromFloat y)
            static member (>.)(x, RealExpr y) = gt (fromFloat x) y
            static member (>.)(RealExpr x, y) = gt x (fromDecimal y)
            static member (>.)(x, RealExpr y) = gt (fromDecimal x) y
            static member (=.)(RealExpr x, RealExpr y) = eq x y
            static member (=.)(RealExpr x, y) = eq x (fromFloat y)
            static member (=.)(x, RealExpr y) = eq (fromFloat x) y
            static member (=.)(RealExpr x, y) = eq x (fromDecimal y)
            static member (=.)(x, RealExpr y) = eq (fromDecimal x) y
            static member (>=.)(RealExpr x, RealExpr y) = ge x y
            static member (>=.)(RealExpr x, y) = ge x (fromFloat y)
            static member (>=.)(x, RealExpr y) = ge (fromFloat x) y
            static member (>=.)(RealExpr x, y) = ge x (fromDecimal y)
            static member (>=.)(x, RealExpr y) = ge (fromDecimal x) y
            static member (<.)(RealExpr x, RealExpr y) = lt x y
            static member (<.)(RealExpr x, y) = lt x (fromFloat y)
            static member (<.)(x, RealExpr y) = lt (fromFloat x) y
            static member (<.)(RealExpr x, y) = lt x (fromDecimal y)
            static member (<.)(x, RealExpr y) = lt (fromDecimal x) y
            static member (<>.)(RealExpr x, RealExpr y) = ueq x y
            static member (<>.)(RealExpr x, y) = ueq x (fromFloat y)
            static member (<>.)(x, RealExpr y) = ueq (fromFloat x) y
            static member (<>.)(RealExpr x, y) = ueq x (fromDecimal y)
            static member (<>.)(x, RealExpr y) = ueq (fromDecimal x) y
            static member (<=.)(RealExpr x, RealExpr y) = le x y
            static member (<=.)(RealExpr x, y) = le x (fromFloat y)
            static member (<=.)(x, RealExpr y) = le (fromFloat x) y
            static member (<=.)(RealExpr x, y) = le x (fromDecimal y)
            static member (<=.)(x, RealExpr y) = le (fromDecimal x) y

            static member Distinct xs =
                Array.map (fun (RealExpr expr) -> expr :> Expr) xs
                |> distinct

            static member If(BoolExpr b, RealExpr expr1, RealExpr expr2) = createITE b expr1 expr2

        /// Return a real const with supplied name
        let Real (s: string) =
            let context = Gs.context ()
            context.MkRealConst s |> RealExpr

        let RealVal (f: float) =
            let context = Gs.context ()

            context.MkReal(f.ToString()) :> RealExpr
            |> RealExpr
