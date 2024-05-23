module MMScaffold
    type expr =
    | Num    of int
    | Lookup of expr
    | Plus   of expr * expr
    | Minus  of expr * expr
    type stmnt =
    | Assign of expr * expr
    | While  of expr * prog
    and prog = stmnt list
    type mem = int array
    
    let (.+.) e1 e2 = Plus(e1, e2)
    let (.-.) e1 e2 = Minus(e1, e2)
    let (.<-.) e1 e2 = Assign (e1, e2)
    
    let fibProg x =
        [
            Num 0 .<-. Num x
            Num 1 .<-. Num 1
            Num 2 .<-. Num 0
            While(Lookup (Num 0),[
                Num 0 .<-. Lookup (Num 0) .-. Num 1
                Num 3 .<-. Lookup (Num 1)
                Num 1 .<-. Lookup (Num 1) .+. Lookup (Num 2)
                Num 2 .<-. Lookup (Num 3)
            ])
        ]
         
    type StateMonad<'a> = SM of (mem -> ('a * mem) option)
    let ret x = SM(fun s -> Some(x, s))
    let fail = SM(fun _ -> None)
    let bind f (SM(a)) =
        SM(fun s ->
            match a s with
            | Some(x, s') ->
                let (SM(g)) = f x
                g s'
            | None -> None)
    
    let (>>=) x f = bind f x
    let (>>>=) x y = x >>= (fun _ -> y)
    let evalSM m (SM f) = f m
    
    type StateBuilder() =
        member this.Bind(f, x) = bind x f
        member this.Return(x) = ret x
        member this.ReturnFrom(x) = x
        member this.Combine(a, b) = a >>= (fun _ -> b)
    
    let state = StateBuilder()
    open FParsec
         
    type Parser<'a> = Parser<'a, unit>
    type ParserResult<'a> = ParserResult<'a, unit>   
    let choice ps = choice (Seq.map attempt ps)
    let createParserForwardedToRef () : Parser<'a> * Parser<'a> ref = createParserForwardedToRef ()
        
