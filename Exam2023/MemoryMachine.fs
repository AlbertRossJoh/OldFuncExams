module MemoryMachine
    open MMScaffold
    
    let (.+.) e1 e2 = Plus(e1, e2)
    let (.-.) e1 e2 = Minus(e1, e2)
    let (.<-.) e1 e2 = Assign (e1, e2)
    
    let emptyMem x : mem =
        Array.zeroCreate x
        
    let lookup (m: mem) i =
        m[i]
        
    let assign (m: mem) i v =
        let cpy = m.Clone() :?> int array
        cpy[i] <- v
        cpy

    let rec evalExpr m exp =
        match exp with
        | Num i -> i
        | Lookup(exp) -> lookup m (evalExpr m exp)
        | Minus(expr, expr1) -> (evalExpr m expr) - (evalExpr m expr1)
        | Plus(expr, expr1) -> (evalExpr m expr) + (evalExpr m expr1)
    and evalStmnt m stmnt : mem =
        match stmnt with
        | Assign(expr, expr1) -> assign m (evalExpr m expr) (evalExpr m expr1)
        | While(expr, stmnts) ->
            if evalExpr m expr = 0 then
                m
            else
                evalProg m (stmnts@[While(expr, stmnts)])
    and evalProg m prog =
        (m, prog) ||> List.fold evalStmnt
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
        
        
    let lookup2 i : StateMonad<int> =
        SM(fun s ->
            if i < 0 || i >= Array.length s then
                None
            else
                Some((lookup s i), s))
    let assign2 i v =
        lookup2 i >>= fun _ ->
            SM(fun s -> Some((), assign s i v))
        
   
    
    let binop f sm1 sm2 =
        sm1 >>= fun i1 ->
        sm2 >>= fun i2 ->
        ret (f i1 i2)
    let rec evalExpr2 expr =
        match expr with
        | Num i -> ret i
        | Lookup(exp) ->  evalExpr2 exp >>= lookup2
        | Minus(expr, expr1) -> binop (-) (evalExpr2 expr) (evalExpr2 expr1)
        | Plus(expr, expr1) -> binop (+) (evalExpr2 expr) (evalExpr2 expr1)
    and evalStmnt2 stmnt =
        match stmnt with
        | Assign(expr, expr1) ->
            evalExpr2 expr >>= fun i ->
            evalExpr2 expr1 >>= assign2 i 
        | While(expr, stmnts) ->
            evalExpr2 expr >>= fun e ->
            if e = 0 then
                ret ()
            else
                evalProg2 (stmnts@[While(expr, stmnts)])
    and evalProg2 prog =
        match prog with
        | [] -> ret ()
        | x::xs ->
            evalStmnt2 x >>>= evalProg2 xs
    
    open FParsec
    
    type Parser<'a> = Parser<'a, unit>
    type ParserResult<'a> = ParserResult<'a, unit>
    let curry f = fun (a, b) -> f a b
    let binop2 pOp p1 p2 = p1 .>> pOp .>>. p2
    let parenthesise p = pchar '[' >>. p .>> pchar ']'
    let choice ps = choice (Seq.map attempt ps)
    let createParserForwardedToRef () : Parser<'a> * Parser<'a> ref = createParserForwardedToRef ()
    let ParseExpr, eref = createParserForwardedToRef<expr>()
    let ParseAtom, aref = createParserForwardedToRef<expr>()
    
    let parseExpr =
        choice [
            binop2 (pchar '+') ParseAtom ParseExpr |>> curry (.+.)
            binop2 (pchar '-') ParseAtom ParseExpr |>> curry (.-.)
            parenthesise ParseExpr |>> Lookup
            ParseAtom
        ]
    let parseAtom =
        choice [
            pint32 |>> Num
        ]
    do aref := parseAtom
    do eref := parseExpr
    