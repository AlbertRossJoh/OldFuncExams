module ReExam2023

    open System

(* If you are importing this into F# interactive then comment out
   the line above and remove the comment for the line bellow.

   Do note that the project will not compile if you do this, but 
   it does allow you to work in interactive mode and you can just remove the '=' 
   to make the project compile again.

   You will also need to load JParsec.fs. Do this by typing
   #load "JParsec.fs" 
   in the interactive environment. You may need the entire path.

   Do not remove the module declaration (even though that does work) because you may inadvertently
   introduce indentation errors in your code that may be hard to find if you want
   to switch back to project mode. 

   Alternative, keep the module declaration as is, but load ExamInteractive.fsx into the interactive environment
   *)
(*
 module ReExam2023 = 
 *)

(* 1: Arithmetic *)
    
    type arith =
    | Num of int
    | Add of arith * arith
    
    let p1 = Num 42
    let p2 = Add(Num 5, Num 3)
    let p3 = Add(Add(Num 5, Num 3), Add(Num 7, Num (-9)))
    
(* Question 1.1: Evaluation *)
    let rec eval a =
        match a with
        | Num x -> x
        | Add(arith, arith1) -> eval arith + eval arith1
    
(* Question 1.2: Negation and subtraction *)
    let rec negate a =
        match a with
        | Num x -> Num -x
        | Add(arith, arith1) -> Add(negate arith, negate arith1)
        
    let rec subtract a b =
        Add(a, negate b)
        

(* Question 1.3: Multiplication *)
        
    let rec multiply a b =
        match a, b with
        | Num(x), Num(y) -> x * y |> Num
        | x, Add(a, b) -> Add((multiply x a), (multiply x b))
        | Add(a, b), c -> Add((multiply a c), (multiply b c))
    
(* Question 1.4: Exponents *)

    let rec pow a b =
        let rec inner f a b =
            if eval b = 1 then
                f a
            else
                inner (fun x -> f (multiply a x)) a (subtract b (Num 1))
        inner id a b
    
(* Question 1.5: Iteration *)

    let rec iterate f acc a =
        if eval a = 1 then
            f acc
        else
           iterate f acc (Add(a, (Num -1))) |> f
        
    let pow2 a b =
        iterate (multiply a) a (Add(b, (Num -1)))
    
(* 2: Code Comprehension *)
 
    let rec isEven =
        function
        | 0            -> true
        | x when x > 0 -> isUnEven (x - 1)
        | x            -> isUnEven (x + 1)
        
    and isUnEven =
        function
        | 0            -> false
        | x when x > 0 -> isEven (x - 1)
        | x            -> isEven (x + 1)
        
    let rec getEvenAndUneven =
        function
        | []                 -> [], []
        | x :: xs when isEven x ->
            let ys, zs = getEvenAndUneven xs
            (x::ys, zs)
        | x :: xs ->
            let ys, zs = getEvenAndUneven xs
            (ys, x::zs)
        

(* Question 2.1: Types, names and behaviour *)

    (* 
    
    Q: What are the types of functions foo, bar, and baz?

    A: 
    foo : int -> bool
    bar : int -> bool
    baz : int list ->  int list * int list


    Q: What do the function foo, bar, and baz do.
       Focus on what they do rather than how they do it.

    A: 
    foo checks if a number is even
    bar checks if a number is uneven
    baz splits an array of ints into even and uneven
    
    Q: What would be appropriate names for functions 
    
    A:
       foo, bar, and baz?
       foo : isEven
       bar : isUneven
       baz : getEvenAndUneven
    *)
        

(* Question 2.2: Code snippets *)

 
    (* 
    The function baz contains the following three code snippets. 

    * A: `baz xs`
    * B: `bar x`
    * C: `(ys, x::zs)`

    Q: In the context of the baz function, i.e. assuming that `x`, `xs`, `ys`, and `zs` all have the correct types,
       what are the types of snippets A, B, and C, expressed using the F# syntax for types, and what are they -- 
       focus on what they do rather than how they do it.
    
    A: baz xs = int list * int list
        int list split into even and uneven
    B: bar x = bool 
        is uneven
    C: (ys, x::zs) = int list * int list
        (even ints, uneven ints)
    
    Q: * Explain the use of the `and`-operator that connect the `foo` and the `bar` functions.
       * Argue if the program would work if you replaced `and` with `let rec`.

    A:  No it would not work as the and keyword is used for mutual recursion.
        If we were to use `let rec` we would need to define one or the other.
    *)

(* Question 2.3: No recursion *) 

    let foo2 a = a % 2 = 0
    let bar2 a = a % 2 <> 0

(* Question 2.4: Tail Recursion *)

    (*

    Q: The function `baz` is not tail recursive. Demonstrate why.
       To make a compelling argument you should evaluate a function call of the function,
       similarly to what is done in Chapter 1.4 of HR, and reason about that evaluation. 
       You need to make clear what aspects of the evaluation tell you that the function 
       is not tail recursive. Keep in mind that all steps in an evaluation chain must 
       evaluate to the same value ( (5 + 4) * 3 --> 9 * 3 --> 27 , for instance).
       
       You do not have to step through the foo- or the bar-functions. You are allowed to evaluate 
       those function immediately.

    A: the last call of the function is not to baz it is either x::ys or x::zs
    
    *)
(* Question 2.5: Continuations *)

    let bazTail =
        let rec inner cont a =
            match a with
            | [] -> [],[]
            | [x] ->
                if isEven x then
                    cont ([x], [])
                else
                    cont ([], [x])
            | x::xs ->
                inner (fun next ->
                    let evens, odds = next
                    let ret =
                        if isEven x then (x::evens, odds)
                        else (evens, x::odds)
                    cont ret) xs
        inner id 
            

(* 3: Balanced brackets *)

      
    let explode (str : string) = [for c in str -> c]
    let implode (lst : char list) = lst |> List.toArray |> System.String
    
(* Question 3.1: Balanced brackets *)
    
    let other c =
        match c with
        | '(' -> ')'
        | '[' -> ']'
        | '{' -> '}'
        // | ')' -> '('
        // | ']' -> '['
        // | '}' -> '{'
        | _ -> '#'
        
    let balanced str =
        let rec inner stack rest =
            match rest with
            | [] -> List.isEmpty stack
            | x::xs ->
                matchWithStack stack xs x
        and matchWithStack stack rest  x =
            match stack with
            | [] -> inner (x::stack) rest
            | y::ys ->
                if other y = x then
                    inner ys rest
                else
                    inner (x::y::ys) rest
        inner [] (explode str)
                
        
(* Question 3.2: Arbitrary delimiters *)
    
    let balanced2 dict str =
        let tryFindOther c =
            Map.tryFind c dict
        let rec inner stack rest =
            match rest with
            | [] -> List.isEmpty stack
            | nextChar::xs ->
                matchWithStack stack xs nextChar
        and matchWithStack stack rest nextChar =
            match stack with
            | [] -> inner (nextChar::stack) rest
            | previousChar::ys ->
                match tryFindOther previousChar, tryFindOther nextChar with
                | Some _, Some _ ->
                        inner (nextChar::previousChar::ys) rest || inner ys rest
                | Some(prevOther), None when prevOther = nextChar->
                        inner ys rest
                | _ -> inner (nextChar::previousChar::ys) rest
                    
        inner [] (explode str)
        
    
(* Question 3.3: Matching brackets and palindromes *)    
    
    let balanced3 str =
        ([], explode str) ||> List.fold (fun stack next ->
                match stack with
                | [] -> next::stack
                | x::xs when other x = next -> xs
                | stack -> next::stack
        ) |> List.isEmpty
        
    
    let removeLast lst =
        match lst with
        | [] -> []
        | _ -> List.removeAt (List.length lst - 1) lst
        
    let symmetric str =
        let a = str
                |> explode
                |> List.filter Char.IsLetter
                |> List.map Char.ToLower
                |> List.splitInto 2
            
        if List.length a = 0 || List.length a = 1 then
            true
        else
            let a, b = (a[0], a[1])
            let len = List.length
            let a, b = 
                if len a <> len b then
                    (removeLast a, b)
                else
                    (a, b)
            (true, a, List.rev b) |||> List.fold2 (fun isEqual a b ->
                    isEqual && a = b)
        
(* Question 3.4: Parsing balanced brackets *)    
               
    open JParsec.TextParser
    
    let parenthesiseBracket p =  pchar '[' >>. p .>> pchar ']'
    let parenthesiseReg p = pchar '(' >>. p .>> pchar ')'
    let parenthesiseTuborg p = pchar '{' >>. p .>> pchar '}'
    let ParseBalanced, bref = createParserForwardedToRef<unit>()
    
    let parseBalancedAux =
        many (choice [
            between (pchar '[') (pchar ']') ParseBalanced |>> (fun _ -> ())
            between (pchar '{') (pchar '}') ParseBalanced |>> (fun _ -> ())
            between (pchar '(') (pchar ')') ParseBalanced |>> (fun _ -> ())
        ]) |>> (fun _ -> ())
        
    // uncomment after you have done parseBalancedAUX
    
    let parseBalanced = parseBalancedAux .>> pstring "**END**"
    do bref := parseBalancedAux
            
(* Question 3.5: Parallel counting *)

    let countBalanced lst i =
        List.splitInto i lst
        |> List.toArray
        |> Array.Parallel.map
               (List.map balanced3
                >> List.filter id
                >> List.length)
        |> Array.sum
        

(* 4: BASIC *)
    
    
    type var = string

    type expr =  
    | Num    of int              // Integer literal
    | Lookup of var              // Variable lookup
    | Plus   of expr * expr      // Addition
    | Minus  of expr * expr      // Subtraction
    
    type stmnt =
    | If of expr * uint32       // Conditional statement (if-then (no else)).
    | Let of var * expr        // Variable update/declaration
    | Goto of uint32           // Goto
    | End                      // Terminate program
      
    type prog = (uint32 * stmnt) list  // Programs are sequences of commands with their own line numbers 

    
    let (.+.) e1 e2 = Plus(e1, e2)  
    let (.-.) e1 e2 = Minus(e1, e2)  
    
    let fibProg xarg =  
        [(10u, Let("x",    Num xarg))                         // x = xarg
         (20u, Let("acc1", Num 1))                            // acc1 = 1
         (30u, Let("acc2", Num 0))                            // acc2 = 0
         
         (40u, If(Lookup "x", 60u))                           // if x > 0 then goto 60 (start loop)
         
         (50u, Goto 110u)                                     // Goto 110 (x = 0, terminate program)
         
         (60u, Let ("x", Lookup "x" .-. Num 1))               // x = x - 1
         (70u, Let ("result", Lookup "acc1"))                 // result = acc1
         (80u, Let ("acc1", Lookup "acc1" .+. Lookup "acc2")) // acc1 = acc1 + acc2
         (90u, Let ("acc2", Lookup "result"))                 // acc2 = result
         (100u, Goto 40u)                                     // Goto 40u (go to top of loop)
         
         (110u, End)                                          // Terminate program
                                                              // the variable result contains the
                                                              // fibonacci number of xarg
         ]

(* Question 4.1: Basic programs *)

    type basicProgram = Map<uint32, stmnt>
    
    let mkBasicProgram (prog: prog) : basicProgram=
        Map.ofList prog
    let getStmnt (l: uint32) (bp: basicProgram) =
        Map.find l bp
    
    let nextLine (l: uint32) (bp: basicProgram) =
        Map.tryFindKey (fun k _ -> k > l) bp |> Option.get
    
    let firstLine (bp: basicProgram) : uint32 =
        Map.minKeyValue bp |> fst
    
(* Question 4.2: State *)

    type state = {
        lineNumber: uint32
        state: Map<string, int>
    }
    
    let getLineNumber (st: state) =
        st.lineNumber
    
    let getState (st: state) =
        st.state
    let emptyState bp =
        {
            lineNumber = firstLine bp
            state = Map.empty 
        }
    
    
    let goto l st =
       { st with lineNumber = l }

    let getCurrentStmnt bp st =
        getStmnt (getLineNumber st) bp
    
    let update v a st =
        { st with state = Map.add v a (getState st) }
    
    let lookup v st =
        st |> getState |> Map.find v
    
    
(* Question 4.3: Evaluation *)
    
    let rec evalExpr e st =
        match e with
        | Num x -> x
        | Lookup v -> lookup v st
        | Plus(e1, e2) -> evalExpr e1 st + evalExpr e2 st
        | Minus(e1, e2) -> evalExpr e1 st - evalExpr e2 st
    
    
    let step bp st =
        { st with lineNumber = bp |> nextLine (getLineNumber st) }
  
        
    let evalProg bp =
        let st = emptyState bp
        let rec inner st =
            match getCurrentStmnt bp st with
            | If(e, l) ->
                let stm = evalExpr e st <> 0
                let ret = 
                    if stm then
                        goto l st
                    else
                        st |> step bp
                inner ret
            | Let(v, e) ->
                let e = evalExpr e st
                update v e st |> goto (nextLine (getLineNumber st) bp) |> inner
            | Goto l -> goto l st |> inner
            | End -> st
        inner st
    
(* Question 4.4: State monad *)
    type StateMonad<'a> = SM of (basicProgram -> state -> 'a * state)  
      
    let ret x = SM (fun _ s -> (x, s))
    
    let bind f (SM a) : StateMonad<'b> =   
        SM (fun p s ->
            let x, s' = a p s
            let (SM g) = f x
            g p s')
          
    let (>>=) x f = bind f x  
    let (>>>=) x y = x >>= (fun _ -> y)  
      
    let evalSM p (SM f) = f p (emptyState p)

    let goto2 _ = failwith "not implemented"
    
    let getCurrentStmnt2 _ = failwith "not implemented"
    
    
    let lookup2 _ = failwith "not implemented"
    let update2 _ = failwith "not implemented"
    
    let step2 _ = failwith "not implemented"

(* Question 4.5: State monad evaluation *)

    type StateBuilder() =

        member this.Bind(f, x)    = bind x f
        member this.Return(x)     = ret x
        member this.ReturnFrom(x) = x
        member this.Combine(a, b) = a >>= (fun _ -> b)

    let state = StateBuilder()

    let evalExpr2 _ = failwith "not implemented"
    
    let evalProg2 _ = failwith "not implemented"
        