module Exam2021_2

    open System
    open Microsoft.FSharp.Core

(* If you are importing this into F# interactive then comment out
   the line above and remove the comment for the line bellow.

   Do note that the project will not compile if you do this, but 
   it does allow you to work in interactive mode and you can just remove the '=' 
   to make the project compile again.

   You will also need to load JParsec.fs. Do this by typing
   #load "JParsec.fs" 
   in the interactive environment. You may need the entire path.

   Do not remove the module declaration (even though that does work) because you may inadvertantly
   introduce indentation errors in your code that may be hard to find if you want
   to switch back to project mode. 

   Alternative, keep the module declaration as is, but load ExamInteractive.fsx into the interactive environment
   *)
(*
 module Exam2021_2 = 
 *)

(* 1: Binary lists *)

(* Question 1.1 *)

    type binList<'a, 'b> =
    | Nil
    | Cons1 of 'a * binList<'a, 'b>
    | Cons2 of 'b * binList<'a, 'b>

    let rec length (bl: binList<'a, 'b>) =
        if bl = Nil then
            0
        else 
            let rec inner bl =
                match bl with
                | Nil -> 1
                | Cons1(_, xs) -> 1+inner xs
                | Cons2(_, xs) -> 1+inner xs
            inner bl
        
    
(* Question 1.2 *)
    let split bl =
        let rec inner acc1 acc2 =
            function
            | Nil -> acc1 |> List.rev ,acc2 |> List.rev
            | Cons1(x, xs) -> inner (x::acc1) acc2 xs
            | Cons2(x, xs) -> inner acc1 (x::acc2) xs
        inner [] [] bl
    let length2 bl =
        let rec inner a b bl =
            match bl with
            | Nil -> a,b
            | Cons1(_, xs) -> inner (a+1) b xs
            | Cons2(_, xs) -> inner a (b+1) xs
        inner 0 0 bl
            

(* Question 1.3 *)


    let rec map f1 f2 bl =
        match bl with
        | Nil -> Nil
        | Cons1(x, xs) -> Cons1(f1 x, map f1 f2 xs)
        | Cons2(x, xs) -> Cons2(f2 x, map f1 f2 xs)

(* Question 1.4 *)

    let rec filter f1 f2 bl =
        match bl with
        | Nil -> Nil
        | Cons1(x, xs) ->
            if f1 x then
                Cons1(x, filter f1 f2 xs)
            else
                filter f1 f2 xs
        | Cons2(x, xs) ->
            if f2 x then
                Cons2(x, filter f1 f2 xs)
            else
                filter f1 f2 xs


(* Question 1.5 *)

    let rec fold f1 f2 acc bl =
        match bl with
        | Nil -> acc
        | Cons1(x, xs) -> fold f1 f2 (f1 acc x) xs
        | Cons2(x, xs) -> fold f1 f2 (f2 acc x) xs

(* 2: Code Comprehension *)
    let rec foo xs ys =
      match xs, ys with
      | [], ys -> ys
      | xs, [] -> xs
      | x :: xs, y :: ys when x < y ->
        x :: (foo xs (y :: ys))
      | x :: xs, y :: ys ->
        y :: (foo (x :: xs) ys)

    and bar =
      function
      | [] -> []
      | [x] -> [x]
      | xs ->
        let (a, b) = List.splitAt (List.length xs / 2) xs
        foo (bar a) (bar b)

(* Question 2.1 *)

    (* 
    
    Q: What are the types of functions foo and bar?

    A:  foo: 'a list -> 'a list -> 'a list
        bar: 'a list -> 'a list


    Q: What does the function bar do.
       Focus on what it does rather than how it does it.

    A: bar sorts a list
    
    Q: What would be appropriate names for functions 
       foo and bar?

    A:  bar -> mergeSort
        foo -> merge
    
    Q: What would be appropriate names of the values a and b in bar.
    
    
    A:  fstPartLst sndPartLst
    
    *)
        

(* Question 2.2 *)

 
    (* 
    The code includes the keyword "and".

    
    Q: What function does this keyword serve in general
       (why would you use "and" when writing any program)?

    A: it is for mutual recursion foo needs to call bar and bar foo, this means that they cannot be defined sequentially


    Q: What would happen if you removed it from this particular program and
       replaced it with a standard "let"
       (change the line "and bar = " to "let rec bar = ")?
       Explain why the program either does or does not work.

    A: it would not work

    *)

(* Question 2.3 *) 
    
    // let foo2 xs ys =
    //     List.unfold (fun acc ->
    //         match acc with
    //         | [], [] -> None
    //         | x::xs, [] -> Some((xs, []), x::xs)) (xs, ys)
    (* use the following code as a starting template
    let foo2 xs ys = List.unfold <a function goes here> (xs, ys)
    *)

(* Question 2.4 *)

    (*

    Q: Neither foo nor bar is tail recursive. Pick one (not both) of them and explain why.
       To make a compelling argument you should evaluate a function call of the function,
       similarly to what is done in Chapter 1.4 of HR, and reason about that evaluation.
       You need to make clear what aspects of the evaluation tell you that the function is not tail recursive.
       Keep in mind that all steps in an evaluation chain must evaluate to the same value
       ((5 + 4) * 3 --> 9 * 3 --> 27, for instance).

    A: the last function call is not to the function itself, in foos case the last call will be an cons operation since the branches go into x::foo

    *)
(* Question 2.5 *)

    let fooTail xs ys =
        let rec inner xs ys acc =
            match xs, ys with
            | [], ys -> ys@acc
            | xs, [] -> xs@acc
            | x :: xs, y :: ys ->
              if x < y then
                  inner xs (y::ys) (x::acc)
              else
                  inner (x::xs) ys (y::acc)
        inner xs ys [] |> List.rev
              
        

(* Question 2.5 *)

    let barTail lst =
        let rec inner cont =
            function
            | [] -> cont []
            | [x] -> cont [x]
            | xs ->
                let a, b = List.splitAt (List.length xs / 2) xs
                inner (fun next -> inner (fooTail next) b |> cont ) a 
        inner id lst

(* 3: Approximating square roots *)

    let perfectSquaresSeq =
        Seq.initInfinite (fun i -> i*i)
    
(* Question 3.1 *)

    let approxSquare (x: int) =
        let closest = List.unfold (fun state ->
            if state > x then None
            else Some(state, state*state)) 2 |> List.tryLast 
        let closest =
            match x with
            | x when x < 0 -> failwith "This function cannot handle a negative square root"
            | x when x > 1 -> closest |> Option.get
            | x -> x
        let rec inner (v: float) i =
            if i <= 0 then
                v
            else
                inner ((((float x)/v)+v)/2.) (i-1)
        inner (Math.Sqrt (float closest))

(* Question 3.2 *)

    let quadratic a b c num =
        let solve f =
            let square = (approxSquare (b*b-(4*a*c)) num)
            (f (-b |> float) square)/(2*a |> float)
        (solve (+), solve (-))

(* Question 3.3 *)

    let parQuadratic eqs numProcesses num =
        eqs
        |> Seq.splitInto numProcesses
        |> Seq.toArray
        |> Array.Parallel.map (Array.map (fun (a, b, c) -> quadratic a b c num) >> Array.toList)
        |> Array.fold (fun acc calc -> acc@calc) []

(* Question 3.4 *)
    open JParsec.TextParser
   
    let curry f = fun (a, b) -> f a b
    let parenthesise p = pchar '[' >>. p .>> pchar ']'
    let whitespaceChar = satisfy Char.IsWhiteSpace 
    let spaces = many whitespaceChar
    
    let (.>*>.) p1 p2 = p1 .>> spaces .>>. p2
    let (.>*>) p1 p2 = p1 .>> spaces .>> p2 
    let (>*>.) p1 p2 = p1 .>> spaces >>. p2
    let binop2 pOp p1 p2 = p1 .>*> pOp .>*>. p2
    let ParseExpr, eref = createParserForwardedToRef()
    let ParseOperation, aref = createParserForwardedToRef<(int -> int)>()
    
    let negate x =
        -x
    
    let parseExpr =
        pint32 .>>
        pstring "x^2" .>*>.
        ParseOperation .>*>.
        pint32 .>>
        pchar 'x' .>*>.
        ParseOperation .>*>.
        pint32 .>*>
        pchar '=' .>*>
        pchar '0'
        |>> (fun ((((a,prefixb),b),prefixc),c) -> quadratic a (prefixb b) (prefixc c))
        
    let parseOperation =
        choice [
            pchar '-' |>> (fun _ -> fun x -> -x)
            pchar '+' |>> (fun _ -> fun x -> +x)
        ]
    do eref := parseExpr
    do aref := parseOperation
    let solveQuadratic str =
        run parseExpr str  |> getSuccess

(* 4: Rational numbers *)

(* Question 4.1 *)

    type rat = unit (* replace this entire type with your own *)

(* Question 4.2 *)

    let mkRat _ = failwith "not implemented"
    let ratToString _ = failwith "not implemented"

(* Question 4.3 *)

    let plus _ = failwith "not implemented"
    let minus _ = failwith "not implemented"
    let mult _ = failwith "not implemented"
    let div _ = failwith "not implemented"

(* Question 4.4 *)

    type SM<'a> = SM of (rat -> ('a * rat) option)
    let ret x = SM (fun st -> Some (x, st))
    let bind (SM m) f =
        SM (fun st ->
            match m st with
            | None -> None
            | Some (x, st') ->
                let (SM g) = f x
                g st')
        
    let (>>=) m f = bind m f
    let (>>>=) m n = m >>= (fun () -> n)
    let evalSM (SM f) s = f s 

    let smPlus _ = failwith "not implemented"
    let smMinus _ = failwith "not implemented"
    let smMult _ = failwith "not implemented"
    let smDiv _ = failwith "not implemented"

(* Question 4.5 *)

    (* You may solve this exercise either using monadic operators or 
        using computational expressions. *)

    type StateBuilder() =

        member this.Bind(x, f)    = bind x f
        member this.Zero ()       = ret ()
        member this.Return(x)     = ret x
        member this.ReturnFrom(x) = x
        member this.Combine(a, b) = a >>= (fun _ -> b)

    let state = new StateBuilder()

    let calculate _ = failwith "not implemented"