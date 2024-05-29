module Exam2024
open System
open Microsoft.FSharp.Collections
open Microsoft.FSharp.Core
(* If you are importing this into F# interactive then comment out
   the line above and remove the comment for the line bellow.

   Do note that the project will not compile if you do this, but 
   it does allow you to work in interactive mode and you can just remove the '=' 
   to make the project compile again.

   You will also need to load JParsec.fs. Do this by typing
   #load "JParsec.fs" 
   in the interactive environment. You may need the entire path.

   Do not remove the module declaration (even though that does work) because you may inadvertently
   introduce indentation errors in your code that may be hard to find when switching back to project mode.

   Alternative, keep the module declaration as is, but load ExamInteractive.fsx into the interactive environment
   *)
(*
 module Exam2024 = 
 *)

(* 1: Transactions *)

    type transactions =
        | Empty
        | Pay     of string * int * transactions
        | Receive of string * int * transactions
        
    let rec balance trans =
        match trans with
        | Empty -> 0
        | Pay(_, amount, trs) -> balance trs - amount
        | Receive(_, amount, trs) -> balance trs + amount
        
    let balanceAcc trans =
        let rec aux trans acc =
            match trans with
            | Empty -> acc
            | Pay(_, amount, trs) -> aux trs (acc-amount)
            | Receive(_, amount, trs) -> aux trs (acc+amount)
        aux trans 0
        
        
    let participants trans =
        let rec aux trans payed received =
            match trans with
            | Empty -> payed, received
            | Pay(name, _, trs) -> aux trs (Set.add name payed) received
            | Receive(name, _, trs) -> aux trs payed (Set.add name received)
        aux trans Set.empty Set.empty
    
    let balanceFold payF receiveF acc trs =
        let rec aux acc trs =
            match trs with
            | Empty -> acc
            | Pay(name, amount, trs) ->
                let acc = payF acc name amount
                aux acc trs
            | Receive(name, amount, trs) ->
                let acc = receiveF acc name amount
                aux acc trs
        aux acc trs
    let col (op: int -> int) (acc: Map<string,int>) name amount =
        let amount = op amount
        if Map.containsKey name acc then
            let curr = Map.find name acc
            Map.add name (curr + amount) acc
        else
            Map.add name amount acc
    let collect trs =
        (Map.empty<string,int>, trs) ||>
        balanceFold (col (fun x -> -x)) (col id)
    
    
(* 2: Code Comprehension *)
        
    let foo (x : char) = x |> int |> fun y -> y - (int '0')
    
    let bar (x : string) = [for c in x -> c]
            
    let rec baz =
        function
        | [] -> 0
        | x :: xs -> x + 10 * baz xs
    
(* Question 2.1 *)

    (* 
    
    Q: What are the types of functions foo, bar, and baz?

    A: 
        foo : char -> int
        bar : string -> char list
        baz : int list -> int


    Q: What do the function foo, bar, and baz do.
       Focus on what they do rather than how they do it.

    A: 
        foo converts a char to an int
        In this case it means that '1' becomes 1 or '9' becomes 9.
        
        bar converts a string to a char list 
        this means that "hello" becomes ['h';'e';'l';'l';'o']
        
        baz converts a list of ints to its base 10 representation(in reverse) 
        this means that given a list [0;1;2] we get 210 of type int
        
        I believe the usage would be:
        "1301" |> bar |> List.map foo |> List.rev |> baz
        this would result in the integer 1301
        
        
        
    Q: What would be appropriate names for functions 
       foo, bar, and baz?

    A:  foo -> charToInt
        bar -> stringToCharList
        baz -> convertToBase10Rev
    
    Q: The function foo only behaves reasonably if certain 
       constraint(s) are met on its argument. 
       What is/are these constraints?
        
    A: foo expects the value to be a char representation of an integer 
       it wont return a reasonable result if given e.g 'a' of which the result would be 49
    
    Q: The function baz only behaves reasonably if certain 
       constraint(s) are met on its argument. 
       What is/are these constraints?
        
    A:  if we want to convert a list of ints to its base 10 representation we need to remember to reverse it first 
        this is because [1;0;0] wont result in 100 it will result in 1
        as it will be evaluated as
        1+10*(0+10*(0))
    *)
    
(* Question 2.2 *)
    
    let stringToInt =
        bar >> List.map foo >> List.rev >> baz

(* Question 2.3 *)
    
    let uncurry f a b = f(a,b)
    let baz2 lst =
        (0, lst |> List.indexed)
        ||> List.fold (fun acc (idx, num) ->
            let tmp = uncurry Math.Pow 10. idx |> int
            acc+num*tmp)
    
(* Question 2.4 *)

    (*

    Q: The function `baz` from Question 2.1 is not tail recursive. Demonstrate why.
       To make a compelling argument you should evaluate a function call of the function,
       similarly to what is done in Chapter 1.4 of HR, and reason about that evaluation. 
       You need to make clear what aspects of the evaluation tell you that the function 
       is not tail recursive. Keep in mind that all steps in an evaluation chain must 
       evaluate to the same value ( (5 + 4) * 3 --> 9 * 3 --> 27 , for instance).
       

    A: 
        A tail recursive function is defined by its last call being to it self which is not the case for baz. This is because it performs a plus and multiplication operation on its own returned value.
        
        This can be demonstrated by evaluating [3;2;1]
        baz [3;2;1]
        3+10*(baz [2;1])
        3+10*(2+10*(baz [1]))
        3+10*(2+10*(1+10*(baz [])))
        3+10*(2+10*(1+10*(0)))
        3+10*(2+10*(1))
        3+10*(2+10)
        3+10*(2+10)
        3+10*(12)
        3+120
        123
        as we see here the last operations evaluated are the arithmetic operations and not baz

    *)
    
(* Question 2.5 *)

    (*
        This is tail recursive as the same input would result in
        bazTail [3;2;1]
        aux id [3;2;1]
        aux (fun next -> 3+10*next |> id) [2;1]
        aux (fun next -> 2+10*next |> (fun next -> 3+10*next |> id) [1]
        aux (fun next -> 1+10*next |> (fun next -> 2+10*next |> (fun next -> 3+10*next |> id))) []
        (fun 0 -> 1+10*0 |> (fun next -> 2+10*next |> (fun next -> 3+10*next |> id)) 
        (fun 1 -> 2+10*1 |> (fun next -> 3+10*next |> id)
        (fun 12 -> 3+10*12 |> id)
        (fun 123 -> 123)
        123
    *)
    let bazTail =
        let rec aux cont =
            function
            | [] -> cont 0
            | x::xs -> aux (fun next -> x+10*next |> cont) xs
        aux id 
        
(* 3: Caesar Ciphers *)

(* Question 3.1 *)
    
    let alpha = ['a'..'z']
    let lenAlpha = List.length alpha
    let relOffset offset idx =
        alpha[(lenAlpha + offset + idx)%lenAlpha]
    let indexed =
        alpha |> List.indexed
    let offsetAlpha offset =
        ([],(alpha |> List.indexed))
        ||> List.fold (fun acc (idx,c) -> (c, (relOffset offset idx))::acc)
        |> Map.ofList
        |> Map.add ' ' ' '
    let encryptFromDict (msg: string) (dict: Map<char,char>) =
        ([], msg)
        ||> Seq.fold (fun acc c -> (Map.find c dict)::acc)
        |> List.rev
        |> List.toArray
        |> String.Concat
    let encrypt (msg: string) offset =
        let offsetAlpha = offsetAlpha offset
        encryptFromDict msg offsetAlpha
        
(* Question 3.2 *)
    let decrypt msg offset =
        let offsetAlpha =
            offsetAlpha offset
            |> Map.toList
            |> List.map (fun (a,b) -> b,a)
            |> Map.ofList
        encryptFromDict msg offsetAlpha
        
        
    
(* Question 3.3 *)
    let decode plain encrypted =
        (None, [0..lenAlpha])
        ||> Seq.fold (fun acc idx ->
            let tmp = encrypt plain idx
            if tmp = encrypted && Option.isNone acc then
                Some(idx)
            else
                acc)
        
        
    
(* Question 3.4 *)
    let parEncrypt (msg: string) offset =
        msg.Split(' ')
        |> Array.Parallel.map (fun msg' -> encrypt msg' offset)
        |> Array.fold (fun acc str -> acc+" "+str) ""
        |> Seq.tail
        |> String.Concat
    
(* Question 3.5 *)
        
    open JParsec.TextParser
    
    let valid =
        satisfy (fun c -> Char.IsLetter c && Char.IsLower c)
        <|>
        satisfy Char.IsWhiteSpace
    
    let parseEncrypt offset =
        many valid
        |>> (Seq.ofList >> String.Concat >> (fun s -> encrypt s offset))
    

(* 4: Letterboxes *)
    
(* Question 4.1 *)
    
    type letterbox = (string*string) list // Replace with your type
    
    let empty () : letterbox = List.empty

(* Question 4.2 *)

    let post sender message (mb:letterbox) : letterbox=
        (sender, message)::mb
    
    let read sender (mb: letterbox) : (string*letterbox) option =
        let msg, xs =
            ((None, []), mb |> List.rev)
            ||> List.fold (fun (found, acc) (sender',msg) ->
                if sender = sender' && Option.isNone found then
                    Some(msg), acc
                else
                    found, (sender',msg)::acc)
        match msg with
        | Some(msg) -> Some(msg, xs)
        | _ -> None

    
(* Question 4.3 *)
    type StateMonad<'a> = SM of (letterbox -> ('a * letterbox) option)  
      
    let ret x = SM (fun s -> Some (x, s))  
    let fail  = SM (fun _ -> None)  
    let bind f (SM a) : StateMonad<'b> =   
        SM (fun s ->   
            match a s with   
            | Some (x, s') ->  let (SM g) = f x               
                               g s'  
            | None -> None)
          
    let (>>=) x f = bind f x  
    let (>>>=) x y = x >>= (fun _ -> y)  
      
    let evalSM (SM f) = f (empty ())
    
    let post2 sender message =
        SM(fun mb -> Some((), post sender message mb))
    let read2 sender =
        SM(read sender) 

(* Question 4.4 *)

    type StateBuilder() =

        member this.Bind(f, x)    = bind x f
        member this.Return(x)     = ret x
        member this.ReturnFrom(x) = x
        member this.Combine(a, b) = a >>= (fun _ -> b)

    let state = StateBuilder()

    type MType =
        | Post of string * string
        | Read of string
    type log = MType list
    
    let trace (l: log) =
        let rec aux l acc =
            match l with
            | [] -> ret acc
            | Post(sender, message)::xs ->
                post2 sender message >>>= aux xs acc
            | Read(sender)::xs ->
                read2 sender >>= fun a ->
                aux xs (a::acc)
        aux l [] >>=
        fun a -> List.rev a
                 |> ret