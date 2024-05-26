module Exam2022_2

    open System
    open Microsoft.FSharp.Collections
    open Microsoft.FSharp.Core

(* If you are importing this into F# interactive then comment out
   the line above and remove the comment for the line bellow.

   Do note that the project will not compile if you do this, but 
   it does allow you to work in interactive mode and you can just remove the '=' 
   to make the project compile again.

   Do not remove the module declaration (even though that does work) because you may inadvertently
   introduce indentation errors in your code that may be hard to find if you want
   to switch back to project mode. 

   Alternative, keep the module declaration as is, but load ExamInteractive.fsx into the interactive environment
   *)
(*
 module Exam2022_2 = 
 *)

(* 1: Grayscale images *)

    type grayscale =
    | Square of uint8
    | Quad of grayscale * grayscale * grayscale * grayscale
    
    let img = 
      Quad (Square 255uy, 
            Square 128uy, 
            Quad(Square 255uy, 
                 Square 128uy, 
                 Square 192uy,
                 Square 64uy),
            Square 0uy)
    
(* Question 1.1 *)
    let maxDepth =
        let rec inner depth img =
            match img with
            | Square _ -> depth
            | Quad(a, b, c, d) ->
                [a; b; c; d]
                |> List.map (inner depth >> (+) 1)
                |> List.max
        inner 0 
    
(* Question 1.2 *)
    let rec mirror img =
        match img with
        | Quad(a, b, c, d) -> Quad(mirror b, mirror a, mirror d, mirror c)
        | a -> a

(* Question 1.3 *)
    let rec operate f =
        function
        | Quad(a, b, c, d) ->
            f (operate f a) (operate f b) (operate f c) (operate f d)
        | v -> v
    
    let mirror2 =
        let m a b c d =
            match a, b, c, d with
            | _, _, _, _ -> Quad(b, a, d, c)
        operate m

(* Question 1.4 *)

    let rec compress img =
        match img with
        | Quad(Square a, Square b, Square c, Square d) ->
            if a = b && b = c && c = d then
                Square a
            else
                Quad(Square a, Square b, Square c, Square d)
        | Quad(a, b, c, d) ->
            Quad(compress a, compress b, compress c, compress d) |> compress
        | _ -> img
        

(* 2: Code Comprehension *)
    let rec foo f =
        function
        | []               -> []
        | x :: xs when f x -> x :: (foo f xs)
        | _ :: xs          -> foo f xs
            
    let rec bar fs xs =
        match fs with
        | []       -> xs
        | f :: fs' -> bar fs' (foo f xs)

(* Question 2.1 *)

    (* 
    
    Q: What are the types of functions foo and bar?

    A:  foo : ('a -> bool) -> 'a list -> 'a list
        bar : ('a -> bool) list -> 'a list -> 'a list

    Q: What do the functions foo and  bar do. 
       Focus on what it does rather than how it does it.

    A:  foo accumulates all elements in a list that fulfills a given function predicate
        bar creates accumulates a list that fulfills all predicates given in a list
    
    Q: What would be appropriate names for functions 
       foo and bar?

    A:  foo -> filter
        bar -> filterMultiple
        
    Q: The function foo uses an underscore `_` in its third case. 
       Is this good coding practice, if so why, and if not why not?
    
    A: In this case it is fine as we do not want to keep the element if it does not match our case
    *)
        

(* Question 2.2 *)

    let bar2 fs xs =
        (xs, fs) ||> List.fold (fun acc f -> List.filter f acc)

(* Question 2.3 *) 

    let baz fs item =
        (true, fs) ||> List.fold (fun acc f -> acc && (f item))

(* Question 2.4 *)

    (*

    Q: Only one of the functions `foo` and `bar` is tail recursive. Which one? 
       Demonstrate why the other one is not tail recursive.
       To make a compelling argument you should evaluate a function call of the function,
       similarly to what is done in Chapter 1.4 of HR, and reason about that evaluation.
       You need to make clear what aspects of the evaluation tell you that the function is not tail recursive.
       Keep in mind that all steps in an evaluation chain must evaluate to the same value
       ((5 + 4) * 3 --> 9 * 3 --> 27, for instance).

    A: bar is tail recursive since the last call is made to bar
        foo is not since it does the cons operation : x :: (foo f xs)

    *)
(* Question 2.5 *)
    // only implement the one that is NOT already tail recursive
    let fooTail f lst =
        let rec inner cont lst =
            match lst with
            | x::xs -> inner (fun next -> cont (if f x then x::next else next)) xs
            | _ -> cont lst
        inner id lst
    let barTail _ = failwith "not implemented"


(* 3: Guess a number *)

    type guessResult = Lower | Higher | Equal
    type oracle =
        { max : int
          f : int -> guessResult }

(* Question 3.1 *)

    let validOracle (o: oracle) =
        let aux toMatch x =
            o.f x = toMatch
        let allInRange = [1..o.max]
        let equalTest = allInRange |> List.filter (aux Equal)
        if not (equalTest |> List.length = 1) then
            false
        else
            let numberChoice = equalTest[0]
            let below = [1..numberChoice-1]
            let above = [numberChoice+1..o.max]
            match below, above with
            | [], [] -> true
            | [], _ ->
                above |> List.filter (aux Lower) = above
            | _, [] -> below |> List.filter (aux Higher) = below
            | _, _ ->
                let greaterTest = below |> List.filter (aux Higher) = below
                let lowerTest = above |> List.filter (aux Lower) = above
                greaterTest && lowerTest

(* Question 3.2 *)

    let randomOracle m oseed =
        let num =
            match oseed with
            | None -> Random().Next(1, m+1)
            | Some(e) -> Random(e).Next(1, m+1)
        { max = m; f = (fun x -> if x = num then Equal else if x < num then Higher else Lower) }

(* Question 3.3 *)
    
    let findNumber o =
        let rec inner acc a b =
            let guess = ((a+b)/2)
            match o.f guess with
            | Higher -> inner (guess::acc) (guess+1) b
            | Lower -> inner (guess::acc) a (guess-1)
            | _ -> guess::acc
        inner [] 1 o.max |> List.rev

(* Question 3.4 *)
    let evilOracle m oseed =
        let mutable choices = [1..m]
        {
            max = m
            f = (fun choice ->
                if List.length choices = 1 then
                    choices <- [1..m]
                    Equal
                else
                    let choiceIdx = List.findIndex ((=) choice) choices
                    let lowerPart,upperPart =  List.splitAt choiceIdx choices
                    let lenLower, lenUpper = List.length lowerPart, List.length upperPart
                    if lenLower < lenUpper then
                        choices <- choices[choiceIdx+1..List.length choices-1]
                        Higher
                    else if lenLower > lenUpper then
                        choices <- choices[1..choiceIdx-1]
                        Lower
                    else
                        let randomNum = 
                            match oseed with
                            | None -> Random().Next(0,1)
                            | Some(v) -> Random(v).Next(0,1)
                        if randomNum = 1 then
                            choices <- choices[1..choiceIdx-1]
                            Lower 
                        else
                            choices <- choices[choiceIdx+1..List.length choices-1]
                            Higher
                )
        }
    
(* Question 3.5 *)
    let parFindNumber ol =
        ol
        |> Array.ofList
        |> Array.Parallel.map findNumber
        |> List.ofArray

(* 4: Assembly *)

    type register = R1 | R2 | R3
    type address = uint

    type assembly =
    | MOVI of register * int
    | MULT of register * register * register
    | SUB of register * register * register
    | JGTZ of register * address
    
     
    let factorial x =           // Address
        [MOVI (R1, 1)           // 0
         MOVI (R2, x)           // 1
         MOVI (R3, 1)           // 2
         MULT (R1, R1, R2)      // 3 (Loop starts here)
         SUB  (R2, R2, R3)      // 4
         JGTZ (R2, 3u)]         // 5 (Loop ends here)
    
(* Question 4.1 *)

    type program = unit (* replace this entire type with your own *)
    let assemblyToProgram _ = failwith "not implemented"

(* Question 4.2 *)

    type state = unit (* replace this entire type with your own *)
    let emptyState _ = failwith "not implemented"
    

(* Question 4.3 *)

    let setRegister _ = failwith "not implemented"
    
    let getRegister _ = failwith "not implemented"
    
    let setProgramCounter _ = failwith "not implemented"
    
    let getProgramCounter _ = failwith "not implemented"
    
    let getProgram _ = failwith "not implemented"
    
(* Question 4.4 *)
    
    type StateMonad<'a> = SM of (state -> 'a * state)

    let ret x = SM (fun s -> x, s)
    let bind f (SM a) : StateMonad<'b> = 
      SM (fun s -> 
      let x, s' = a s
      let (SM g) = f x
      g s')

    let (>>=) x f = bind f x
    let (>>>=) x y = x >>= (fun _ -> y)

    let evalSM prog (SM f) = f (emptyState prog)

    let setReg _ = failwith "not implemented"
    
    let getReg _ = failwith "not implemented"
    
    let setPC _ = failwith "not implemented"
    
    let incPC _ = failwith "not implemented"
    
    let lookupCmd _ = failwith "not implemented"
    


(* Question 4.5 *)

    type StateBuilder() =

        member this.Bind(f, x)    = bind x f
        member this.Return(x)     = ret x
        member this.ReturnFrom(x) = x
        member this.Combine(a, b) = a >>= (fun _ -> b)

    let state = new StateBuilder()

    let runProgram _ = failwith "not implemented"
    