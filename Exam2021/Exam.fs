module Exam2021
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
 module Exam2021 = 
 *)

(* 1: Dungeon crawler *)

(* Question 1.1 *)

    type direction = North | East | South | West
    type coord     = C of int * int

    let move dist dir (C(x, y) : coord) =
        match dir with
        | North -> C(x,y-dist)
        | South -> C(x,y+dist)
        | West -> C(x-dist,y)
        | East -> C(x+dist,y)
    let turnRight dir =
        match dir with
        | North -> East
        | East -> South
        | South -> West
        | West -> North
        
    let turnLeft dir =
        match dir with
        | North -> West
        | East -> North
        | South -> East
        | West -> South
                
    
(* Question 1.2 *)

    type position = P of (coord * direction)
    type move     = TurnLeft | TurnRight | Forward of int

    let step (P(pos, dir)) m =
        match m with
        | TurnLeft -> P(pos, turnLeft dir)
        | TurnRight -> P(pos, turnRight dir)
        | Forward x -> P(move x dir pos, dir)
        

(* Question 1.3 *)


    let rec walk pos =
        function
        | [] -> pos
        | x::xs -> walk (step pos x) xs

    let rec walk2 pos mlst =
        (pos, mlst) ||> List.fold step

(* Question 1.4 *)

    let path pos mlst =
        let rec inner rest pos =
            match rest with
            | [] -> []
            | x::xs ->
                let pos2 = step pos x
                match x with
                | Forward _ -> pos2::inner xs pos2
                | _ -> inner xs pos2
        pos::inner mlst pos |> List.map (fun (P(c, _)) -> c)

(* Question 1.5 *)

    let path2 pos mlst =
        let rec inner rest (P(pos,dir)) acc =
            match rest with
            | [] -> pos::acc
            | x::xs ->
                let pos2 = step (P(pos, dir)) x
                match x with
                | Forward _ -> inner xs pos2 (pos::acc)
                | _ -> inner xs pos2 acc
        inner mlst pos [] |> List.rev
(* Question 1.6 *)

(* Q: Your solution for `path` is not tail recursive. Why? To make a compelling
      argument you should evaluate a function call of the function, similarly to
      what is done in Chapter 1.4 of HR, and reason about that evaluation.
      You need to make clear what aspects of the evaluation tell you that the
      function is not tail recursive. Keep in mind that all steps in an evaluation
      chain must evaluate to the same value
      (```(5 + 4) * 3 --> 9 * 3 --> 27```, for instance).

   A: the last function call to inner is not always inner it is the cons operation
*)

    let path3 pos mlst =
        let rec inner rest (P(pos,dir)) cont =
            match rest with
            | x::xs ->
                let pos2 = step (P(pos, dir)) x
                match x with
                | Forward _ -> inner xs pos2 (fun next -> cont (pos::next))
                | _ -> inner xs pos2 cont
            | [] -> cont [pos]
        inner mlst pos id

(* 2: Code Comprehension *)
    let foo bar =
        let mutable m = Map.empty
        let aux x =
            match Map.tryFind x m with
            | Some y when Map.containsKey x m -> y
            | None   ->
            m <- Map.add x (bar x) m; bar x

        aux

    let rec bar x =
      match x with 
      | 0 -> 0 
      | 1 -> 1
      | y -> baz (y - 1) + baz (y - 2)

    and baz = foo bar

(* Question 2.1 *)

    (* 
    
    Q: What are the types of functions foo, bar, and baz?

    A:  foo : (int -> int) -> int -> int
        bar : int -> int
        baz : int -> int


    Q: What do functions foo and baz do (skip bar)?
       Focus on what they do rather than how they do it.

    A:  foo gets a number if it has been calculated (memoization)
        baz get the memoized number

    The function foo uses a mutable variable.

    Q: What function does it serve (why is it there)?

    A:  Since the fibonacci sequence is a tree like structure the repeating parts of the fibonacci sequence would appear at different times
        it will not be possibles for these lower level of recursion to get the same value if the map was not mutable
        
                     5
                3          2
             2     1      1   1
            1  1  1  0  1  0
          1  0
    Q: What would happen if you removed the mutable keyword from the line
       let mutable m = Map.empty? Would the function foo still work?
       If yes, why; if no, why not?

    A:  no were mutating it  with "m <- Map.add x (bar x) m; bar x" we would need to change how the function "foo" passes on state if we remove it
        though if we changed it to not be mutable we might as well not have it for the reasons explained above

    Q: What would be appropriate names for functions 
       foo, bar, and baz?

    A:  foo -> memoize
        bar -> recurse
        baz -> fibonacci
    
    *)
        

(* Question 2.2 *)

 
    (* 
    The code includes the keyword "and".

    
    Q: What function does this keyword serve in general
       (why would you use "and" when writing any program)?

    A: the keyword and it utilized for mutual recursion, this is done since bar has to call baz and baz has to call bar


    Q: What would happen if you removed it from this particular program and
       replaced it with a standard "let"
       (change the line "and baz = foo bar" to "let baz = foo bar")?

    A: it would not compile

    *)

(* Question 2.3 *) 

    (* 
    The function foo generates a warning during compilation:
    "Warning: Incomplete pattern matches on this expression.".

    Q: Why does this happen, and where? 

    A: it happens when matching the "tryfind" since there is a conditional in the expression 
    the compiler thinks that there are some cases that are possibly not covered but in this case it is not a problem


    Q: For these particular three functions will this incomplete pattern match
       ever cause problems for any possible execution of baz? If yes, why;
       if no, why not.

    A: no if it matches with some it will already have found a key and thus the containskey is redundant

    Q: The function foo has two redundant computations and is hence not as
       efficient as it could be. What are these two computations and why
       are they redundant?

    A: as explained above the containsKey is redundant, the second computation is the "bar x" which is done twice this can be reduced to a single function call

    *)

    let foo2 _ = failwith "not implemented"

(* Question 2.4 *)

    let rec barbaz x =
        let baz = foo barbaz
        match x with 
        | 0 -> 0 
        | 1 -> 1
        | y -> baz (y - 1) + baz (y - 2)

    (*

    Q: Without explicitly timing the execution times, compare the execution
       times of baz and barbaz. One is slower than the other.
       Why? You do not have to give exact times, just spot which one is
       slower and explain why.

    A: <Your answer goes here>

    *)
(* Question 2.5 *)

    let bazSeq _ = failwith "not implemented"

(* 3: Guess the next sequence element *)

(* Question 3.1 *)

    type element = unit (* Your type goes here in stead of unit *)

(* Question 3.2 *)

    let elToString _ = failwith "not implemented"
    let elFromString _ = failwith "not implemented"

(* Question 3.3 *)

    let nextElement _ = failwith "not implemented"

(* Question 3.4 *)

    let elSeq _ = failwith "not implemented"
    let elSeq2 _ = failwith "not implemented"

    (*

    Q: Why would Seq.initInfinite not be an appropriate choice to
       write a function like elSeq?

    A: <Your answer goes here>

    *)

(* Question 3.5 *)

    let compress _ = failwith "not implemented"

(* Question 3.6 *)

    let elParse _ = failwith "not implemented"
    let elFromString2 _ = failwith "not implemented"

(* 4: Rings *)

(* Question 4.1 *)

    type 'a ring = RemoveThisConstructor of 'a (* replace this entire type with your own *)

(* Question 4.2 *)

    let length _ = failwith "not implemented"
    let ringFromList _ = failwith "not implemented"
    let ringToList _ = failwith "not implemented"

(* Question 4.3 *)

    let empty _ = failwith "not implemented"
    let push _ = failwith "not implemented"
    let peek _ = failwith "not implemented"
    let pop _ = failwith "not implemented"
    let cw _ = failwith "not implemented"
    let ccw _ = failwith "not implemented"

(* Question 4.4 *)

    type StateMonad<'a, 'b> = SM of ('b ring -> ('a * 'b ring) option)
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

    let smLength _ = failwith "not implemented"
    let smPush _ = failwith "not implemented"
    let smPop _ = failwith "not implemented"
    let smCW _ = failwith "not implemented"
    let smCCW _ = failwith "not implemented"

(* Question 4.4 *)

    (* You may solve this exercise either using monadic operators or 
        using computational expressions. *)

    type StateBuilder() =

        member this.Bind(x, f)    = bind x f
        member this.Zero ()       = ret ()
        member this.Return(x)     = ret x
        member this.ReturnFrom(x) = x
        member this.Combine(a, b) = a >>= (fun _ -> b)

    let state = new StateBuilder()

    let ringStep _ = failwith "not implemented"
    let iterRemoveSumEven _ = failwith "not implemented"