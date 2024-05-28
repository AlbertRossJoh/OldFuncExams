open System
open Exam2021_2

let testQ1 () =
    (* Testsfor Q1.1 *)
    printfn "Testing Question 1"
    // printfn "%A" (length Nil)
    // printfn "%A" (length (Cons1 (3, Cons2 (true, Cons1 (4, Cons2 (false, Nil))))))
    // printfn "%A" (split Nil)
    // printfn "%A" (split (Cons1 (3, Cons2 (true, Cons1 (4, Cons2 (false, Cons2(true, Nil)))))))
    // printfn "%A" (length2 Nil)
    // printfn "%A" (length2 (Cons1 (3, Cons2 (true, Cons1 (4, Cons2 (false, Cons2(true, Nil)))))))
    // printfn "%A" (map (fun x -> x % 2 = 0)
    // (function | true -> 0 | false -> 1)
    // (Nil : binList<int, bool>))
    // printfn "%A" (map (fun x -> x % 2 = 0)
    // (function | true -> 0 | false -> 1)
    // (Cons1 (3, Cons2 (true, Cons1 (4, Cons2 (false, Cons2(true, Nil)))))))
    // printfn "%A" (filter (fun x -> x % 2 = 0)
    //    id
    //    (Nil : binList<int, bool>))
    // printfn "%A" (filter (fun x -> x % 2 = 0)
    //    id
    //    (Cons1 (3, Cons2 (true, Cons1 (4, Cons2 (false, Cons2(true, Nil)))))))
    printfn "%A" (fold (+)
     (fun acc -> function | true -> acc | false -> -acc)
     0
     (Nil : binList<int, bool>))
    printfn "%A" (fold (+)
     (fun acc -> function | true -> acc | false -> -acc)
     0
     (Cons1 (3, Cons2 (true, Cons1 (4, Cons2 (false, Cons2(true, Nil)))))))

    ()

let testQ2 () =
    printfn "Testing Question 2"
    // place debug prints for Q2 here
    printfn "%A" (barTail ([10; 4; 3; 5; 0; 2] |> List.rev))
    ()

let testQ3 () =
    printfn "Testing Question 3"
    // printfn "%A" (approxSquare 5 0)
    // printfn "%A" (approxSquare 5 1)
    // printfn "%A" (approxSquare 5 2)
    // printfn "%A" (approxSquare 5 3)
    // printfn "%A" (approxSquare 5 4)
    // printfn "%A" (quadratic 5 -4 -1 1)
    // printfn "%A" (quadratic 5 -3 -1 1)
    // printfn "%A" (quadratic 5 -3 -1 2)
    // printfn "%A" (quadratic 5 -3 -1 3)
    // printfn "%A" ([1..10] |> List.map (fun x -> (x, -(x + 1), -(x + 2))) |> fun eqs -> parQuadratic eqs 3 5)
    // printfn "%A" (solveQuadratic "-4x^2 - 5x + 6 = 0" 5)
    // printfn "%A" (solveQuadratic "-4x^2    -  5x+ 6=    0" 5)
    // printfn "%A" (solveQuadratic "-4x^2-5x+6=0" 5)
    // printfn "%A" (solveQuadratic "-4x^3 - 5x + 6 = 0" 5)
    // printfn "%A" (solveQuadratic "-4x^2 - 5x + 6 = 0 Hello World" 5)
    ()

let testQ4 () =
    printfn "Testing Question 4"
    // printfn "%A" (mkRat 5 6 |> Option.get |> ratToString)
    // printfn "%A" (mkRat 15 10 |> Option.get |> ratToString)
    // printfn "%A" (mkRat -15 10 |> Option.get |> ratToString)
    // printfn "%A" (mkRat 15 -10 |> Option.get |> ratToString)
    // printfn "%A" (mkRat -15 -10 |> Option.get |> ratToString)
    // printfn "%A" (mkRat 0 5 |> Option.get |> ratToString)
    // printfn "%A" (mkRat 5 0)
    
    let r1 = mkRat 2 3 |> Option.get
    let r2 = mkRat 3 4 |> Option.get
    let r3 = mkRat 4 5 |> Option.get
    let r4 = mkRat 5 6 |> Option.get
    let r5 = mkRat 6 7 |> Option.get
    // printfn "%A" (plus r1 r2 |> Option.get |> ratToString)
    // printfn "%A" (minus r1 r2 |> Option.get |> ratToString)
    // printfn "%A" (minus r2 r2 |> Option.get |> ratToString)
    // printfn "%A" (mult r1 r2 |> Option.get |> ratToString)
    // printfn "%A" (div r1 r2 |> Option.get |> ratToString)
    // printfn "%A" (div r1 (minus r2 r2 |> Option.get))
    // printfn "%A" (r1 |> evalSM (smPlus r2) |> Option.get |> snd |> ratToString)
    // printfn "%A" (r1 |> evalSM (smMinus r2) |> Option.get |> snd |> ratToString)
    // printfn "%A" (r1 |> evalSM (smMult r2) |> Option.get |> snd |> ratToString)
    // printfn "%A" (r1 |> evalSM (smDiv r2) |> Option.get |> snd |> ratToString)
    printfn "%A" (evalSM (calculate [(r2, smPlus); (r3, smMinus); (r4, smMult); (r5, smDiv)]) r1 |> Option.get |> snd |> ratToString)
    ()

[<EntryPoint>]
let main argv =
    // testQ1 ()
    // testQ2 ()
    // testQ3 ()
    testQ4 ()
    0 // return an integer exit code
