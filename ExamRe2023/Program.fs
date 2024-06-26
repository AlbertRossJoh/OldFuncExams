﻿open System
open ReExam2023
open JParsec.TextParser

let testQ1 () =
    (* Testsfor Q1.1 *)
    printfn "Testing Question 1"
    // printfn "%A" (eval p1)
    // printfn "%A" (eval p2)
    // printfn "%A" (eval p3)
    // printfn "%A" (negate p1)
    // printfn "%A" (negate p2)
    // printfn "%A" (negate p3)
    // printfn "%A" (subtract p1 p2)
    // printfn "%A" (subtract p2 p3)
    // printfn "%A" (subtract p3 p1)
    // printfn "%A" (subtract p3 p1 |> eval)
    // printfn "%A" (multiply p1 p1)
    // printfn "%A" (multiply p2 p2)
    // printfn "%A" (multiply p3 p3)
    // printfn "%A" (pow (arith.Num 2) (arith.Num 8))
    // printfn "%A" (pow p2 (arith.Num 3))
    // printfn "%A" (iterate negate p1 p2)
    // printfn "%A" (iterate (sprintf "f(%s)") "acc" p2)
    // printfn "%A" (iterate negate p1 (subtract p2 (arith.Num 1)))
    // printfn "%A" (pow2 (arith.Num 2) (arith.Num 8))
    // printfn "%A" (pow2 p2 (arith.Num 3) |> eval)
    ()
    

let testQ2 () =
    // printfn "Testing Question 2"
    // printfn "%A" (getEvenAndUneven [1..10])
    // printfn "%A" (bazTail [1..10])
    ()

let testQ3 () =
    printfn "Testing Question 3"
    // printfn "balanced: %A" (balanced "()")
    // printfn "balanced: %A" (balanced "(){([])}")
    // printfn "balanced: %A" (balanced "(){([])}(")
    // printfn "balanced: %A" (balanced ")(){([])}(")
    // printfn "%A" (balanced2 (Map.ofList ['a', 'b'; 'b', 'c']) "abcb")
    // printfn "%A" (balanced2 (Map.ofList ['a', 'b'; 'b', 'c']) "abacb")
    // printfn "balanced3: %A" (balanced3 "()")
    // printfn "balanced3: %A" (balanced3 "(){([])}")
    // printfn "balanced3: %A" (balanced3 "(){([])}(")
    // printfn "balanced3: %A" (balanced3 ")(){([])}(")
    // printfn "%A" (symmetric "")
    // printfn "%A" (symmetric "s")
    // printfn "%A" (symmetric "csc")
    // printfn "%A" (symmetric "aabbaa")
    // printfn "%A" (symmetric "Dromedaren Alpotto planerade mord!!!")
    // printfn "%A" (symmetric "Dromedaren Alpotto skadar ingen")
    // printfn "%A" (run parseBalanced "{([]())}{}**END**")
    // printfn "%A" (run parseBalanced "{([]())[]}{}**END**")
    // printfn "%A" (run parseBalanced ")(){([])}(**END**")
    // let lst = [for i in 1..10000 do
    //                yield! ["()"; "({})"; "()({[]})"; "("; "{{}"; "{(})"]]
    // printfn "%A" (countBalanced lst 1)
    // printfn "%A" (countBalanced lst 10)
    // printfn "%A" (countBalanced lst 100)
    // printfn "%A" (countBalanced lst 1000)
    ()

let testQ4 () =
    printfn "Testing Question 4"
    // printfn "%A" (mkBasicProgram [10u, End])
    let fp = 10 |> fibProg |> mkBasicProgram
    // printfn "%A" (getStmnt 50u fp)
    // printfn "%A" (nextLine 50u fp)
    // printfn "%A" (firstLine fp)
    // printfn "%A" (emptyState fp)
    // printfn "%A" (emptyState fp |> goto 50u)
    // printfn "%A" (emptyState fp |> getCurrentStmnt fp)
    // printfn "%A" (emptyState fp |> goto 50u |> getCurrentStmnt fp)
    // printfn "%A" (emptyState fp |> update "x" 42)
    // printfn "%A" (emptyState fp |> update "x" 42 |> lookup "x")
    let st = emptyState fp
    let st' = update "x" 42 st
    // printfn "%A" (evalExpr (Num 5) st)
    // printfn "%A" (evalExpr (Lookup "x") st')
    // printfn "%A" (evalExpr (Plus(Lookup "x", Num 5)) st')
    let smallProg = [(10u, Let("x", Num 42)); (20u, End)] |> mkBasicProgram
    // printfn "%A" (evalProg smallProg)
    // printfn "%A" (evalProg fp)
    // printfn "%A" (fp |>bp evalProg |> lookup "result")
    // printfn "%A" (goto2 50u |> evalSM fp)
    // printfn "%A" (goto2 50u >>>= getCurrentStmnt2 |> evalSM fp)
    // printfn "%A" (update2 "x" 42 |> evalSM fp)
    // printfn "%A" (update2 "x" 42 >>>= lookup2 "x" |> evalSM fp)
    // printfn "%A" (goto2 50u >>>= step2 |> evalSM fp)
    // printfn "%A" (Num 5 |> evalExpr2 |> evalSM fp)
    // printfn "%A" (update2 "x" 42 >>>= evalExpr2 (Lookup "x") |> evalSM fp)
    // printfn "%A" (update2 "x" 42 >>>= evalExpr2 (Plus (Lookup "x", Num 5)) |> evalSM fp)
    let smallProg = [(10u, Let ("x", Num 42)); (20u, End)] |> mkBasicProgram
    // printfn "%A" (evalProg2 |> evalSM smallProg)
    printfn "%A" (evalProg2 |> evalSM fp)
    printfn "%A" (evalProg2 >>>= lookup2 "result" |> evalSM fp)
    ()


open JParsec.TextParser

[<EntryPoint>]
let main argv =
    // testQ1 ()
  
    0 // return an integer exit code
