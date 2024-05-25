// For more information see https://aka.ms/fsharp-console-apps
open System.Linq
open Logic
open CodeComprehension
open CollatzConjecture
open MemoryMachine
open Microsoft.FSharp.Core
open FParsec
// test negate
//
// let res = [0..10] |> forall mod2 |> eval
// printfn "%A" res
// let res2 = [0..2..10] |> forall mod2 |> eval
// printfn "%A" res2
// let res3 = [0..10] |> exists mod2 |> eval
// printfn "%A" res3
// let res4 = [1..2..10] |> exists mod2 |> eval
// printfn "%A" res4
// let res5 = [1..10] |> existsOne mod2 |> eval
// printfn "%A" res5
// let res6 = [1..10] |> existsOne mod5 |> eval
// printfn "%A" res6
// let res7 = [1..10] |> existsOne mod6 |> eval
// printfn "%A" res7
//
// printfn "%A" (baz "Hej med dig" "Hej med")
// printfn "%A" (collatz 1)
// printfn "%A" (collatz 2)
// printfn "%A" (collatz 3)
// printfn "%A" (collatz 42)
// printfn "%A" (evenOddCollatz 1)
// printfn "%A" (evenOddCollatz 2)
// printfn "%A" (evenOddCollatz 3)
// printfn "%A" (evenOddCollatz 77031)
// printfn "%A" (maxCollatz 1 10)
// printfn "%A" (maxCollatz 100 1000)
// printfn "%A" (maxCollatz 1000 1000)

// printfn "%A" (collect 20 30)
// printfn "%A" (collect 100 110)
printfn "%A" (parallelMaxCollatz 1 1000 1)
printfn "%A" (parallelMaxCollatz 1 1000 2)
printfn "%A" (parallelMaxCollatz 1 1000 100)
printfn "%A" (parallelMaxCollatz 1 1000 500)

// let m = (assign (emptyMem 5) 2 42)
// printfn "%A" m
// printfn "%A" (lookup m 4)
// printfn "%A" (assign m 2 42)
// printfn "%A" (evalExpr m (Num 5))
// printfn "%A" (evalExpr m (Lookup (Num 2)))
//
// printfn "%A" (evalExpr m (Plus (Lookup (Num 2), Num 5)))
// printfn "%A" (lookup (evalProg (emptyMem 4) (fibProg 10)) 2)

// let m = emptyMem 5
// printfn "%A" (lookup2 2 |> evalSM m |> Option.map fst)
// printfn "%A" (lookup2 -23 |> evalSM m |> Option.map fst)
// printfn "%A" (assign2 2 42 >>>= lookup2 2 |> evalSM m |> Option.map fst)
// printfn "%A" (assign2 2 42 >>>= lookup2 4 |> evalSM m |> Option.map fst)
// printfn "%A" (assign2 2 42 |> evalSM m |> Option.map snd)

let m = emptyMem 4
let m2 =  assign (emptyMem 5) 2 42
// printfn "%A" (Num 5 |> evalExpr2 |> evalSM m |> Option.map fst)
// printfn "%A" (Lookup(Num 2) |> evalExpr2 |> evalSM m2 |> Option.map fst)
// printfn "%A" (Plus(Lookup(Num 2), Num 5) |> evalExpr2 |> evalSM m2 |> Option.map fst)
// printfn "%A" (Assign(Num 4, Num 5) |> evalStmnt2 |> evalSM m2 |> Option.map snd)
// printfn "%A" (evalProg2 (fibProg 10) |> evalSM m |> Option.map snd)
// printfn "%A" (evalProg2 (fibProg 10) >>>= lookup2 2 |> evalSM m |> Option.map fst)

printfn "%A" ("5+4" |> run parseExpr)
// printfn "%A" ("[4+[3]]" |> run parseExpr)
// printfn "%A" ("[5 + 4]" |> run parseExpr)
