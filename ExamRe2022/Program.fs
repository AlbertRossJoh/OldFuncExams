open System
open Exam2022_2
open Microsoft.FSharp.Core

let testQ1 () =
    (* Testsfor Q1.1 *)
    printfn "Testing Question 1"
    // printfn "%A" (maxDepth (Square 123uy))
    // printfn "%A" (maxDepth img)

    // printfn "%A" (mirror (Square 123uy))
    // printfn "%A" (mirror img)
    // let average a b c d =
    //     match a, b, c, d with
    //     | Square a, Square b, Square c, Square d ->
    //         Square(((float a + float b + float c + float d) / 4.0) |> uint8)
    //     | _,_,_,_ -> Quad(a, b, c, d)
    // printfn "%A" (operate average img)
    // printfn "%A" (mirror2 (Square 123uy))
    // printfn "%A" (mirror2 img)
    // printfn "%A" (compress (Square 123uy))
    // printfn "%A" (compress (Quad (Square 123uy, Square 123uy, Square 123uy, Square 123uy)))
    // printfn "%A" (compress (Quad (Square 123uy, Square 123uy, Square 123uy, Square 0uy)))
    // printfn "%A" (compress (Quad (Square 123uy, Square 123uy, Quad (Square 123uy, Square 123uy, Square 123uy, Square 123uy), Square 123uy)))
    
    ()

let testQ2 () =
    printfn "Testing Question 2"
    // printfn "%A" (bar [(fun x -> (%) x 2 = 0);(fun x -> x <> 10);] [0..10])
    // printfn "%A" (bar2 [(fun x -> (%) x 2 = 0);(fun x -> x <> 10)] [0..10])
    // printfn "%A" (foo (baz [(fun x -> (%) x 2 = 0);(fun x -> x <> 10)]) [0..10])
    // printfn "%A" (fooTail (baz [(fun x -> (%) x 2 = 0);(fun x -> x <> 10)]) [0..10])
    ()

let testQ3 () =
    printfn "Testing Question 3"
    // printfn "%A" (validOracle {max = 10; f = fun _ -> Lower })
    // printfn "%A" (validOracle { max = 10; f = fun x -> if x = 5 then Equal else if x < 5 then Higher else Lower })
    let o = randomOracle 10 (Some 42)
    // printfn "%A" (o.f 5)
    // printfn "%A" (o.f 8)
    // printfn "%A" (o.f 7)
    // printfn "%A" (validOracle o)
    // printfn "%A" (findNumber o)
    // printfn "%A" (randomOracle 20 (Some 42) |> findNumber)
    // printfn "%A" (randomOracle 1000000 (Some 42) |> findNumber)
    // printfn "%A" (findNumber (evilOracle 10 (Some 42)))
    // printfn "%A" (findNumber (evilOracle 20 (Some 42)))
    // printfn "%A" (findNumber (evilOracle 1000000 (Some 42)))
    // printfn "%A" (validOracle (evilOracle 10 (Some 42)))
    // printfn "%A" (parFindNumber [o; randomOracle 20 (Some 42); randomOracle 1000000 (Some 42)])
    ()

let testQ4 () =
    printfn "Testing Question 4"
    // printfn "%A" (assemblyToProgram (factorial 10))
    // printfn "%A" (factorial 10 |> emptyState |> getRegister R1)
    // printfn "%A" (factorial 10 |> emptyState |> setRegister R1 10 |> getRegister R1)
    // printfn "%A" (factorial 10 |> emptyState |> setProgramCounter 100u |> getProgramCounter)
    // printfn "%A" (getReg R1 |> evalSM (factorial 10) |> fst)
    // printfn "%A" (setReg R1 10 >>>= getReg R1 |> evalSM (factorial 10) |> fst)
    // printfn "%A" (lookupCmd |> evalSM (factorial 10) |> fst)
    // printfn "%A" (incPC >>>= lookupCmd |> evalSM (factorial 10) |> fst)
    // printfn "%A" (incPC >>>= incPC >>>=incPC >>>=lookupCmd |> evalSM (factorial 10) |> fst)
    // printfn "%A" (setPC 100u >>>= lookupCmd |> evalSM (factorial 10) |> fst)
    printfn "%A" (runProgram () >>>= getReg R1 |> evalSM (factorial 5) |> fst)
    printfn "%A" (runProgram () >>>= getReg R1 |> evalSM (factorial 10) |> fst)
    ()

[<EntryPoint>]
let main argv =
    // testQ1 ()
    // testQ3()
    testQ4()
    0 // return an integer exit code
