open System
open Exam2022
open Microsoft.FSharp.Core
open JParsec.TextParser

let testQ1 () =
    (* Testsfor Q1.1 *)
    printfn "Testing Question 1"
    // printfn "%A" (countWhite (Square 123uy))
    // printfn "%A" (countWhite img)
    // printfn "%A" (rotateRight (Square 123uy))
    // printfn "%A" (rotateRight (Quad(Square 0uy, Square 85uy, Square 170uy, Square 255uy)))
    // printfn "%A" (rotateRight img)
    // printfn "%A" (map (fun x -> Square(x+10uy)) (Square(0uy)))
    // printfn "%A" (map (fun x -> Square(x+10uy)) (Quad (Square 0uy, Square 85uy, Square 170uy, Square 255uy)))
    // printfn "%A" (map (fun x -> Quad (Square (x+10uy), Square (x+20uy), Square (x+30uy), Square (x+40uy))) (Square(123uy)))
    // printfn "%A" (bitmap (Square 120uy))
    // printfn "%A" (bitmap (Square 150uy))
    // printfn "%A" (bitmap img)
    // printfn "%A" (fold (fun acc x -> acc + int x) 0 (Square 123uy))
    // printfn "%A" (fold (fun acc x -> acc + int x) 0 (Quad (Square 0uy, Square 85uy, Square 170uy, Square 255uy)))
    // printfn "%A" (fold (fun acc x -> acc + int x) 0 img)
    // printfn "%A" (countWhite2 (Square 123uy))
    // printfn "%A" (countWhite2 img)
    ()

let testQ2 () =
    printfn "Testing Question 2"
    // printfn "%A" (bar2 [0..10] |> List.indexed |> List.map (fun (idx, item) -> (sprintf "%B" idx, idx, item)))
    // printfn "%A" (bar [0..1000000000])
    // printfn "%A" (fooTail 0)
    // printfn "%A" (fooTail 2)
    // printfn "%A" (fooTail 3)
    // printfn "%A" (fooTail 4)
    // printfn "%A" (barTail [0..10] |> List.indexed |> List.map (fun (idx, item) -> (sprintf "%B" idx, idx, item)))
    ()

let testQ3 () =
    printfn "Testing Question 3"
    // add (init (fun x y -> x + y) 2 3) (init (fun x y -> x * y) 2 3) |> print
    // add (init (fun x y -> x + y) 2 3) (init (fun x y -> x * y) 3 2)
    // printfn "%A" (dotProduct m1 m2 0 1)
    // printfn "%A" (dotProduct m1 m2 1 0)
    // mult m1 m2 |> print
    // mult m1 (init (fun _ _ -> 0) 1 9)
    // parInit (fun i j -> i * 3 + j + 1) 2 3 |> print
    ()

let testQ4 () =
    printfn "Testing Question 4"
    // printfn "%A" (runStackProgram [Push 5])
    // printfn "%A" (runStackProgram [Push 5; Push 4; Add; Push 8; Mult])
    // printfn "%A" (runStackProgram [Push 5; Push 4; Add; Push 8; Mult; Push 42; Add])
    // printfn "%A" (runStackProgram [Push 5; Push 4; Add; Push 8; Mult; Mult])
    // printfn "%A" (push 5 >>>= push 6 >>>= pop |> evalSM)
    // printfn "%A" (pop |> evalSM)
    // printfn "%A" ([Push 5] |> runStackProg2 |> evalSM |> Option.map fst)
    // printfn "%A" ([Push 5; Push 4; Add; Push 8; Mult] |> runStackProg2 |> evalSM |> Option.map fst)
    // printfn "%A" ([Push 5; Push 4; Add; Push 8; Mult; Push 42; Add] |> runStackProg2 |> evalSM |> Option.map fst)
    // printfn "%A" ([Push 5; Push 4; Add; Push 8; Mult; Mult] |> runStackProg2 |> evalSM |> Option.map fst)
    // printfn "%A" ([] |> runStackProg2 |> evalSM |> Option.map fst)
    let str = 
        "PUSH 5\nPUSH 4   \nADD  \n   PUSH   8\nMULT   \n" |>
        Seq.toArray
        |> Array.map (fun c -> if c = '\n' then 'x' else c)
        |> String.Concat
    
    run parseStackProg str |>
    getSuccess |>
    runStackProg2 |>
    evalSM |>
    Option.map fst |>
    (printfn "%A")
    // printfn
    // "      PUSH     5    xADDx" |>
    // run parseStackProg |>
    // (printfn "%A")
    // getSuccess |>
    // runStackProg2 |>
    // evalSM |>
    // Option.map fst |>
    // (printfn "%A")
    ()

[<EntryPoint>]
let main argv =
    // testQ1 ()
    // testQ3 ()
    testQ4()
    0 // return an integer exit code
