open System
open Exam2024
open JParsec.TextParser
let testQ1 () =
    (* Testsfor Q1.1 *)
    printfn "Testing Question 1"
    
    printfn "%A" (balance Empty)
    printfn "%A" (balance (Pay("Alice", 500, Receive("Bob", 200, Empty))))
    printfn "%A" (balanceAcc Empty)
    printfn "%A" (balanceAcc (Pay("Alice", 500, Receive("Bob", 200, Empty))))
    printfn "%A" (participants (Pay("Alice", 500, Receive("Bob", 200, Empty))))
    let balance' trs =
        balanceFold (fun acc _ x -> acc - x)
                    (fun acc _ x -> acc + x)
                    0
                    trs
    printfn "%A" (balance' Empty)
    printfn "%A" (balance' (Pay("Alice", 500, Receive("Bob", 200, Empty))))
    printfn "%A" (collect Empty)
    printfn "%A" (collect (Pay("Alice", 500, Receive("Bob", 200, Empty))))
    printfn "%A" (collect (Pay("Bob", 100, Pay("Alice", 500, Receive("Bob", 200, Empty)))))
    ()

let testQ2 () =
    printfn "Testing Question 2"
    // printfn "%A" (baz [1..9])
    // printfn "%A" ("1301" |> bar |> List.map foo |> List.rev |> baz)
    printfn "%A" (baz [1;0;0])
    printfn "%A" (baz2 [0;0;1])
    printfn "%A" (stringToInt "0")
    printfn "%A" (stringToInt "987655443")
    printfn "%A" (bazTail [0;0;1])
    printfn "%A" (bazTail [3;4;4;5;5;6;7;8;9])
    ()

let testQ3 () =
    printfn "Testing Question 3"
    printfn "%A" (encrypt "hello world" 0 |> fun s -> decrypt s 0)
    printfn "%A" (encrypt "hello world" 3|> fun s -> decrypt s 3)
    printfn "%A" (encrypt "hello world" 127|> fun s -> decrypt s 127)
    printfn "%A" (decode "hello world" "hello world")
    printfn "%A" (decode "hello world" "hello mom")
    printfn "%A" (decode "hello world" "khoor zruog")
    printfn "%A" (decode "hello world" "ebiil tloia")
    
    printfn "%A" (parEncrypt "hello world" 0|> fun s -> decrypt s 0)
    printfn "%A" (parEncrypt "hello world" 3|> fun s -> decrypt s 3)
    printfn "%A" (parEncrypt "hello world" 127|> fun s -> decrypt s 127)
    printfn "%A" (parEncrypt "" 127|> fun s -> decrypt s 127)
    printfn "%A" (run (parseEncrypt 0) "hello world")
    printfn "%A" (run (parseEncrypt 3) "hello world")
    printfn "%A" (run (parseEncrypt 127) "hello world")
    printfn "%A" (run (parseEncrypt 127) "hello World")
    ()

let testQ4 () =
    printfn "Testing Question 4"
    printfn "%A" (empty () |>
                post "Alice" "Hello John!" |>
                post "Bob" "John, how are you?" |>
                post "Alice" "Good bye John." |>
                read "Alice" |>
                Option.map fst)
    printfn "%A" (empty () |>
                post "Alice" "Hello John!" |>
                post "Bob" "John, how are you?" |>
                post "Alice" "Good bye John." |>
                read "Charlie" |>
                Option.map fst)
    printfn "%A" (post2 "Alice" "Hello John!" >>>= post2 "Bob" "John, how are you?" >>>=
                    post2 "Alice" "Good bye John." >>>= read2 "Alice" |> evalSM |> Option.map fst)
    printfn "%A" (post2 "Alice" "Hello John!" >>>= post2 "Bob" "John, how are you?" >>>=
                    post2 "Alice" "Good bye John." >>>= read2 "Charlie" |> evalSM |> Option.map fst)
    printfn "%A" ([Post("Alice", "Hello John!"); Post("Bob", "John, how are you?");
                    Post("Alice", "Good bye John.");
                    Read("Bob"); Read("Alice"); Read("Alice")] |>
                    trace |> evalSM |> Option.map fst)
    printfn "%A" ([Post("Alice", "Hello John!"); Post("Bob", "John, how are you?");
                    Post("Alice", "Good bye John.");
                    Read("Charlie"); Read("Bob"); Read("Alice");  Read("Alice")] |>
                    trace |>  evalSM |> Option.map fst)
    ()

[<EntryPoint>]
let main argv =
    testQ1 ()
    testQ2 ()
    testQ3 ()
    testQ4 ()
    0 // return an integer exit code
