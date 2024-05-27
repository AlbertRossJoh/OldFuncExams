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
    ()

let testQ3 () =
    printfn "Testing Question 3"
    // place debug prints for Q3 here
    ()

let testQ4 () =
    printfn "Testing Question 4"
    // place debug prints for Q4 here
    ()

[<EntryPoint>]
let main argv =
    testQ1 ()
    0 // return an integer exit code
