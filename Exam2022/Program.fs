open System
open Exam2022

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
    // place debug prints for Q4 here
    ()

[<EntryPoint>]
let main argv =
    // testQ1 ()
    testQ3 ()
    0 // return an integer exit code
