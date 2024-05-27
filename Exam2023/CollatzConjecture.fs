module CollatzConjecture

    open Microsoft.FSharp.Collections
    
    let collatz =
        let rec inner acc x =
            if x = 0 then
                []
            else if x < 0 then
                failwith $"Non positive number: %A{x}"
            else if x = 1 then
                x::acc
            else
                let next =
                    if x % 2 = 0 then
                        x / 2
                    else
                        3*x+1
                inner (x::acc) next
        inner [] >> List.rev
        

    let evenOddCollatz =
        collatz >> List.fold (fun (even, odd) item ->
            if item % 2 = 0 then
                even+1, odd
            else
                even, odd+1) (0,0)
 
 
    let rec getlen acc x =
        if x = 0 then
            0
        else if x < 0 then
            failwith $"Non positive number: %A{x}"
        else if x = 1 then
            acc+1
        else
            let next =
                if x % 2 = 0 then
                    x / 2
                else
                    3*x+1
            getlen (acc+1) next
    let maxCollatz x y =
        let rec aux i acc m =
            if i >= y then
                m, getlen 0 m
            else
                let tmp = getlen 0 i
                let next, num =
                    if tmp >= acc then
                        i, tmp
                    else m, acc
                aux (i+1) num next
        aux (x+1) 0 x 
        
        
        
    let add key value m =
        let value =
            if Map.containsKey key m then
                Map.find key m |> Set.add value
            else Set.ofList [value]
        Map.add key value m
    let collect x y =
        let rec aux i acc =
            let curr = add (getlen 0 i) i acc
            if i >= y then
                curr
            else
                aux (i+1) curr
        aux x Map.empty
        
        
    let parallelMaxCollatz x y n =
        [x..(y-x)/n..y]
        |> Seq.pairwise
        |> Seq.toArray
        |> Array.Parallel.map (fun (curr, next) -> maxCollatz curr next)
        |> Array.maxBy snd
        |> fst
            