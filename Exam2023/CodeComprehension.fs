module CodeComprehension


    // Checks if ys is a substring of xs
    // xs = hej med dig
    // ys = hej med
    // dig
    let rec foo (xs: char list) (ys: char list) : char list option =
        match xs, ys with
        | _,[] -> Some xs
        | x :: xs', y :: ys' when x = y -> foo xs' ys'
        | _ , _ -> None
    
    
    
    let rec foo2 (xs: char list) (ys: char list) : char list option =
        let ylen = List.length ys
        let xlen = List.length xs
        if xlen < ylen then
            None
        else
            let idx, _ = ((0, false), List.indexed ys) ||> List.fold (fun (acc, switch) (idx, c) ->
                if idx < ylen && not switch then
                    if c = xs[idx] then
                        idx, false
                    else
                        acc, true
                else
                    acc, switch)
            if idx = 0 || idx + 1 >= xlen then
                None
            else
                let idx = idx+1
                List.splitAt idx xs |> snd |>Some
        

    // I hate this
    // returns the part of xs that is not in ys
    let rec bar (xs: char list) (ys: char list) : char list =
        match foo2 xs ys with
        | Some zs -> bar zs ys
        | None -> xs
            // match xs with
            // | [] -> []
            // | x::xs' -> x::(bar xs' ys)
    
    // let barTail (xs: char list) (ys: char list) : char list =
        
    // Finds a substring and returns it
    let baz (a: string) (b: string) =
        bar [for c in a -> c] [for c in b -> c] |>
        List.fold (fun acc c -> acc + string c) ""

