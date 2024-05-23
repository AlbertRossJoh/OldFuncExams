module Logic
    open LogicScaffold
    let rec eval prop =
        match prop with
        | TT -> true
        | FF -> false
        | And(prop, prop1) -> eval prop && eval prop1
        | Or(prop, prop1) -> eval prop || eval prop1
         
         
    let rec negate prop =
        match prop with
        | FF -> TT
        | TT -> FF
        | And(prop, prop1) -> Or(negate prop, negate prop1)
        | Or(prop, prop1) -> And(negate prop, negate prop1)

    let rec forall f lst =
        match lst with
        | [] -> TT
        | [x] -> f x
        | x::xs -> And(f x, forall f xs)
    
    let exists f lst =
        match lst with
        | [] -> FF
        | [x] -> f x
        | x::xs -> Or(f x, forall f xs)
    
    let existsOne f lst =
        let rec inner lst status =
            match lst with
            | [] -> FF
            | [x] -> And(f x, status) |> negate
            | x::xs ->
                let fx = f x
                if And(status, fx) |> eval then
                    FF
                else
                    inner xs (Or(fx, status))
        inner lst FF
            
