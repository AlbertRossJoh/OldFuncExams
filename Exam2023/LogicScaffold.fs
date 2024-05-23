module LogicScaffold
    type prop =
    | TT
    | FF
    | And of prop * prop
    | Or of prop * prop
    let p1 = And(TT, FF)
    let p2 = Or(TT, FF)
    let p3 = And(Or(TT, And(TT, FF)), TT)
    let p4 = And(Or(TT,And(TT,FF)), Or(FF,And(TT,FF)))
    let mod2 x = if x % 2 = 0 then TT else FF
    let mod5 x = if x % 5 = 0 then TT else FF
    let mod6 x = if x % 6 = 0 then TT else FF
    

    let test f =
        let res = printfn "%A"
        res (f TT)
        res (f FF)
        res (f p1)
        res (f p2)
        res (f p3)
        res (f p4)

