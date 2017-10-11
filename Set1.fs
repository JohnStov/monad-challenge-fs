type Seed = {unSeed:bigint}

let mkSeed (s : int) =
    {unSeed = bigint s}

let rand s = 
    let s' = (s.unSeed * bigint 16807) % bigint 0x7FFFFFFF
    s' , {unSeed = s'}

let fiveRands () = 
    let (v1, s1) = rand (mkSeed 1)
    let (v2, s2) = rand s1
    let (v3, s3) = rand s2
    let (v4, s4) = rand s3
    let v5 = fst (rand s4)
    [v1; v2; v3; v4; v5]

let product vs =
    vs |> List.fold (*) (bigint 1)
