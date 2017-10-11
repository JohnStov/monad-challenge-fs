type Seed = {unSeed:bigint}

let mkSeed (s : int) =
    {unSeed = bigint s}

type Gen<'a> = Seed -> 'a * Seed

let rand s = 
    let s' = (s.unSeed * bigint 16807) % bigint 0x7FFFFFFF
    s' , {unSeed = s'}

let fiveRands () = 
    let v1, s1 = rand (mkSeed 1)
    let v2, s2 = rand s1
    let v3, s3 = rand s2
    let v4, s4 = rand s3
    let v5 = fst (rand s4)
    [v1; v2; v3; v4; v5]

let product vs =
    vs |> List.fold (*) (bigint 1)

let toLetter (v : bigint) =
    (char) (((int) 'a') + (int)(v % bigint 26))

let randLetter s =
    let v, s' = rand s
    toLetter v, s'

let randString3 () =
    let l1, s1 = randLetter (mkSeed 1)
    let l2, s2 = randLetter s1
    let l3 = fst (randLetter s2)
    [l1; l2; l3] |> System.String.Concat

let generalA (f : 'a -> 'a) (g : Gen<'a>)=
    fun s -> 
        let a, s' = g s
        f a, s'

let randEven = generalA (fun x -> x * bigint 2) rand

let randOdd = generalA (fun x -> x + bigint 1) randEven

let randTen = generalA (fun x -> x * bigint 5)randEven 

let generalPair (genA : Gen<'a>) (genB : Gen<'b>) : Gen<'a * 'b> =
    fun s ->
        let a, s1 = genA s
        let b, s2 = genB s1
        (a, b), s2

let randPair = generalPair randLetter rand

let generalB (f: 'a -> 'b -> 'c) (genA : Gen<'a>) (genB : Gen<'b>) : Gen<'c> =
    fun s ->
        let a, s1 = genA s
        let b, s2 = genB s1
        (f a b), s2

let generalPair2 genA genB = generalB (fun a  b -> a, b) genA genB
    
let randPair2 = generalPair2 randLetter rand
