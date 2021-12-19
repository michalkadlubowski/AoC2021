module day18

type SnailfishNumber = SnailfishNumberPart * SnailfishNumberPart 
and SnailfishNumberPart = SimpleSnailFishNumer of int | ComplexSnailfishNumber of SnailfishNumber

let rec parse (line:string) : SnailfishNumber =
    let indexOfPairDelimiter =
        let mutable delimitersCount = 0
        let mutable openBracketsCount = 0
        line.ToCharArray()
        |> Array.findIndex (fun char ->
            match char with
            | '[' ->
                openBracketsCount <- openBracketsCount + 1
                false
            | ',' ->
                delimitersCount <- delimitersCount + 1
                delimitersCount = openBracketsCount
            | _ -> false
        )
    let fin = line.Length-2
    let left = line.[1..indexOfPairDelimiter-1]
    let right = line.[indexOfPairDelimiter+1..fin]
    let isInt (s:string) = System.Int32.TryParse s |> fst
    let leftResult = if (isInt left) then SimpleSnailFishNumer (int left) else  ComplexSnailfishNumber (parse left)
    let rightResult = if (isInt right) then SimpleSnailFishNumer (int right) else ComplexSnailfishNumber (parse right)
    (leftResult, rightResult)

let getInputParsed () =
    (System.IO.File.ReadAllLines("data\\18.txt"))
    |> Array.map parse    

let rec serialize num =
    let rec printSimple x =
        match x with 
        | SimpleSnailFishNumer s -> sprintf "%i" s
        | ComplexSnailfishNumber c -> sprintf "[%s,%s]" (printSimple (fst c)) (printSimple (snd c))
    let left = printSimple (fst num)
    let right = printSimple (snd num)
    sprintf "[%s,%s]" left right

let rec getExplosionData snailNum depth =
    match snailNum with
    | (ComplexSnailfishNumber x, ComplexSnailfishNumber y) -> getExplosionData x (depth+1) |> Option.orElse (getExplosionData y (depth+1))
    | (ComplexSnailfishNumber x, _) -> getExplosionData x (depth + 1)
    | (_, ComplexSnailfishNumber y) -> getExplosionData y (depth + 1)
    | (SimpleSnailFishNumer x, SimpleSnailFishNumer y) when depth = 4 -> Some(x,y) 
    | _ -> None

let shouldExplode (n: SnailfishNumber) = 
    getExplosionData n 0 |> Option.isSome

let findLastSimpleBeforeExplosionRec node =
    let mutable exploSionStepNo = None
    let mutable lastSimpleValueStepBeforeExplostion = None

    let rec findLastSimpleBeforeExplosionRec n depth iter =
        match n with
        | SimpleSnailFishNumer s ->    
            if exploSionStepNo.IsNone then
                lastSimpleValueStepBeforeExplostion <- Some iter
            (n, iter)
        | ComplexSnailfishNumber (x,y) -> 
            if depth = 4 && exploSionStepNo.IsNone then
                exploSionStepNo <- Some iter
            let (left, newIter) = findLastSimpleBeforeExplosionRec x (depth+1) (iter+1)
            let (right, finalIter) = findLastSimpleBeforeExplosionRec y (depth+1) (newIter+1)
            (ComplexSnailfishNumber (left, right), finalIter)
    findLastSimpleBeforeExplosionRec node 0 0 |> ignore
    (lastSimpleValueStepBeforeExplostion, exploSionStepNo)

let explodeFinal 
    (lastSimpleIterNo: int option) 
    (explosionIterNo: int option) 
    (explosionLeft : int) 
    (explosionRight : int)
    node =
    let mutable shouldSetRight = false;
    let rec explodeFinalRec n iter =
        match n with
        | SimpleSnailFishNumer x -> 
            if lastSimpleIterNo.IsSome && iter = lastSimpleIterNo.Value   then
                (SimpleSnailFishNumer (x + explosionLeft), iter)          
            elif shouldSetRight then
                shouldSetRight <- false
                (SimpleSnailFishNumer (x + explosionRight), iter)            
            else
                (n, iter)
        | ComplexSnailfishNumber (x,y) -> 
            if explosionIterNo.IsSome && iter = explosionIterNo.Value then
                shouldSetRight <- true
                (SimpleSnailFishNumer 0, iter+2)
            else
                let (left, newIter) = explodeFinalRec x (iter+1)
                let (right, finalIter) = explodeFinalRec y (newIter+1)
                (ComplexSnailfishNumber (left, right), finalIter)
    let res = explodeFinalRec node 0 |> fst
    match res with
    | ComplexSnailfishNumber x -> x

let rec shouldSplit n = 
    match n with
    | SimpleSnailFishNumer x -> x >= 10
    | ComplexSnailfishNumber (x, y) -> shouldSplit x || shouldSplit y

let split n =
    let splitFn x =
        let left : int = System.Math.Round((float x)/(float 2), System.MidpointRounding.ToZero) |> int
        let right : int = System.Math.Round((float x)/(float 2), System.MidpointRounding.AwayFromZero) |> int
        ComplexSnailfishNumber (SimpleSnailFishNumer left, SimpleSnailFishNumer right)
    
    let rec splitMap num =
        let (leftSnailNum, rightSnailNum) = num
        match (leftSnailNum, rightSnailNum) with
        | (SimpleSnailFishNumer x,y) when x >= 10 -> (splitFn x, y)
        | (ComplexSnailfishNumber x, y) when shouldSplit leftSnailNum -> (ComplexSnailfishNumber (splitMap x), y)
        | (x, SimpleSnailFishNumer y) when y >= 10 -> (x, splitFn y)
        | (x,ComplexSnailfishNumber y) when shouldSplit rightSnailNum -> (x, ComplexSnailfishNumber (splitMap y))
        | _ -> (leftSnailNum, rightSnailNum)
    splitMap n

let explode node =
    let (exLeft, exRight) = getExplosionData node 0 |> Option.defaultValue (0,0)
    let (l,ex) = findLastSimpleBeforeExplosionRec (ComplexSnailfishNumber node)
    explodeFinal l ex exLeft exRight (ComplexSnailfishNumber node)

let rec reduceSnailFishNumber n =
    if shouldExplode n then
        reduceSnailFishNumber (explode n)
    elif shouldSplit (ComplexSnailfishNumber n) then
        reduceSnailFishNumber (split n)
    else
        n

let addNumbersAndReduce n1 n2 =
    reduceSnailFishNumber (ComplexSnailfishNumber n1, ComplexSnailfishNumber n2)

let rec calcMagnitude num =
    match num with
    | SimpleSnailFishNumer x -> x
    | ComplexSnailfishNumber (l,r) -> 3 * calcMagnitude l + 2 * calcMagnitude r

let answer1 () =
    getInputParsed ()
        |> Seq.reduce addNumbersAndReduce
        |> ComplexSnailfishNumber
        |> calcMagnitude

let answer2 () =
    let inputNums = getInputParsed () |> Seq.toList
    cartesian inputNums inputNums
    |> Seq.filter (fun (x,y) -> x <> y)
    |> Seq.map (fun (x, y) -> addNumbersAndReduce x y)
    |> Seq.map (fun x -> calcMagnitude (ComplexSnailfishNumber x))
    |> Seq.max