module day19
type Point3D = { x:int;y:int;z:int}

let transformFunctions =
    [|
    fun (p: Point3D) -> { x= p.x; y= p.y; z = p.z}
    fun (p: Point3D) -> { x= -p.y; y= p.x; z = p.z}
    fun (p: Point3D) -> { x= -p.x; y= -p.y; z = p.z}
    fun (p: Point3D) -> { x= p.y; y= -p.x; z = p.z}

    fun (p: Point3D) -> { x= p.x; y= -p.y; z = -p.z}
    fun (p: Point3D) -> { x= p.y; y= p.x; z = -p.z}
    fun (p: Point3D) -> { x= -p.x; y= p.y; z = -p.z}
    fun (p: Point3D) -> { x= -p.y; y= -p.x; z = -p.z}    

    fun (p: Point3D) -> { x= p.y; y= p.z; z = p.x}
    fun (p: Point3D) -> { x= -p.z; y= p.y; z = p.x}
    fun (p: Point3D) -> { x= -p.y; y= -p.z; z = p.x}
    fun (p: Point3D) -> { x= p.z; y= -p.y; z = p.x}

    fun (p: Point3D) -> { x= p.y; y= -p.z; z = -p.x}
    fun (p: Point3D) -> { x= p.z; y= p.y; z = -p.x}
    fun (p: Point3D) -> { x= -p.y; y= p.z; z = -p.x}
    fun (p: Point3D) -> { x= -p.z; y= -p.y; z = -p.x}

    fun (p: Point3D) -> { x= p.x; y= -p.z; z = p.y}
    fun (p: Point3D) -> { x= p.z; y= p.x; z = p.y}
    fun (p: Point3D) -> { x= -p.x; y= p.z; z = p.y}
    fun (p: Point3D) -> { x= -p.z; y= -p.x; z = p.y}

    fun (p: Point3D) -> { x= p.x; y= p.z; z = -p.y}
    fun (p: Point3D) -> { x= -p.z; y= p.x; z = -p.y}
    fun (p: Point3D) -> { x= -p.x; y= -p.z; z = -p.y}
    fun (p: Point3D) -> { x= p.z; y= -p.x; z = -p.y}   
    |]

let rebasePoint newBase point =
    { x = point.x - newBase.x; y = point.y - newBase.y; z = point.z - newBase.z}

let getInputParsed() =
    (System.IO.File.ReadAllLines("data\\19.txt"))
    |> splitSeq ("")
    |> Seq.map(fun x -> 
        x 
        |> Seq.skip 1 
        |> Seq.map(fun s -> s.Split (',') |> Array.map int)
        |> Seq.map(fun nums -> { x= nums.[0]; y= nums.[1]; z= nums.[2]})
        |> Seq.toArray)

let findMatchingBaseAssumeSameDirection (existing: Point3D seq) (candidate: Point3D seq) =
    cartesian (existing|> Seq.toList) (candidate |> Seq.toList) 
    |> Seq.tryFind (fun (existingP, newP) -> 
        let newBase  = { x = newP.x - existingP.x; y = newP.y - existingP.y; z = newP.z - existingP.z}
        let rebasedCandidates = 
            candidate
            |> Seq.map (rebasePoint newBase)
            |> Seq.toArray
        let duplicateCount =
            existing
            |> Seq.append rebasedCandidates
            |> Seq.groupBy id
            |> Seq.map snd
            |> Seq.filter (fun x -> x |> Seq.length > 1)
            |> Seq.length
        duplicateCount >= 12
    )
    |> Option.map (fun (existingP, newP) -> { x = newP.x - existingP.x; y = newP.y - existingP.y; z = newP.z - existingP.z})

let findMatching (existing: Point3D seq) (candidate: Point3D seq) =
    let allTransformations = 
        transformFunctions
        |> Seq.map (fun fns -> candidate |> Seq.map fns)
        |> Seq.toArray
    let matchFound =
        allTransformations
            |> Array.Parallel.map (fun x -> (x,findMatchingBaseAssumeSameDirection existing x))
            |> Seq.tryFind (fun (ponts, b) -> Option.isSome b)
    match matchFound with
    | None -> None
    | Some(x, b) -> Some(b.Value, x |> Seq.map (rebasePoint b.Value))

let rec matchPoints (currentMapped: Point3D seq) (bases: Point3D list) (lastSensorsMatched: Point3D[] list) (sensorReadings: Point3D[] seq) =
    printfn "Remaining %i" (sensorReadings |> Seq.length)
    if sensorReadings |> Seq.length = 0 then 
        (currentMapped, bases)
    else
        let (arr, m) = 
            sensorReadings 
            |> Seq.map (fun x -> (x, lastSensorsMatched |> Seq.map (fun m -> findMatching m x) |> Seq.tryFind Option.isSome))
            |> Seq.find (fun x -> Option.isSome (snd x))  
        let (b, points) = m.Value.Value
        let rebasedPoints = points |> Seq.toArray
        let newResult = 
            currentMapped 
            |> Seq.append rebasedPoints
            |> Seq.distinct
            |> Seq.toArray
        let newReadings = sensorReadings |> Seq.filter(fun x -> x <> arr)
        matchPoints newResult (b::bases) (rebasedPoints::lastSensorsMatched) newReadings

let manhattanDist p1 p2 =
    System.Math.Abs(p1.x - p2.x) + System.Math.Abs(p1.y - p2.y) + System.Math.Abs(p1.z - p2.z)
        
let answer1 () =
    let input = getInputParsed() |> Seq.toArray
    matchPoints input.[0] [{x=0;y=0;z=0}] [input.[0]] input.[1..]
    |> fst
    |> Seq.toArray
    |> Seq.length

let answer2 () =
    let input = getInputParsed() |> Seq.toArray
    let res =
        matchPoints input.[0] [{x=0;y=0;z=0}] [input.[0]] input.[1..]
        |> snd
        |> Seq.toList
    let allPairs = cartesian res res 
    allPairs |> Seq.map (fun (p1,p2) -> manhattanDist p1 p2) |> Seq.max