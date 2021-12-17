module day17

open utils
open System.Text.RegularExpressions

type AreaRange = { MinX:int; MaxX:int;MinY:int;MaxY:int;}
type VelocityData = int * int
type ProbeData = Position * VelocityData

let getInputParsed () =
    let regex = Regex @"-?\d+"
    let inputStr = (System.IO.File.ReadAllText("data\\17.txt"))
    let matches =
        regex.Matches inputStr
        |> Seq.cast<Match>
        |> Seq.toList
        |> List.map (fun x -> int x.Value)
    {MinX = matches.[0]; MaxX = matches.[1]; MinY = matches.[2]; MaxY = matches.[3]}

let isWithinRange range position =
    let (x,y) = position
    x >= range.MinX && x<= range.MaxX && y >= range.MinY && y <= range.MaxY

let canReachRange range position =
    let (x,y) = position
    x < range.MaxX && y > range.MinY

let stepX x = if x > 0 then x - 1 elif x < 0 then x + 1 else 0
let stepY y = y - 1

let calcNextVelocity (velocityData : VelocityData) =
    let (currX,currY) = velocityData
    (stepX currX, stepY currY)

let step (probeData:ProbeData) =
    let (currentXPosition, currentYPosition) = fst probeData
    let (currentXVelocity, currentYVelocity) = snd probeData
    let nextPosition = (currentXPosition + currentXVelocity, currentYPosition + currentYVelocity)
    let nextVelocity = calcNextVelocity (snd probeData)
    (nextPosition, nextVelocity)

let rec canXReach currentXValue range speed =
    let reached xValue = xValue >= range.MinX && xValue <= range.MaxX
    let canReach xValue = (reached xValue) || xValue <= range.MaxX && speed > 0 // assumption - target to right
    match (reached currentXValue, canReach currentXValue) with 
    | (true,_) -> true
    | (_,true) -> canXReach (currentXValue + speed) range (stepX speed)
    | _ -> false
 
let getPossibleXVelocities range =
    seq { 1.. range.MaxX+1}
    |> Seq.filter (fun x -> canXReach 0 range x)

let rec canYReach currentYValue range speed =
    let reached = currentYValue >= range.MinY && currentYValue <= range.MaxY
    let canReach = currentYValue >= range.MinY
    match (reached, canReach) with 
    | (true,_) -> true
    | (_,true) -> canYReach (currentYValue + speed) range (stepY speed)
    | _ -> false

let getPossibleYVelocities range =
    let maxByLowerBound = System.Math.Abs(range.MinY-1) 
    seq { (range.MinY-1)..maxByLowerBound}
    |> Seq.filter (fun x -> canYReach 0 range x)
 
let rec willReachRange range probeData =
    let position = fst probeData    
    let isWithin = isWithinRange range position
    let canReach = canReachRange range position
    match (isWithin, canReach) with
    | (true, _) -> true
    | (_,true) -> willReachRange range (step probeData) 
    | _ ->  false

let getReasonablePossibilities range =
    let xValues = getPossibleXVelocities range |> Seq.toList
    let yValues = getPossibleYVelocities range |> Seq.toList
    cartesian xValues yValues

let answer1 () =
    let range = getInputParsed ()
    getReasonablePossibilities range 
        |> List.map (fun (_,y) -> ((1+y)/2)*y)
        |> List.max

let answer2 () =
    let range = getInputParsed ()
    let allGoodOnes =
        getReasonablePossibilities range
        |> Seq.map (fun xy -> (Position (0,0), VelocityData xy ))
        |> Seq.filter (fun x -> willReachRange range x)
    allGoodOnes |> Seq.distinct |> Seq.length