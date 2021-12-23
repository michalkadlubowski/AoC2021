module day21

open utils
open System.Collections.Generic

type Score = int
type Position = int
type Count = bigint
type PlayerData = Position * Score

let getInputParsed () =
    let inputLines = (System.IO.File.ReadAllLines("data\\21.txt"))
    let player1position = inputLines.[0].[28..] |> int
    let player2position = inputLines.[1].[28..] |> int
    (player1position, player2position)

let rolls =
    let allRolls =
        Seq.initInfinite(fun x -> (x % 100) + 1)
        |> Seq.chunkBySize 3
        |> Seq.map (fun x -> x |> Seq.sum)
        |> Seq.take 100
        |> Seq.toList
    seq { while true do yield! allRolls }
    
let movePlayerAndGetScore (playerData:PlayerData) (rollSum: int)=
    let (playerPosition, playerScore) = playerData
    let newPositionBeforeAdjustment = (playerPosition + rollSum)
    let newPosition = if newPositionBeforeAdjustment%10=0 then 10 else newPositionBeforeAdjustment%10
    let newScore = playerScore+newPosition
    PlayerData (newPosition, newScore)

let rec findWinningScore (index:int) (p1Data:PlayerData) (p2Data:PlayerData) (rolls:IEnumerator<int>) =
    if snd p1Data >= 1000  || snd p2Data >= 1000 then
       (index, p1Data, p2Data)
    else
        rolls.MoveNext()
        if index%2 = 0 then
            findWinningScore (index+1) (movePlayerAndGetScore p1Data rolls.Current) p2Data rolls
        else
            findWinningScore (index+1) p1Data (movePlayerAndGetScore p2Data rolls.Current) rolls

let allPossibilities =
    let allNums = [|1;2;3|] |> Array.toList
    let allPossibilities = cartesian allNums allNums |> cartesian allNums
    let distribution =
        allPossibilities
            |> Seq.map(fun (x,(y,z)) -> x+y+z)
            |> Seq.groupBy id
            |> Seq.map (fun (x,y) -> (x, y |> Seq.length))
            |> Seq.toArray
    distribution

let playerTurnInMultiverse playerData =
    allPossibilities |>
    Seq.map(fun (i,count) -> (movePlayerAndGetScore playerData i, count))

let rec multipleTurnsInMultiverse player1Data player2Data (summary : bigint*bigint) (multiplier:bigint) =
    let player1PossiblePlayerData : (PlayerData * Count) list =
        allPossibilities
        |> Seq.map(fun (i,count) -> (movePlayerAndGetScore player1Data i, multiplier * (count |> bigint)))
        |> Seq.toList
    let player2PossiblePlayerData : (PlayerData * Count) list =
        allPossibilities
        |> Seq.map(fun (i,count) -> (movePlayerAndGetScore player2Data i, multiplier * (count |> bigint)))
        |> Seq.toList

    let allCombinations = cartesian player1PossiblePlayerData player2PossiblePlayerData

    let playerWon = fun ((_,score),_) -> score >= 3
    let gamesWhenP1Won = player1PossiblePlayerData |> Seq.filter (fun p1 -> playerWon p1) |> Seq.sumBy (fun  (_,count) -> count)
    let gamesWhenP2Won = allCombinations |> Seq.filter (fun (p1,p2) -> (not (playerWon p1) && playerWon p2)) |> Seq.sumBy (fun (p1,p2) -> (snd p1) * (snd p2))
    let gamesWhenNooneWon = allCombinations |> Seq.filter (fun (p1,p2) ->(not (playerWon p1)  && not (playerWon p2)))
    let newSummary = (fst summary +  gamesWhenP1Won, snd summary +  gamesWhenP2Won)
    if gamesWhenNooneWon |> Seq.length = 0 then
        newSummary
    else
        let res =
            gamesWhenNooneWon
            |> Seq.map (fun (p1Possibilities, p2Possibilities) -> 
                let mult = (snd p1Possibilities)*(snd p2Possibilities)
                multipleTurnsInMultiverse (fst p1Possibilities) (fst p2Possibilities) newSummary mult)
            |> Seq.reduce (fun (x,y) (x2,y2) -> (x+x2, y+y2))
        res
        
let answer1 () =
    let (pos1,pos2) = getInputParsed()
    let player1Data = PlayerData (pos1, 0)
    let player2Data = PlayerData (pos2, 0)
    let enum = rolls.GetEnumerator()
    let (i,p1,p2) = findWinningScore 0 player1Data player2Data enum
    (i*3) * (System.Math.Min(snd p1, snd p2))
   

let answer2 () =
    let (pos1,pos2) = getInputParsed()
    let player1Data = PlayerData (pos1, 0)
    let player2Data = PlayerData (pos2, 0)
    let res = multipleTurnsInMultiverse player1Data player2Data (bigint 0, bigint 0) (bigint 1)
    0