module day6

open utils

let getInput () =
    (System.IO.File.ReadAllText("data\\06.txt"))
        .Split(",")
    |> Seq.map int

let toCountsUsingIndex (nums: seq<int>) =
    let mapOfCounts =
        nums
        |> Seq.groupBy id
        |> Seq.map (fun x -> (fst x, snd x |> Seq.length))
        |> Map.ofSeq
    [ 0 .. 8 ]
    |> Seq.map (fun x ->
        mapOfCounts
        |> Map.tryFind x
        |> Option.defaultValue 0)
    |> Seq.toArray
    |> Array.map int64

let tick2 (countsByStage: int64 array) =
    [| countsByStage.[1] // 0
       countsByStage.[2] // 1
       countsByStage.[3] // 2
       countsByStage.[4] // 3
       countsByStage.[5] // 4
       countsByStage.[6] // 5
       countsByStage.[7] + countsByStage.[0] // 6
       countsByStage.[8] // 7
       countsByStage.[0] // 8
       |]

let answer1 () =
    let data = getInput ()
    [ 1 .. 80 ]
    |> Seq.fold (fun acc x -> tick2 acc) (data |> toCountsUsingIndex)
    |> Seq.sum


let answer2 () =
    let data = getInput ()
    [ 1 .. 256 ]
    |> Seq.fold (fun acc x -> tick2 acc) (data |> toCountsUsingIndex)
    |> Seq.sum
