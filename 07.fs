module day7

open utils

let getInput () =
    (System.IO.File.ReadAllText("data\\07.txt"))
        .Split(",")
    |> Seq.map int

let answer1 () =
    let data =
        getInput () |> Seq.toArray |> Array.sortBy id
    let mean = data.[data.Length / 2]
    data |> Seq.sumBy (fun x -> abs (x - mean))

let answer2 () =
    let data = getInput ()
    let allPoints = [ Seq.min data .. Seq.max data ]
    let calcNewType pos1 pos2 =
        let diff = pos1 - pos2 |> abs
        ((1 + diff) * diff) / 2
    let calcResultForPosition position =
        data
        |> Seq.sumBy (fun x -> (calcNewType x position))
    allPoints
    |> Seq.map calcResultForPosition
    |> Seq.min
