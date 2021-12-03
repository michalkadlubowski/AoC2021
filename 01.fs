module day1

open utils

let dataAsInts = 
    readLines("data\\01-01.txt")  |>
    Seq.map System.Int32.Parse

let answer1 = dataAsInts 
                |> Seq.pairwise
                |> Seq.map(fun (i, y) -> y>i)
                |> Seq.filter (fun (i) -> i = true)
                |> Seq.length

let answer2 = dataAsInts 
                |> Seq.windowed 3
                |> Seq.pairwise
                |> Seq.map (fun(i,y) -> (Seq.sum i, Seq.sum y))
                |> Seq.map(fun (i, y) -> y>i)
                |> Seq.filter (fun (i) -> i = true)
                |> Seq.length

