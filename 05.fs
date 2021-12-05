module day5

open utils

type Point = int * int

let parsePoint (str: string) =
    let numStrs = str.Split(",")
    (int numStrs.[0], int numStrs.[1])

let parseLine (line: string) =
    let points = line.Split(" -> ")
    let p1 = parsePoint points.[0]
    let p2 = parsePoint points.[1]
    (p1, p2)

let getInput =
    readLines ("data\\05.txt") |> Seq.map parseLine

let expandPoints withDiagonals (p1: int * int) (p2: int * int) =
    let minX = System.Math.Min(fst p1, fst p2)
    let maxX = System.Math.Max(fst p1, fst p2)
    let minY = System.Math.Min(snd p1, snd p2)
    let maxY = System.Math.Max(snd p1, snd p2)
    let dx = maxX - minX
    let dy = maxY - minY

    if dx = 0 then
        [ minY .. maxY ] |> Seq.map (fun x -> (fst p1, x))
    else if dy = 0 then
        [ minX .. maxX ] |> Seq.map (fun x -> (x, snd p1))
    else if dx = dy && withDiagonals then
        let yFun =
            if fst p1 = minX && snd p1 = minY
               || fst p2 = minX && snd p2 = minY then
                (fun i -> minY + i)
            else
                (fun i -> maxY - i)

        [ minX .. maxX ]
        |> Seq.mapi (fun i x -> (x, yFun i))
    else
        Seq.empty

let countNonUniquePoins withDiagonals points =
    points
    |> Seq.collect (fun x -> expandPoints withDiagonals (fst x) (snd x))
    |> Seq.groupBy (id)
    |> Seq.filter (fun x -> Seq.length (snd x) > 1)
    |> Seq.length

let answer1 () = getInput |> countNonUniquePoins false

let answer2 () = getInput |> countNonUniquePoins true
