module day13

open utils

let getInputAsPoints () =
    let dataGroups =
        (System.IO.File.ReadAllLines("data\\13.txt"))
        |> splitSeq ""
        |> Seq.toArray
    let points = dataGroups.[0]
    let folds = dataGroups.[1]
    let pointsMapped =
        points
        |> Seq.map (fun x -> x.Split(',') |> Seq.toArray)
        |> Seq.map (fun x -> Position ((x.[0] |> int),(x.[1] |> int)))
    let foldsMapped =
        folds
        |> Seq.map (fun x -> x.Split(' ') |> Seq.last)
        |> Seq.map (fun x -> x.Split('=') |> Seq.toArray)
        |> Seq.map (fun x -> (x.[0],x.[1] |> int))
    (pointsMapped, foldsMapped)

let foldY foldValue points=
    points
    |> Seq.map(fun (x,y) -> Position (x, if y < foldValue then y else foldValue-(y-foldValue)))
    |> Seq.distinct

let foldX foldValue (points: Position seq)=
    points
    |> Seq.map(fun (x,y) -> Position ((if x < foldValue then x else foldValue-(x-foldValue)), y))
    |> Seq.distinct

let mapFold fold =
    let (t,v) = fold
    match t with
    | "x" -> foldX v
    | "y" -> foldY v

let printPoints (points: Position seq) =
    let maxX = points |> Seq.map fst |> Seq.max
    let maxY = points |> Seq.map snd |> Seq.max
    let matrix = Array2D.create (maxX+1) (maxY+1) false
    points |> Seq.iter(fun (x,y) -> Array2D.set matrix x y true)
    for r = 0 to Array2D.length2 matrix - 1 do
        printfn "%s" ""
        for c = 0 to Array2D.length1 matrix - 1 do
            printf "%s " (if matrix.[c, r] then "#" else " ")
    printfn "%s" ""        

let answer1 () =
    let (points, folds) = getInputAsPoints()
    let firstFold = folds |> Seq.map mapFold |> Seq.head
    firstFold points |> Seq.length

let answer2 () =
    let (points, folds) = getInputAsPoints()
    let res =
        folds
        |> Seq.map mapFold
        |> Seq.fold (fun acc x -> x acc) points
    printPoints res
    res |> Seq.length