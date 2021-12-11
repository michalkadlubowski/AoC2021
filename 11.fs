module day11

open utils

let getInput () =
    (System.IO.File.ReadAllLines("data\\11.txt"))
    |> Seq.map (fun x -> x.ToCharArray() |> Seq.map charToInt)
    |> array2D

let rec processSingleFlash (grid: int [,]) (position: Position) =
    let (x, y) = position

    if grid.[x, y] <> 10 then
        ()
    else
        Array2D.set grid x y (grid.[x, y] + 1)

        let neighboursToIncrease =
            getNeighbouringTilesWithDiagonals position grid
            |> Array.filter (fun (x, y) -> grid.[x, y] < 10)

        neighboursToIncrease
        |> Array.iter (fun (px, py) -> Array2D.set grid px py (grid.[px, py] + 1))

        neighboursToIncrease
        |> Array.filter (fun (x, y) -> grid.[x, y] = 10)
        |> Array.iter (fun p -> processSingleFlash grid p)

let step grid =
    grid
    |> Array2D.iteri (fun x y value -> Array2D.set grid x y (value + 1))

    let allPositionsToFlash =
        grid
        |> Array2D.mapi (fun x y value -> (Position(x, y), value = 10))
        |> array2DtoJagged
        |> Array.collect id
        |> Array.filter (snd)
        |> Array.map fst

    allPositionsToFlash
    |> Array.iter (fun x -> processSingleFlash grid x)
    grid
    |> Array2D.iteri (fun x y value -> Array2D.set grid x y (if value > 9 then 0 else value))

let singleIteration grid =
    step grid
    let res = 
        grid
        |> array2DtoJagged
        |> Array.collect id
        |> Array.filter (fun x -> x = 0)
        |> Array.length
    res

let answer1 () =
    let grid = getInput ()
    [ 1 .. 100 ]
    |> Seq.sumBy (fun _ ->  singleIteration grid)

let answer2 () =
    let grid: int [,] = getInput ()
    let totalNo = Array2D.length1 grid * (Array2D.length2 grid)
    Seq.unfold
        (fun iter ->
            let score= singleIteration grid
            let toReturn = (iter, score)
            Some(toReturn, (iter + 1)))
        1
    |> Seq.find (fun (_, score) -> score = totalNo)
    |> fst