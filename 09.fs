module day9

open utils
type Position = int * int

let getInput () =
    (System.IO.File.ReadAllLines("data\\09.txt"))
    |> Seq.map (fun x -> x.ToCharArray() |> Seq.map charToInt)
    |> array2D

let transformations: (int -> Position -> Position) [] =
    [| (fun offset (x, y) -> x - offset, y)
       (fun offset (x, y) -> x + offset, y)
       (fun offset (x, y) -> x, y + offset)
       (fun offset (x, y) -> x, y - offset) |]

let withinBounds<'a> (postion: Position) (arr: 'a [,]) =
    let width = arr |> Array2D.length1
    let height = arr |> Array2D.length2
    let (x, y) = postion
    (0 <= x && x < width) && (0 <= y && y < height)

let getNeighbouringTiles (postion: Position) (grid) =
    transformations
    |> Array.map (fun transformFun -> transformFun 1 postion)
    |> Array.filter (fun position -> (withinBounds position grid))

let getNeighbouringTilesValues (postion: Position) (grid) =
    getNeighbouringTiles postion grid
    |> Array.map (fun x -> grid.[fst x, snd x])

let lowestPoitPositions array2d =
    array2d
    |> Array2D.mapi (fun x y item ->
        (getNeighbouringTilesValues (x, y) array2d
         |> Array.filter (fun x -> x <= item)
         |> Array.length = 0,
         Position(x, y)))
    |> array2DtoJagged
    |> Array.collect id
    |> Array.filter fst
    |> Array.map snd

let rec getLake (visitedPositions: Position array) (position: Position) (array2d: int [,]) =
    let neighbours = getNeighbouringTiles position array2d

    let filteredNeighbours =
        neighbours
        |> Array.filter (fun (x, y) ->
            array2d.[x, y] <> 9
            && (visitedPositions |> Array.contains (x, y) |> not))

    if (filteredNeighbours.Length = 0) then
        visitedPositions
    else
        let allVisited =
            filteredNeighbours
            |> Array.append visitedPositions
            |> Array.distinct

        filteredNeighbours
        |> Array.fold (fun acc x -> getLake acc x array2d) allVisited
        |> Array.distinct

let answer1 () =
    let inputArray2d = getInput ()

    inputArray2d
    |> lowestPoitPositions
    |> Array.sumBy (fun (x, y) -> inputArray2d.[x, y] + 1)

let answer2 () =
    let inputArray2d = getInput ()
    let lowestPositions = inputArray2d |> lowestPoitPositions

    lowestPositions
    |> Array.map (fun position -> getLake [| position |] position inputArray2d)
    |> Array.map (fun x -> x.Length)
    |> Array.sortByDescending id
    |> Array.take 3
    |> Array.reduce (*)