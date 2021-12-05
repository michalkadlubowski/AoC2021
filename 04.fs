module day4

open utils

type BoardField = int * bool

let parseBoard (boardStr: string) =
    let lines = boardStr.Split("\r\n")

    let nums =
        lines
        |> Seq.map (fun x ->
            x.Split(" ")
            |> Seq.filter (fun y -> y.Length <> 0)
            |> Seq.map (fun x -> BoardField(int x, false)))
        |> array2D

    nums

let getInput =
    let chunks =
        System
            .IO
            .File
            .ReadAllText("data\\04.txt")
            .Split("\r\n\r\n")
        |> Seq.toArray

    let nums =
        chunks.[0].Split(",")
        |> Seq.map int
        |> Seq.toArray

    let boards = chunks.[1..] |> Array.map parseBoard
    (nums, boards)

let isBingo (x: BoardField [,]) =
    let rowMatch =
        array2DtoJagged x
        |> Seq.exists (fun arr -> (arr |> Seq.fold (fun x y -> x && snd y) true) = true)

    let colMatch =
        array2DtoJaggedColumns x
        |> Seq.exists (fun arr -> (arr |> Seq.fold (fun x y -> x && snd y) true) = true)

    rowMatch || colMatch

let calculateScore (board: BoardField [,]) (num: int) =
    let unmarkedSum =
        board
        |> Seq.cast<BoardField>
        |> Seq.filter (fun x -> snd x = false)
        |> Seq.map (fun x -> fst x)
        |> Seq.sum

    unmarkedSum * num

let applyNumber (num: int) (board: BoardField [,]) =
    match find2Dindex board (fun x -> fst x = num) with
    | Some (x, y) -> Array2D.set board x y (BoardField(num, true))
    | None -> ()

    let score =
        match isBingo board with
        | true -> Some(calculateScore board num)
        | false -> None

    (board, score)

let tryAllBoards (boards: BoardField [,] []) (num: int) =
    boards
    |> Seq.map (fun x -> applyNumber num x)
    |> Seq.toArray // Materialize to ensure all boards are updated
    |> Array.tryPick (fun x -> snd x)

let answer1 () =
    let (nums, boards) = getInput

    let res =
        nums |> Seq.pick (fun x -> tryAllBoards boards x)

    res

let answer2 () =
    let (nums, boards) = getInput
    let mutable latestScore = 0

    for num in nums do
        let nonBingoBoards =
            boards
            |> Array.filter (fun x -> isBingo x = false)

        match tryAllBoards nonBingoBoards num with
        | Some x -> latestScore <- x
        | None -> ()

    latestScore
