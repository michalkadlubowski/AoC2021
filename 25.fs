module day25

open utils
open System.Collections.Generic


let getInput () =
    (System.IO.File.ReadAllLines("data\\25.txt"))
    |> Seq.map (fun x -> x.ToCharArray())
    |> array2D

let getNextPosition state p =
    let (x,y) = p
    let rowsCount = state |> Array2D.length1
    let colsCount = state |> Array2D.length2
    let cellState = state.[x,y]
    match cellState with
    | 'v' -> if x = rowsCount - 1 then (0,y) else (x+1,y)
    | '>' -> if y = colsCount - 1 then (x,0) else (x,y+1)

let canMoveToPosition (state: char[,]) (p: Position) =
    state.[fst p, snd p] = '.'

let moveSingleStep state =
    let shouldMove p =
        getNextPosition state p
        |> canMoveToPosition state
    let flattened =
        state
        |> Array2D.mapi(fun x y v -> ((x,y), v))
        |> array2DtoJagged
        |> Seq.collect id
    
    let toMoveEast =
        flattened
        |> Seq.filter(fun (p,v) -> v = '>' && shouldMove p)
        |> Seq.toArray
    
    toMoveEast
    |> Seq.iter(fun (p,v) ->
        let newPos = getNextPosition state  p
        Array2D.set state (fst newPos) (snd newPos) v
        Array2D.set state (fst p) (snd p) '.')

    let toMoveSouth =
        flattened
        |> Seq.filter(fun (p,v) -> v = 'v' && shouldMove p)
        |> Seq.toArray

    toMoveSouth
    |> Seq.iter(fun (p,v) ->
        let newPos = getNextPosition state  p
        Array2D.set state (fst newPos) (snd newPos) v
        Array2D.set state (fst p) (snd p) '.')
    
    (toMoveEast.Length + toMoveSouth.Length)

let answer1 () =
    let initialState = getInput()
    seq { 1 .. System.Int32.MaxValue }
    |> Seq.find (fun x -> moveSingleStep initialState = 0)

let answer2 () =
    0 