module day10

type LineResult =
    | Incomplete of int64
    | Incorrect of int64

let getInput () =
    (System.IO.File.ReadAllLines("data\\10.txt"))
    |> Seq.map (fun x -> x.ToCharArray())

let getPointsIncorrect char =
    match char with
    | ')' -> 3
    | ']' -> 57
    | '}' -> 1197
    | '>' -> 25137

let getPointsIncomplete char =
    match char with
    | ')' -> 1
    | ']' -> 2
    | '}' -> 3
    | '>' -> 4

let getClosing char =
    match char with
    | '(' -> ')'
    | '[' -> ']'
    | '{' -> '}'
    | '<' -> '>'

let scoreLine (line: char array) =
    let stack = System.Collections.Generic.Stack()

    let invalidChar =
        line
        |> Array.tryFind (fun char ->
            match char with
            | '('
            | '['
            | '{'
            | '<' ->
                stack.Push char
                false
            | closing ->
                if getClosing (stack.Peek()) = closing then
                    stack.Pop() |> ignore
                    false
                else
                    true)

    if invalidChar.IsSome then
        int64 (getPointsIncorrect invalidChar.Value) 
        |> Incorrect
    else
        stack.ToArray() 
        |> Array.map getClosing
        |> Array.fold (fun acc x -> (acc * (int64 5)) + (int64 (getPointsIncomplete x))) (int64 0)
        |> Incomplete

let answer1 () =
    getInput ()
    |> Seq.map scoreLine
    |> Seq.map (fun x ->
        match x with
        | Incorrect res -> res
        | _ -> int64 0)
    |> Seq.sum

let answer2 () =
    let scoresSorted =
        getInput ()
        |> Seq.map scoreLine
        |> Seq.map (fun x ->
            match x with
            | Incomplete res -> res
            | _ -> int64 0)
        |> Seq.filter (fun x -> x <> int64 0)
        |> Seq.sortByDescending id
        |> Seq.toArray

    scoresSorted.[scoresSorted.Length / 2]
