module day2

type SubState =
    { Depth: int
      Horizontal: int
      Aim: int }

let parse (str: string) =
    let split = str.Split [| ' ' |]
    (split.[0], split.[1] |> int)

let apply1 (state: SubState) (cmd: string * int) =
    let modifier = snd cmd

    match fst cmd with
    | "forward" -> { state with Horizontal = state.Horizontal + modifier }
    | "down" -> { state with Depth = state.Depth + modifier }
    | "up" -> { state with Depth = state.Depth - modifier }
    | _ -> state

let apply2 (state: SubState) (cmd: string * int) =
    let modifier = snd cmd

    match fst cmd with
    | "forward" ->
        { state with
            Horizontal = state.Horizontal + modifier
            Depth = state.Depth + (state.Aim * modifier) }
    | "down" -> { state with Aim = state.Aim + modifier }
    | "up" -> { state with Aim = state.Aim - modifier }
    | _ -> state

let getFinalPosition applicator =
    readLines ("data\\02.txt")
    |> Seq.map parse
    |> Seq.fold (fun x y -> applicator x y) { Depth = 0; Horizontal = 0; Aim = 0 }

let answer1 () =
    let x = getFinalPosition apply1
    x.Horizontal * x.Depth

let answer2 () =
    let x = getFinalPosition apply2
    x.Horizontal * x.Depth
