module day24

open utils
open System.Collections.Generic

type State = {x:int; y: int; z: int; w: int}

let initialState = {x=0;y=0;z=0;w=0}

let mapTransformToFn opetator firstParam sndParam (state:State) =
    let firstParamGetter = 
        match firstParam with 
        | "x" -> state.x
        | "y" -> state.y
        | "z" -> state.z
        | "w" -> state.w
    let resultSetter = 
        match firstParam with 
            | "x" -> fun res -> { state with x = res}
            | "y" -> fun res -> { state with y = res}
            | "z" -> fun res -> { state with z = res}
            | "w" -> fun res -> { state with w = res}
    let sndParamGetter =
        match sndParam with 
        | "x" -> state.x
        | "y" -> state.y
        | "z" -> state.z
        | "w" -> state.w
        | intStr -> 
            //printf "%s \t" intStr
            int intStr
    match opetator with
    | "add" -> resultSetter (firstParamGetter + sndParamGetter)
    | "mul" -> resultSetter (firstParamGetter * sndParamGetter)
    | "div" -> resultSetter (int (System.Math.Round((float firstParamGetter) / (float sndParamGetter), System.MidpointRounding.ToZero)))
    | "mod" -> resultSetter (firstParamGetter % sndParamGetter)
    | "eql" -> resultSetter (if firstParamGetter = sndParamGetter then 1 else 0)

let mapInputInstruction paramName inputNumber state  =
    let inputFn =
        match paramName with
            | "x" -> fun res -> { state with x = inputNumber}
            | "y" -> fun res -> { state with y = inputNumber}
            | "z" -> fun res -> { state with z = inputNumber}
            | "w" -> fun res -> { state with w = inputNumber}
    inputFn inputNumber
    
let parseLine (line:string) (inputs: IEnumerator<int>) =
    let chunks = line.Split(' ')
    let operator = chunks.[0]
    let resultingFn =
        match operator with
        | "inp" -> 
            //printfn "\r\n WAT"
            let input = inputs.Current
            inputs.MoveNext() |> ignore
            mapInputInstruction chunks.[1] input
        | _     -> mapTransformToFn chunks.[0] chunks.[1] chunks.[2]
    resultingFn

let getInputParsed (inputNums: int seq) =
    let enumerator = inputNums.GetEnumerator()
    enumerator.MoveNext() |> ignore
    let lines = (System.IO.File.ReadAllLines("data\\24.txt"))
    lines
    |> Array.take 10
    |> Array.map (fun l -> parseLine l enumerator)
    |> Array.reduce (>>)

let convertNumberToSeq (number: int64) = 
        let numberString = string number
        numberString.ToCharArray() 
        |> Seq.map (string >> int)

let answer1 () =
    let instructions = getInputParsed 
    //let test = instructions (convertNumberToSeq (9L)) initialState
    let max = 9999999999L
    let res =
        Seq.unfold(fun (i:int64) -> Some (i- int64 1, i- int64 1)) max
        |> Seq.find (fun num ->
            let digits = convertNumberToSeq num
            if digits |> Seq.tryFind (fun x -> x = 0) |> Option.isSome then 
                false
            else
                let resState = instructions digits initialState
                resState.z <> 0
            )
    res
    
let answer2 () =
    0  