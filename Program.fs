// Learn more about F# at http://fsharp.org
module AoC2020

open System
open day10

[<EntryPoint>]
let main argv =
    answer1 ()  |> printfn "%i" 
    answer2 ()  |> printfn "%i" 
    0 // return an integer exit code
