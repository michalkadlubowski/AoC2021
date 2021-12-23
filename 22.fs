module day22

open utils
open System.Text.RegularExpressions
open System

type Cuboid = { OnOff: bool; MinX:int64; MaxX:int64; MinY:int64; MaxY:int64; MinZ:int64; MaxZ:int64; Exclusions : Cuboid list }
    with   
    member  this.calculateArea () =
        let mainArea = ((this.MaxX - this.MinX) + int64 1)* ((this.MaxY - this.MinY)+ int64 1) * ((this.MaxZ - this.MinZ)+ int64 1)
        let exclusionsArea = this.Exclusions |> List.sumBy (fun x ->  x.calculateArea())
        mainArea - exclusionsArea

let parseLine str =
    let pattern = @"((-?\d+)\.\.(-?\d+))"
    let regex = Regex pattern
    let constrains =
        regex.Matches str
        |> Seq.cast<Match>
        |> Seq.toList
        |> List.map (fun x -> x.Groups.Values |> Seq.toArray)
        |> List.map (fun values -> int64 values.[2].Value, int64 values.[3].Value)
    { OnOff = str.[1] = 'n'
      MinX = fst constrains.[0]; MaxX = snd constrains.[0]
      MinY = fst constrains.[1]; MaxY = snd constrains.[1]
      MinZ = fst constrains.[2]; MaxZ = snd constrains.[2]
      Exclusions = List.empty<Cuboid>}

let getInputParsed () =
    let lines = (System.IO.File.ReadAllLines("data\\22.txt"))
    lines |> Array.map parseLine

let rec intersectCubes (existingCube:Cuboid) (cubeToApply:Cuboid) =
    let intersectionCube: Cuboid =
        {   OnOff = existingCube.OnOff |> not
            MinX = Math.Max(existingCube.MinX, cubeToApply.MinX); MaxX = Math.Min(existingCube.MaxX, cubeToApply.MaxX)
            MinY = Math.Max(existingCube.MinY, cubeToApply.MinY); MaxY = Math.Min(existingCube.MaxY, cubeToApply.MaxY)
            MinZ = Math.Max(existingCube.MinZ, cubeToApply.MinZ); MaxZ = Math.Min(existingCube.MaxZ, cubeToApply.MaxZ)
            Exclusions = List.empty<Cuboid> }
    if intersectionCube.MinX > intersectionCube.MaxX || intersectionCube.MinY > intersectionCube.MaxY || intersectionCube.MinZ > intersectionCube.MaxZ then
        existingCube
    else
        let newExclusions = existingCube.Exclusions |> List.map (fun x -> intersectCubes x cubeToApply)
        { existingCube with Exclusions = intersectionCube::newExclusions }

let answer1 () =
    getInputParsed() 
    |> Seq.take 10
    |> Seq.fold (fun acc y -> y::(acc |> List.map (fun x -> intersectCubes x y))) List.empty<Cuboid>
    |> Seq.filter(fun x -> x.OnOff) 
    |> Seq.sumBy (fun x -> x.calculateArea())

let answer2 () =
    getInputParsed() 
    |> Seq.fold (fun acc y -> y::(acc |> List.map (fun x -> intersectCubes x y))) List.empty<Cuboid>
    |> Seq.filter(fun x -> x.OnOff) 
    |> Seq.sumBy (fun x -> x.calculateArea())