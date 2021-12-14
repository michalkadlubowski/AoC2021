module day14

open utils

type Pair = char*char

let getInputParsed () =
    let dataGroups =
        (System.IO.File.ReadAllLines("data\\14.txt"))
        |> splitSeq ""
        |> Seq.toArray
    let startingStr = dataGroups.[0] |> Seq.head
    let insertions = dataGroups.[1]
    let insertionsMapped =
        insertions
        |> Seq.map (fun x -> x.Split(" -> ") |> (fun x -> (Pair (x.[0].ToCharArray().[0], x.[0].ToCharArray().[1]), (Seq.last x).[0])))
        |> Map.ofSeq
    (startingStr, insertionsMapped)

let step (initialPolymerPairs: Map<Pair,int64>) (insertions:Map<Pair,char>) =
    initialPolymerPairs
        |> Map.toSeq
        |> Seq.collect(fun (pair, count) -> [|(Pair(fst pair,insertions.[pair]), count); (Pair(insertions.[pair], snd pair), count)|])
        |> Seq.groupBy fst
        |> Seq.map (fun (pair,elements) -> (pair, elements |> Seq.sumBy (fun x -> snd x)) )
        |> Map.ofSeq

let findAnswer steps =
    let (polymerTemplate, insertions) = getInputParsed()
    let polymerPairsMap =
        polymerTemplate
        |> Seq.pairwise
        |> Seq.groupBy id
        |> Seq.map (fun (pair,elements) -> (Pair pair, elements |> Seq.length |> int64  ))
        |> Map.ofSeq
    let res =
        [1..steps]
        |> Seq.fold (fun acc x -> step acc insertions) polymerPairsMap
        |> Map.toSeq
        |> Seq.collect (fun (x,count) -> [|(fst x, count); (snd x, count)|])
        |> Seq.groupBy fst
        |> Seq.map (fun (x, elements) -> (x, elements |> Seq.sumBy snd))
        |> Seq.map (fun (x,cnt) -> (x, (cnt + (1 |> int64))/ (2 |> int64)))
        |> Seq.toArray
        |> Array.sortByDescending snd
        |> Array.map snd
    res.[0] - res.[res.Length-1]

let answer1 () =
    findAnswer 10

let answer2 () =
    findAnswer 40