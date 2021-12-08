module day8

open utils

let sequenceWithin seqA seqB =
    seqB
    |> Seq.forall (fun x -> seqA |> Seq.contains x)

let sequenceNotWithin seqA seqB = sequenceWithin seqA seqB |> not

let charsToString (chars: char array) = chars |> System.String

let split (char: char) (x: string) = x.Split(char)

let mapLine line =
    let input = line |> split '|' |> Seq.toArray

    let getDigits str =
        str
        |> split ' '
        |> Seq.filter (fun x -> x.Length > 0)
        |> Seq.map (fun x -> x.ToCharArray())

    let allDigits = getDigits input.[0]
    let outputDigits = getDigits input.[1]
    (allDigits, outputDigits)

let mapDigits (allDigitsEncoded: seq<char []>) =
    let findSingle predicate =
        allDigitsEncoded
        |> Seq.filter predicate
        |> Seq.exactlyOne

    let dig1 = findSingle (fun x -> Seq.length x = 2)
    let dig4 = findSingle (fun x -> Seq.length x = 4)
    let dig8 = findSingle (fun x -> Seq.length x = 7)
    let dig7 = findSingle (fun x -> Seq.length x = 3)

    let dig9 =
        findSingle (fun x ->
            Seq.length x = 6
            && sequenceWithin x dig1
            && sequenceWithin x dig4)

    let dig6 = findSingle (fun x -> Seq.length x = 6 && sequenceNotWithin x dig1)
    let dig3 = findSingle (fun x -> Seq.length x = 5 && sequenceWithin x dig1)
    let dig2 = findSingle (fun x -> Seq.length x = 5 && sequenceNotWithin dig9 x)
    let dig5 = findSingle (fun x -> Seq.length x = 5 && sequenceWithin dig9 x && sequenceNotWithin x dig1)
    let dig0 = findSingle (fun x -> Seq.length x = 6 && sequenceWithin x dig7 && sequenceNotWithin x dig5)

    [| dig0
       dig1
       dig2
       dig3
       dig4
       dig5
       dig6
       dig7
       dig8
       dig9 |]
    |> Array.mapi (fun i x ->
        x
        |> Array.sortBy id
        |> (fun s -> (charsToString s), i))
    |> Map.ofSeq

let decodeDigits (digitsToDecode: seq<char array>) (mapOfDigits: Map<string, int>) =
    let result =
        digitsToDecode
        |> Seq.mapi (fun i chars ->
            chars
            |> Array.sortBy id
            |> (fun s -> charsToString s))
        |> Seq.map (fun x -> mapOfDigits.[x])
        |> Seq.toArray

    result

let getInput () =
    (System.IO.File.ReadAllLines("data\\08.txt"))
    |> Seq.map mapLine

let answer1 () =
    getInput ()
    |> Seq.map (fun (digits, output) -> mapDigits digits |> decodeDigits output)
    |> Seq.toArray
    |> Seq.collect id
    |> Seq.filter (fun i -> i = 1 || i = 4 || i = 7 || i = 8)
    |> Seq.length

let answer2 () =
    getInput ()
    |> Seq.map (fun (digits, output) -> mapDigits digits |> decodeDigits output)
    |> Seq.toArray
    |> Seq.map (fun x -> x.[0] * 1000 + x.[1] * 100 + x.[2] * 10 + x.[3])
    |> Seq.sum