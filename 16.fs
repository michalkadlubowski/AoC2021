module day16

open utils
open System

[<Literal>]
let BitsCountType = 0
[<Literal>]
let SubPacketsCountType = 1

type Packets = char array
type PacketVersion = int
type TypeId = int
type ResultingPacket = LiteralValuePacket of PacketVersion*int64 | OperatorPacket of TypeId * PacketVersion * ResultingPacket seq

let hexToBin s = 
    s 
    |> Seq.collect (fun singleChar -> 
        Convert.ToString(Convert.ToInt32(singleChar.ToString(), 16), 2).PadLeft(4,'0').ToCharArray())
    |> Seq.toArray

let binToDec s = System.Convert.ToInt64(s, 2)

let getInputParsed () = 
    let hexInput = (System.IO.File.ReadAllText("data\\16.txt"))
    let binInput = hexToBin hexInput |> Seq.toArray
    binInput

let rec extractLiteralValue (packets:Packets) =
    let isLastGroup = packets.[0] = '0'
    if isLastGroup then packets.[1..4] else
    Array.append packets.[1..4] (extractLiteralValue packets.[5..])

let rec parsePackets (packets:Packets) (packetsAcc:ResultingPacket seq) (takeCount: int option) =
    if packets |> Seq.length < 8 || takeCount.IsSome && (packetsAcc |> Seq.length = takeCount.Value) then 
        (packets, packetsAcc) 
    else
        let version = new string(packets.[0..2]) |> binToDec |> int // 3
        let packetType = new string(packets.[3..5]) |> binToDec |> int // 6
        match packetType with
        | 4 -> 
            let valuePackets = extractLiteralValue packets.[6..] 
            let realLength = valuePackets.Length/4 + valuePackets.Length + 6
            let decValue = binToDec (new string(valuePackets))
            let newAcc = Seq.append packetsAcc [LiteralValuePacket (version, decValue)]
            parsePackets packets.[realLength..] newAcc takeCount
        | typeId -> 
            let lenType = packets.[6] |> charToInt
            match lenType with
            | SubPacketsCountType ->
                let subPacketsCount = binToDec (new string(packets.[7..17])) |> int // 18
                let (remainingPackets, res) = parsePackets packets.[18..] Seq.empty<ResultingPacket> (Some subPacketsCount)
                if (res |> Seq.length <> subPacketsCount) then
                    "error"  |> printfn "%s" 
                let newAcc = Seq.append packetsAcc [OperatorPacket (typeId, version, res)]
                parsePackets remainingPackets newAcc takeCount
            | BitsCountType ->
                let bitsCount = binToDec (new string(packets.[7..21])) |> int // 22
                let (_, res) = parsePackets packets.[22..22+bitsCount-1] Seq.empty<ResultingPacket> None
                let newAcc = Seq.append packetsAcc [OperatorPacket (typeId, version, res)]
                parsePackets packets.[22+bitsCount..] newAcc takeCount        

let rec collectVersions resultingPackets =
    resultingPackets
    |> Seq.collect (fun x ->
        match x with
        | LiteralValuePacket (ver, value) -> seq {ver}
        | OperatorPacket (_, ver, packets) -> Seq.append (seq {ver}) (collectVersions packets))

let rec calculateSinglePacketValue (packet : ResultingPacket) =
    match packet with
    | LiteralValuePacket (_, value) -> value
    | OperatorPacket (typeId,_,packets) when typeId = 0 -> packets |> Seq.map calculateSinglePacketValue |> Seq.reduce (+)
    | OperatorPacket (typeId,_,packets) when typeId = 1 -> packets |> Seq.map calculateSinglePacketValue |> Seq.reduce (*)
    | OperatorPacket (typeId,_,packets) when typeId = 2 -> packets |> Seq.map calculateSinglePacketValue |> Seq.min
    | OperatorPacket (typeId,_,packets) when typeId = 3 -> packets |> Seq.map calculateSinglePacketValue |> Seq.max
    | OperatorPacket (typeId,_,packets) when typeId = 5 -> 
        let packetsCalculated = packets |> Seq.map calculateSinglePacketValue |> Seq.toArray
        if packetsCalculated.[0] > packetsCalculated.[1] then (1 |> int64) else (0 |> int64)
    | OperatorPacket (typeId,_,packets) when typeId = 6 -> 
        let packetsCalculated = packets |> Seq.map calculateSinglePacketValue |> Seq.toArray
        if packetsCalculated.[0] < packetsCalculated.[1] then (1 |> int64) else (0 |> int64)
    | OperatorPacket (typeId,_,packets) when typeId = 7 -> 
        let packetsCalculated = packets |> Seq.map calculateSinglePacketValue |> Seq.toArray
        if packetsCalculated.[0] = packetsCalculated.[1] then (1 |> int64) else (0 |> int64)

let answer1 () =
    let packets = getInputParsed ()
    parsePackets packets Seq.empty<ResultingPacket> None
    |> snd
    |> collectVersions
    |> Seq.sum

let answer2 () =
    let packets = getInputParsed ()
    let packet = 
        parsePackets packets Seq.empty<ResultingPacket> None
        |> snd
        |> Seq.head
    calculateSinglePacketValue packet