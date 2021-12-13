module day12

open utils

type Cavern =
    | SmallCavern of string
    | LargeCavern of string

type Connection = Cavern * Cavern

let mapString (str: string) : Cavern =
    if System.Char.IsLower str.[0] then
        SmallCavern str
    else 
        LargeCavern str

let getInputAsConnections () =
    (System.IO.File.ReadAllLines("data\\12.txt"))
    |> Seq.map (fun x -> x.Split('-'))
    |> Seq.map (fun x -> x.[0],x.[1])
    |> Seq.map(fun (x,y) -> Connection (mapString x, mapString y))
    |> Seq.map(fun (x,y) -> seq { Connection (x,y) ; Connection (y,x) })
    |> Seq.collect id

let rec allWalks (availableConnections: Connection seq) (visitedCaverns: Cavern seq) (canVisitSmallTwice :bool)=
    let current = visitedCaverns |> Seq.last
    let visitedCavernsSet = visitedCaverns |> set
    let allPossibleConnections = 
        availableConnections
        |> Seq.filter (fun (x,_) ->  x = current)

    let canVisitAgain (pos: Cavern) =
        match pos with
        | SmallCavern _ -> canVisitSmallTwice || (Set.contains pos visitedCavernsSet |> not)
        | LargeCavern _ -> true

    if current = SmallCavern "end" || allPossibleConnections |> Seq.length = 0 then
        seq { visitedCaverns }
    else     
        let destonations =
            allPossibleConnections
            |> Seq.filter(fun (x,_) -> x = current)
            |> Seq.map snd
        let newAvailableConnections = 
                match current with
                | SmallCavern(_) when canVisitSmallTwice = false -> availableConnections |> Seq.filter(fun (x,y) -> canVisitAgain x && canVisitAgain y)
                | SmallCavern(_) when canVisitSmallTwice && current = SmallCavern "start" -> availableConnections |> Seq.filter(fun (x,y) -> x <> mapString "start" && y <> mapString "start")
                |_ -> availableConnections            
        let res =
            destonations
            |> Seq.map (fun x -> 
                let canVisitTwice = match x with 
                                    | SmallCavern _ -> canVisitSmallTwice && Set.contains x visitedCavernsSet |> not
                                    | LargeCavern _ -> canVisitSmallTwice
                (x,canVisitTwice))
            |> Seq.collect(fun (d,p) -> allWalks newAvailableConnections (Seq.append visitedCaverns (Seq.singleton d)) p)
        res;

let answer1 () =
    let cons = getInputAsConnections() |> Seq.toArray
    let start = seq {SmallCavern "start"}
    let allPossible = allWalks cons start false
    let allEnding =
        allPossible
        |> Seq.filter (fun x -> x |> Seq.last = mapString "end")
        |> Seq.toArray
    allEnding |> Seq.length

let answer2 () =
    let cons = getInputAsConnections() |> Seq.toArray
    let start = seq {SmallCavern "start"}
    let allPossible = allWalks cons start true
    let allEnding =
        allPossible
        |> Seq.filter (fun x -> x |> Seq.last = mapString "end")
    allEnding |> Seq.length