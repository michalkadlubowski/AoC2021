module day23

open utils

type AmphipodType = A | B | C | D
type AreaType= WAL | HAL | HNR | R of AmphipodType
type AmphipodState = Initial | AlreadyMoved | IsWhereItBelongs
type Amphipod = AmphipodType * AmphipodState
type AreaData = AreaType * Position  //* Amphipod option
type AmphipodData = Amphipod * AreaData

let board =
    [|
    [|HAL;HAL; HNR ;HAL;HNR;HAL;HNR;HAL;HNR;HAL;HAL|];
    [|WAL;WAL; R A ;WAL;R B;WAL;R C;WAL;R D;WAL;WAL|];
    [|WAL;WAL; R A ;WAL;R B;WAL;R C;WAL;R D;WAL;WAL|];
    [|WAL;WAL; R A ;WAL;R B;WAL;R C;WAL;R D;WAL;WAL|];
    [|WAL;WAL; R A ;WAL;R B;WAL;R C;WAL;R D;WAL;WAL|]
    |]
    |> array2D
    |> Array2D.mapi (fun x y r-> AreaData (r, Position (x,y)))

let burrowMap =
    let map = Map.empty<AmphipodType, Position seq>
    map
        |> Map.add A (seq { (1,2); (2,2); (3,2); (4,2) })
        |> Map.add B (seq { (1,4); (2,4); (3,4); (4,4)  })
        |> Map.add C (seq { (1,6); (2,6); (3,6); (4,6)  })
        |> Map.add D (seq { (1,8); (2,8); (3,8); (4,8)  })

let hallway = board |> array2DtoJagged |> Seq.head
let hallwayTargets = hallway |>  Seq.filter (fun (t,_) -> t <> HNR) |> Seq.toArray

let initialPositionsOfAmnipods =
    [|
    AmphipodData (Amphipod (D, AmphipodState.Initial), board.[1,2])
    AmphipodData (Amphipod (D, AmphipodState.Initial), board.[2,2])
    AmphipodData (Amphipod (D, AmphipodState.Initial), board.[3,2])
    AmphipodData (Amphipod (D, AmphipodState.Initial), board.[4,2])

    AmphipodData (Amphipod (A, AmphipodState.Initial), board.[1,4])
    AmphipodData (Amphipod (C, AmphipodState.Initial), board.[2,4])
    AmphipodData (Amphipod (B, AmphipodState.Initial), board.[3,4])
    AmphipodData (Amphipod (C, AmphipodState.Initial), board.[4,4])

    AmphipodData (Amphipod (C, AmphipodState.Initial), board.[1,6])
    AmphipodData (Amphipod (B, AmphipodState.Initial), board.[2,6])
    AmphipodData (Amphipod (A, AmphipodState.Initial), board.[3,6])
    AmphipodData (Amphipod (B, AmphipodState.Initial), board.[4,6])

    AmphipodData (Amphipod (A, AmphipodState.Initial), board.[1,8])
    AmphipodData (Amphipod (A, AmphipodState.Initial), board.[2,8])
    AmphipodData (Amphipod (C, AmphipodState.Initial), board.[3,8])
    AmphipodData (Amphipod (B, AmphipodState.Initial), board.[4,8])
    |]

let allGood (amphipods: Amphipod seq) =
    amphipods |> Seq.forall (fun (_,state) -> state = IsWhereItBelongs)

let mutable pathCache = Map.empty<Position * Position, Position list>
let getFulllPath (a:Position) (b:Position) =
    if pathCache |> Map.tryFind (a,b) |> Option.isSome then (pathCache |> Map.tryFind (a,b)).Value
    else 
        let mutable res =  List.empty<Position>
        if fst a > 0 then
            res <- List.append res ([0..fst a] |> List.rev |> List.map(fun x -> (x, snd a))) 
        if snd a <> snd b then
            let from = System.Math.Min(snd a, snd b)
            let dest = System.Math.Max(snd a, snd b)
            let mutable hallwayMoves = [from..dest] |> List.map (fun x -> (0,x))
            if snd a <> from then hallwayMoves <- hallwayMoves |> List.rev
            res <- List.append res hallwayMoves
        if fst b > 0 then
            res <- List.append res ([0..fst b] |> List.map(fun x -> (x, snd b))) 
        res <- res |> List.distinct
        //res <- res |> List.append res(List.singleton b)
        pathCache <- Map.add (a,b) res pathCache//|> Seq.toArray
        res

let isThereAPath (a:Position) (b:Position) (amphipods: AmphipodData seq) =
    let fullPath = getFulllPath a b
    let len = (fullPath |> Seq.length) - 2
    let pathWithoutStartAndEnd = fullPath |> Seq.skip 1 |> Seq.take len
    amphipods |> Seq.forall (fun (_,(_,pos)) -> pathWithoutStartAndEnd |> Seq.contains pos |> not)

let getDest (amphipod: AmphipodData) (amphipods: AmphipodData seq) = 
    let (t, state) = fst amphipod
    let possibleDests = burrowMap.[t]
    let occupants = amphipods |> Seq.filter (fun (_,(_,p)) -> possibleDests |> Seq.contains p)
    let sortedDests = possibleDests |> Seq.sortByDescending fst
    if Seq.isEmpty occupants then 
        sortedDests |> Seq.head |> Some
    else if occupants |> Seq.forall (fun ((otype,_),_) -> t = otype) then
        let occupantsPositions = occupants |> Seq.map (fun  (_,(_,p))-> p) |> Set.ofSeq
        sortedDests |> Seq.find (fun x -> occupantsPositions |> Set.contains x |> not) |> Some
    else None

let getAllPossibleMoves (amphipods: AmphipodData seq) = 
    let allInValidState = amphipods |> Seq.filter (fun ((_,state),_) -> state <> IsWhereItBelongs)
    let allAlreadyMoved = allInValidState |>  Seq.filter (fun ((_,state),_) -> state = AlreadyMoved)    
    let movesForAlreadyMoved = 
        allAlreadyMoved
        |> Seq.map (fun x -> (snd (snd x),getDest x amphipods))
        |> Seq.filter (fun (_,y) -> y |> Option.isSome)
        |> Seq.map (fun (x,y) -> (x, y.Value))
        |> Seq.filter (fun (x, y) -> isThereAPath x y amphipods)
        |> Seq.toArray
    if movesForAlreadyMoved.Length > 0 then
        movesForAlreadyMoved |> Array.take 1
    else
        let allInInitialState = allInValidState |>  Seq.filter (fun ((_,state),_) -> state = Initial)
        let amphiPodyPositions = amphipods |> Seq.map (fun (_,(_,pos)) -> pos) |> Set.ofSeq
        allInInitialState
                |> Seq.map(fun (_,(_,xPos)) -> hallwayTargets|> Seq.map (fun (_,hPos) -> (xPos, hPos)))
                |> Seq.collect id
                |> Seq.filter (fun (_,destPos) -> amphiPodyPositions |> Set.contains destPos |> not)
                |> Seq.filter (fun (xPos, hPos) -> isThereAPath xPos hPos amphipods)
                |> Seq.toArray

let makeMove (amphipods: AmphipodData seq) p1 p2 =
    let toMove = amphipods |> Seq.find (fun  (_,(_,pos)) -> pos = p1)
    let ((amphipodType,amphipodState),_) = toMove
    let path = getFulllPath p1 p2
    let pathLen = (path |> Seq.length) - 1
    let newState = if amphipodState = Initial then AlreadyMoved else IsWhereItBelongs
    let newType = fst board.[fst p2, snd p2]
    let moved = ((amphipodType, newState), (newType, p2))
    let newState = 
        amphipods
        |> Seq.map (fun x -> if x = toMove then moved else x )
    let costOfMove = 
        match amphipodType with
            | A -> 1
            | B -> 10
            | C -> 100
            | D -> 1000
    (newState, pathLen * costOfMove)

let mutable currentMin = System.Int32.MaxValue
let mutable deph0movesAnalysed = 0

let rec solve (amphipods: AmphipodData seq) score depth =
    if score >= currentMin then None else
    let possibleMoves = getAllPossibleMoves amphipods |> Seq.toArray
    let movesCount = possibleMoves.Length
    if allGood (amphipods |> Seq.map fst) then 
        currentMin <- score
        Some score
    elif movesCount = 0 then
         None
    else
        let score =
            possibleMoves
            |> Seq.toArray
            |> Array.Parallel.mapi (fun i (x,y) ->                                      
                let (afterMoveState, moveCost) = makeMove amphipods x y
                let newScore = score + moveCost
                let res = solve afterMoveState newScore (depth+1)
                if depth = 0 then
                    deph0movesAnalysed <- deph0movesAnalysed + 1
                    printfn "DONE %i/%i..." deph0movesAnalysed movesCount 
                res)
            |> Seq.map (fun s -> Option.defaultValue System.Int32.MaxValue s)
            |> Seq.min
        Some score


let answer1 () =
    let path = getFulllPath (0,10) (1,2)
    let test = getAllPossibleMoves initialPositionsOfAmnipods |> Seq.toArray
    let solved = solve initialPositionsOfAmnipods 0 0
    solved.Value

let answer2 () =
    0  