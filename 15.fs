module day15

open utils
open System
open FSharpx.Collections

[<CustomComparison; CustomEquality>]
type WayPoint2 =
    { Position : Position; CostOfEdge: int; TotalCost : int option}
    interface IEquatable<WayPoint2> with
        member this.Equals other = other.Position.Equals this.Position && other.TotalCost.Equals this.TotalCost

    override this.Equals other =
        match other with
        | :? WayPoint2 as p -> (this :> IEquatable<_>).Equals p
        | _ -> false

    interface IComparable with
        member this.CompareTo other =
            match other with
            | :? WayPoint2 as p -> (this :> IComparable<_>).CompareTo p
            | _ -> -1

    interface IComparable<WayPoint2> with
        member this.CompareTo other = other.TotalCost.Value.CompareTo this.TotalCost.Value


let getInputParsed () =
    (System.IO.File.ReadAllLines("data\\15.txt"))
    |> Seq.toArray
    |> Seq.map (fun x-> x.ToCharArray() |> Seq.map (fun x-> (charToInt x)))
    |> array2D
    |> Array2D.mapi (fun x y cost->  {  Position = Position (x,y); TotalCost = None; CostOfEdge = cost })   

let getTransformedInputParsed () =
    let input = getInputParsed()
    let l1 = Array2D.length1 input
    let l2 = Array2D.length2 input
    let resArray = Array2D.init (5*l1) (5*l2) (fun x y ->
        let oldX = x%l1
        let oldY = y%l2
        let xModifier = x/l1
        let yModifier = y/l1
        let values = [1..9]
        let res = values.[(((input.[oldX,oldY].CostOfEdge + xModifier + yModifier)-1)  % 9)]
        res)
    resArray
    |> Array2D.mapi (fun x y cost->  {  Position = Position (x,y); TotalCost = None; CostOfEdge = cost })   


let heyDJFindPathCost (arr : WayPoint2[,]) =
    let goal = Position ((arr |> Array2D.length1) - 1, (arr |> Array2D.length2) - 1)

    let mutable frontier = PriorityQueue.empty<WayPoint2> true
    let start = {  Position = Position (0,0); TotalCost = Some(0); CostOfEdge = arr.[0,0].CostOfEdge}
    frontier <- (PriorityQueue.insert start frontier)
    let mutable cameFrom =  Map.empty<Position, Option<Position>>
    let mutable costSoFar = Map.empty<Position, int>
    cameFrom <- (cameFrom |> Map.add start.Position None)
    costSoFar <- (costSoFar |> Map.add start.Position 0)
    while frontier |> PriorityQueue.isEmpty |> not do
        let (current, newFrontier) = frontier.Pop()
        let currentPosition = current.Position
        frontier <- newFrontier
        if currentPosition = goal then 
            frontier <- PriorityQueue.empty<WayPoint2> true
        else
            getNeighbouringTilesWithoutDiagonals currentPosition arr
                |> Array.iter(fun next ->
                    let costOfEdge = (arr.[fst next, snd next]).CostOfEdge
                    let newCost = costSoFar.[currentPosition] + costOfEdge
                    if costSoFar.ContainsKey(next) |> not || newCost < costSoFar.[next] then
                        costSoFar <- (costSoFar |> Map.add next newCost)
                        frontier <- frontier.Insert { Position = next; TotalCost = Some(newCost); CostOfEdge = costOfEdge}
                        cameFrom <- (cameFrom |> Map.add next (Some currentPosition))
                    else 
                        ())
    costSoFar.[goal]

let answer1 () =
    heyDJFindPathCost (getInputParsed ())

let answer2 () =
    heyDJFindPathCost (getTransformedInputParsed ())
