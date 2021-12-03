module day3

open utils

let parse (str : string) =
    let inline charToInt c = int c - int '0'
    str.ToCharArray()
        |> Seq.map(fun x -> x |> charToInt)
        |> Seq.toArray

let getInputAs2DArray = 
    readLines("data\\03.txt")
        |> Seq.map parse
        |> Seq.toArray
        |> array2D

let getMostCommonByColumn (arr: int[,]) =
    let rowsCount = arr |> Array2D.length1
    let colsCount = arr |> Array2D.length2
    [0..colsCount - 1]
        |> Seq.map (fun i -> arr.[*,i])
        |> Seq.map (fun i -> i |> Seq.sum)
        |> Seq.map (fun x -> (System.Math.Round((float x)/(float rowsCount), System.MidpointRounding.AwayFromZero)))
        |> Seq.map (fun x -> System.Convert.ToInt32(x))

let getLeastCommonByColumn (arr: int[,]) = 
    getMostCommonByColumn arr
        |> Seq.map(fun i -> if i = 1 then 0 else 1)

let rec eliminateBySelector selectorFn colNo (arr: int[,]) =
    if (Array2D.length1 arr = 1) 
        then arr.[0,*]
    else
        let mostCommonByColumn = selectorFn arr
        let colFilter = Seq.item colNo mostCommonByColumn
        let filtered = arr 
                            |> array2DtoJagged
                            |> Array.filter (fun x -> x.[colNo] = colFilter)
                            |> array2D
        eliminateBySelector selectorFn (colNo+1) filtered

let eliminateByMostCommon colNo (arr: int[,]) = eliminateBySelector getMostCommonByColumn colNo arr
let eliminateByLeastCommon colNo (arr: int[,]) = eliminateBySelector getLeastCommonByColumn colNo arr

let bitIntArrToNum arr =
    let convert s =  System.Convert.ToInt32(s, 2)    
    arr
        |> Seq.fold (fun x y -> x + y.ToString()) ""
        |> convert

let answer1 =     
    let gammaRate = getInputAs2DArray 
                            |> getMostCommonByColumn 
                            |> bitIntArrToNum        
    let epsilonRate = getInputAs2DArray
                        |> getLeastCommonByColumn
                        |> bitIntArrToNum
    gammaRate * epsilonRate

let answer2 = 
    let oxygenGeneratorRating = getInputAs2DArray |> eliminateByMostCommon 0 |> bitIntArrToNum
    let co2ScrubberRating = getInputAs2DArray |> eliminateByLeastCommon 0 |> bitIntArrToNum
    oxygenGeneratorRating * co2ScrubberRating