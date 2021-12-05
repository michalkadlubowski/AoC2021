[<AutoOpen>]
module utils

open System.IO

let readLines (filePath:string) = seq {
    use sr = new StreamReader (filePath)
    while not sr.EndOfStream do
        yield sr.ReadLine ()
}

let splitSeq (splitToken : string) input =
    let i = ref 0
    input |>
        Seq.map(fun x ->
                    if x = splitToken then i.Value <- i.Value + 1
                    i.Value, x) |>
        Seq.groupBy fst |>
        Seq.map(fun (_,x) -> Seq.map snd x |> Seq.filter (fun s -> s<> splitToken))
        
let array2DtoJagged<'a> (arr: 'a[,]) : 'a [][] = 
    [| for x in 0 .. Array2D.length1 arr - 1 do
            yield [| for y in 0 .. Array2D.length2 arr - 1 -> arr.[x, y] |]
        |]

let array2DtoJaggedColumns<'a> (arr: 'a[,]) : 'a [][] = 
    [| for x in 0 .. Array2D.length2 arr - 1 do
            yield [| for y in 0 .. Array2D.length1 arr - 1 -> arr.[y, x] |]
        |]        

let find2Dindex<'a when 'a:equality> (arr: 'a[,]) matchFn: Option<int*int> = 
    let rec go x y =
          if   y >= arr.GetLength 1 then None
          elif x >= arr.GetLength 0 then go 0 (y+1)
          elif matchFn arr.[x,y]   then Some (x,y)
          else go (x+1) y
    go 0 0