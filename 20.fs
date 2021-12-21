module day20

open utils

type Image = char [,]
type Background = char
type ImageData = Image * Background

let getInputParsed () =
    let inputLines =
        (System.IO.File.ReadAllLines("data\\20.txt"))
    let enhancmentAlg = inputLines.[0]
    let imageLines = inputLines.[2..]
    let imageMatrix =
        imageLines
        |> Seq.map (fun x -> x.ToCharArray())
        |> array2D
    (enhancmentAlg, (imageMatrix, '.'))

let growImage (image: ImageData) =
    let newLen = 2 + Array2D.length1 (fst image)
    let newColLen = 2 + Array2D.length2 (fst image)
    let newArray = Array2D.create newLen newColLen (snd image)
    (fst image)
    |> Array2D.iteri (fun x y _ -> Array2D.set newArray (x + 1) (y + 1) (fst image).[x, y])
    newArray

let getWithNeighbours position =
    let (x,y) = position
    [|
       (x-1, y-1); (x-1,y); (x-1,y+1);
       (x, y-1); (x,y); (x,y+1);
       (x+1,y-1);(x+1,y); (x+1,y+1)
    |]

let getDecimalCode (postion: Position) (imageData:ImageData) =
    let allPositions = getWithNeighbours postion
    let (grid, background) = imageData
    let mapped =
        allPositions
        |> Seq.map (fun x ->
            if withinBounds x grid then
                grid.[fst x, snd x]
            else
                background)
        |> Seq.map (fun x -> if x = '.' then '0' else '1')
        |> Seq.toArray

    let str = new string (mapped)
    System.Convert.ToInt32(str, 2)

let transformPosition (alg: string) position imageData =
    let algValues = alg.ToCharArray()
    let decimalCode = getDecimalCode position imageData
    algValues.[decimalCode]

let enhanceImage alg (image: ImageData) =
    let transformation = transformPosition alg
    let background = snd image
    let oldGrown = growImage image
    let newGrown =
        oldGrown
        |> Array2D.mapi (fun x y _ -> transformation (x, y) (oldGrown,background))

    let newBackground =
        match background with 
        | '.' -> alg.ToCharArray().[0]
        | '#' -> alg.ToCharArray().[511]

    (newGrown, newBackground)

let enhanceImageAndCountLitPixels img alg times =
    [ 1 .. times ]
    |> Seq.fold (fun (imageData) _ -> enhanceImage alg imageData) img
    |> fst
    |> Seq.cast<char>
    |> Seq.sumBy (fun c -> if c = '#' then 1 else 0)

let answer1 () =
    let (alg, image) = getInputParsed ()
    enhanceImageAndCountLitPixels image alg 2

let answer2 () =
    let (alg, image) = getInputParsed ()
    enhanceImageAndCountLitPixels image alg 50
