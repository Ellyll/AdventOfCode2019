module Problem08a

let parseImage (width: int) (height: int) (data: string) =
    let depth = (String.length data) / (width*height) // number of layers in data
    Array3D.init width height depth (fun x y z -> data.[ (z*width*height) + (y*width) + x])

let getData () =
    System.IO.File.ReadAllText("Problem08/Problem08.data").Trim()

let countDigitsOnLayer digit layer image =
    seq { 
            for y in 0..((Array3D.length2 image)-1) do
                for x in 0..((Array3D.length1 image)-1) do
                    yield (x,y)
        }
    |> Seq.fold (fun numberOfZeros (x,y) ->
            if image.[x,y,layer] = digit then
                numberOfZeros + 1
            else
                numberOfZeros                    
        ) 0

let getResult image =
    let layersWithZerosCounts =
        seq { 0..((Array3D.length3 image)-1) }
        |> Seq.map (fun layer ->
                layer,
                countDigitsOnLayer '0' layer image
            )
    let layer =
        layersWithZerosCounts
        |> Seq.minBy (snd)
        |> fst
    let numberOf1s = image |> countDigitsOnLayer '1' layer
    let numberOf2s = image |> countDigitsOnLayer '2' layer
    numberOf1s * numberOf2s

let test () =
    printfn "No tests defined"

let run () =
    let image = (getData ()) |> parseImage 25 6
    let result = getResult image
    printfn "Result: %A" result
    ()
