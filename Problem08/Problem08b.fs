module Problem08b

let parseImage (width: int) (height: int) (data: string) =
    let depth = (String.length data) / (width*height) // number of layers in data
    Array3D.init width height depth (fun x y z -> data.[ (z*width*height) + (y*width) + x])

let getData () =
    System.IO.File.ReadAllText("Problem08/Problem08.data").Trim()

let flatten image =
    Array3D.init (Array3D.length1 image) (Array3D.length2 image) 1 (fun x y _ ->
        let colour =
            seq { 0..(Array3D.length3 image)-1 }
            |> Seq.map (fun l -> image.[x,y,l])
            |> Seq.tryFind (fun c -> c = '0' || c = '1')
        match colour with
            | Some c ->c
            | None -> failwithf "Colour not found for (%d,%d)" x y
        )

let printImage image =
    for layer in 0..(Array3D.length3 image)-1 do
        for y in 0..(Array3D.length2 image)-1 do
            for x in 0..(Array3D.length1 image)-1 do
                printf "%s" (if image.[x,y,layer] = '0' then " " else "X")
            printfn ""
        printfn ""


let test () =
    printfn "No tests defined"

let run () =
    let image = (getData ()) |> parseImage 25 6
    let result = flatten image
    printImage result
    ()
