module Problem03b

type Path =
    | U of int
    | D of int
    | L of int
    | R of int

type Point =
    { x: int ; y: int }
    override this.ToString() = sprintf "(%d,%d)" this.x this.y

let origin = { x = 0 ; y = 0 }

let parseWire (line: string) =
    line.Split ","
    |> Array.map (fun s ->
            let a = s.[0]
            let b = System.Int32.Parse(s.[1..])
            match a with
            | 'U' -> U b
            | 'D' -> D b
            | 'L' -> L b
            | 'R' -> R b
            | c -> failwithf "Invalid character: %c" c
        )
    |> List.ofArray


let getData () = 
    let lines = System.IO.File.ReadAllLines "Problem03/Problem03.data"
    lines
    |> Array.map (parseWire)
    |> List.ofArray


let pathsToPoints (paths: Path list) : Point list =
    paths
    |> List.fold (fun (points, last) path ->
        let ps =
            match path with
            | U n -> [ 1..n ] |> List.map (fun i -> { last with y = last.y + i })
            | D n -> [ 1..n ] |> List.map (fun i -> { last with y = last.y - i })
            | R n -> [ 1..n ] |> List.map (fun i -> { last with x = last.x + i })
            | L n -> [ 1..n ] |> List.map (fun i -> { last with x = last.x - i })
            |> List.rev
        let newPoints = List.append ps points
        let newLast = List.head newPoints
        (newPoints, newLast)
    ) ([origin], origin)
    |> fst
    |> List.rev

let tryfindCrossingSteps (p: Point) (wire: Point list) =
    if p = origin then
        None
    else    
        let res = List.tryFindIndex (fun x -> (x = p)) wire
        res

let getResult wires =
    let rec inner crossings remainingWires =
        match remainingWires with
        | []
        | [_] -> crossings // all done!
        | curr::rest -> // split into current wire and remaining wires
            let newCrossings =
                curr
                |> List.mapi (fun i p ->
                    rest
                    |> List.choose(fun qs ->
                        match tryfindCrossingSteps p qs with
                        | Some n ->
                            Some ((i+n), p) // get combined steps for crossover
                        | None -> None
                        )
                    )
                |> List.collect (id)                                  
            inner (List.append newCrossings crossings) rest
    let crossings = inner [] wires
    crossings
    |> List.filter (fun (_,p) -> p <> origin)
    |> List.minBy (fst)
    |> fst

let test () =
    [
        ([ [ R 8;U 5;L 5;D 3] ; [ U 7;R 6;D 4;L 4 ] ], 30)
        ([ [ R 75; D 30; R 83; U 83; L 12; D 49; R 71; U 7; L 72 ] ; [ U 62; R 66; U 55; R 34; D 71; R 55; D 58; R 83 ] ], 610)
        ([ [ R 98; U 47; R 26; D 63; R 33; U 87; L 62; D 20; R 33; U 53; R 51 ] ; [ U 98; R 91; D 20; R 16; D 67; R 40; U 7; R 15; U 6; R 7 ] ], 410)
    ]
    |> List.iter (fun (wires, expected) ->
        let result = getResult (wires |> List.map (pathsToPoints))
        if result <> expected then
            printfn "Test failed, %A should be %A but was %A" wires expected result
        else
            printfn "Test passed, %A was %A" wires expected
    )

let run () =
    let wires = getData()
    let result = getResult (wires |> List.map (pathsToPoints))
    printfn "Result: %d" result
    ()