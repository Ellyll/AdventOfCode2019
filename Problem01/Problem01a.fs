module Problem01a

let getData () =
    System.IO.File.ReadAllLines "Problem01/Problem01.data"
    |> Array.map (System.Int32.Parse)

(*
    Fuel required to launch a given module is based on its mass. Specifically,
    to find the fuel required for a module, take its mass, divide by three,
    round down, and subtract 2.
*)
let calcFuleRequired mass =
    ((float mass / 3.0) |> System.Math.Floor) - 2.0
    |> int

let test () =
    [
        (12, 2)
        (14, 2)
        (1969, 654)
        (100756, 33583)
    ]
    |> List.iter (fun (mass, expected) ->
        let result = calcFuleRequired mass
        if result <> expected then
            printfn "Test failed, %d should be %d but was %d" mass expected result
        else
            printfn "Test passed, %d was %d" mass expected
        )


let run () =
    //test ()
    let data = getData ()
    let sum =
        data
        |> Array.map (calcFuleRequired)
        |> Array.reduce (+)
    printfn "Sum: %d" sum
    printfn "Problem 01 Completed at %s" <| System.DateTime.Now.ToString()