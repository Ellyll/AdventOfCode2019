module Problem02a

let getData () =
    let data = System.IO.File.ReadAllText("Problem02/Problem02.data").Trim().Split ',' |> Array.map (System.Int32.Parse)
    data.[1] <- 12
    data.[2] <- 2
    data

let execute (memory: int []) =
    let rec inner (m: int []) (p: int) : int [] =        
        let op = m.[p]
        match op with
        | 1 ->
            m.[m.[p+3]] <- (m.[m.[p+1]] + m.[m.[p+2]])
            inner m (p+4)
        | 2 ->
            m.[m.[p+3]] <- (m.[m.[p+1]] * m.[m.[p+2]])
            inner m (p+4)
        | 99 ->
            m
        | n ->
            failwithf "execution failed, invalid opcode %d at position %d" op p
    inner memory 0


let test () =
    [
        ([| 1;9;10;3;2;3;11;0;99;30;40;50 |], [| 3500;9;10;70;2;3;11;0;99;30;40;50 |])
        ([| 1;0;0;0;99 |], [| 2;0;0;0;99 |])
        ([| 2;3;0;3;99 |], [| 2;3;0;6;99 |])
        ([| 2;4;4;5;99;0 |], [| 2;4;4;5;99;9801 |])
        ([| 1;1;1;4;99;5;6;0;99 |], [| 30;1;1;4;2;5;6;0;99 |])
    ]
    |> List.iter (fun (start, expected) ->
        let result = execute start
        if result <> expected then
            printfn "Test failed, %A should be %A but was %A" start expected result
        else
            printfn "Test passed, %A was %A" start expected
    )


let run () =
    let newData = execute <| getData()
    printfn "Result: %d" newData.[0]
    ()
