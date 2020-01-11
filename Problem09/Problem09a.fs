module Problem09a

type ParameterMode =
    | Immediate
    | Position
    | Relative

type State = { Memory: Map<int64,int64> ; Ptr: int64 ; RelativeBaseOffset: int64 ; InputBuffer: int64 list ; Output: int64 option ; IsHalted: bool }


let getData () =
    let data =
        System.IO.File.ReadAllText("Problem09/Problem09.data").Trim().Split ','
        |> Array.mapi (fun i inst -> int64 i, System.Int64.Parse inst)
        |> Map.ofArray
    data


let getParameterMode (digit: char) =
    match digit with
    | '2' -> Relative
    | '1' -> Immediate
    | ' '
    | '0' -> Position
    | _ -> failwithf "Unknown parameter mode: %c" digit


let getValueFromLocation (location: int64) state =
    if location < 0L then
        failwithf "Invalid negative memory location: %d" location
    match state.Memory |> Map.tryFind location with
        | Some x -> x
        | None -> 0L


let getValue parameter parameterMode (state: State) =
    match parameterMode with
    | Immediate -> parameter
    | Position -> getValueFromLocation parameter state
    | Relative -> getValueFromLocation (parameter+state.RelativeBaseOffset) state


let setValue value location (state: State) =
    if Map.containsKey location state.Memory then
        { state with Memory = state.Memory |> Map.remove location |> Map.add location value }
    else
        { state with Memory = state.Memory |> Map.add location value }


let getInput state =
    match state.InputBuffer with
    | [] -> None, state
    | x::rest ->
        Some x, { state with InputBuffer = rest }


let setOutput value (state: State) =
    { state with Output = Some value }


let execute (state: State) : State =
    if state.IsHalted then
        state
    else    
        let m = state.Memory        
        let p = state.Ptr

        let digits = sprintf "%5d" m.[p]
        let length = String.length digits
        let opCode = digits.[(length-2)..(length-1)] |> System.Int32.Parse
        match opCode with
        | 1 -> // Add
            let a = getValue m.[p+1L] (getParameterMode digits.[2]) state
            let b = getValue m.[p+2L] (getParameterMode digits.[1]) state
            let newState = setValue (a + b) m.[p+3L] state
            { newState with Ptr = p + 4L }
        | 2 -> // Multiply
            let a = getValue m.[p+1L] (getParameterMode digits.[2]) state
            let b = getValue m.[p+2L] (getParameterMode digits.[1]) state
            let newState = setValue (a * b) m.[p+3L] state
            { newState with Ptr = p + 4L }
        | 3 -> // Input
            match getInput state with
            | Some value, state' ->
                let state'' = setValue value m.[p+1L] state'
                { state'' with Ptr = p + 2L }
            | None, _ -> state
        | 4 -> // Output
            let value = getValue m.[p+1L] (getParameterMode digits.[2]) state
            let newState = setOutput value state
            { newState with Ptr = p + 2L }
        | 5 -> // jump-if-true
            let p1 = getValue m.[p+1L] (getParameterMode digits.[2]) state
            if p1 > 0L then
                let p2 = getValue m.[p+2L] (getParameterMode digits.[1]) state
                { state with Ptr = p2 }
            else
                { state with Ptr = p + 3L }
        | 6 -> // jump-if-false
            let p1 = getValue m.[p+1L] (getParameterMode digits.[2]) state
            if p1 = 0L then
                let p2 = getValue m.[p+2L] (getParameterMode digits.[1]) state
                { state with Ptr = p2 }
            else
                { state with Ptr = p + 3L }
        | 7 -> // less than
            let p1 = getValue m.[p+1L] (getParameterMode digits.[2]) state
            let p2 = getValue m.[p+2L] (getParameterMode digits.[1]) state
            let value = if p1 < p2 then 1L else 0L
            let newState = setValue value m.[p+3L] state
            { newState with Ptr = p + 4L }
        | 8 -> // equals
            let p1 = getValue m.[p+1L] (getParameterMode digits.[2]) state
            let p2 = getValue m.[p+2L] (getParameterMode digits.[1]) state
            let value = if p1 = p2 then 1L else 0L
            let newState = setValue value m.[p+3L] state
            { newState with Ptr = p + 4L }
        | 9 -> // adjusts the relative base
            let p1 = getValue m.[p+1L] (getParameterMode digits.[2]) state
            { state with RelativeBaseOffset = state.RelativeBaseOffset+p1 ; Ptr = p + 2L }
        | 99 -> // Halt
            { state with IsHalted = true }
        | _ ->
            failwithf "execution failed, invalid opcode %d at position %d" opCode p

let getResult inputs instructions =
    let rec runProgram outputs state =
        if state.IsHalted then
            outputs
        else
            let state' = execute state        
            let outputs' =
                match state'.Output with
                | Some x -> x::outputs
                | None -> outputs
            runProgram outputs' { state' with Output = None }
    runProgram [] { Memory = instructions ; Ptr = 0L ; RelativeBaseOffset = 0L; InputBuffer = inputs ; Output = None ; IsHalted = false }
    |> List.rev


let test () =
    [
         [ 109L;1L;204L;-1L;1001L;100L;1L;100L;1008L;100L;16L;101L;1006L;101L;0L;99L ], [], [ 109L;1L;204L;-1L;1001L;100L;1L;100L;1008L;100L;16L;101L;1006L;101L;0L;99L ]
         [ 1102L;34915192L;34915192L;7L;4L;7L;99L;0L ], [], [ 1219070632396864L ]
         [ 104L;1125899906842624L;99L ], [], [ 1125899906842624L ]
    ]
    |> List.iter (fun (program, inputs, expected) ->
        let instructions =
            program
            |> List.mapi (fun i x -> int64 i, int64 x)
            |> Map.ofList
        let result = getResult inputs instructions
        if result <> expected then
            printfn "Test failed, %A should be %A but was %A" program expected result
        else
            printfn "Test passed, %A was %A" program expected
        )

let run () =
    let instructions = getData()
    let result = getResult [ 1L ] instructions
    printfn "Result: %A"  result
    ()