module Problem07a

type ParameterMode =
    | Immediate
    | Position

type State = { Memory: Map<int,int> ; Ptr: int ; Inputs: int list ; Output: int option }


let getData () =
    let data =
        System.IO.File.ReadAllText("Problem07/Problem07.data").Trim().Split ','
        |> Array.mapi (fun i inst -> i, System.Int32.Parse inst)
        |> Map.ofArray
    data


let getParameterMode (digit: char) =
    match digit with
    | '1' -> Immediate
    | ' '
    | '0' -> Position
    | _ -> failwithf "Unknown parameter mode: %c" digit


let getValue parameter parameterMode (state: State) =
    match parameterMode with
    | Immediate -> parameter
    | Position ->
        if not (Map.containsKey parameter state.Memory) then
            failwithf "getValue failed, location %d out of range, parameterMode=%A Ptr=%d, instructions=%A"
                parameter parameterMode state.Ptr
                [state.Memory.[state.Ptr] ; state.Memory.[state.Ptr+1] ; state.Memory.[state.Ptr+2] ; state.Memory.[state.Ptr+3] ]
        state.Memory.[parameter]


let setValue value location (state: State) =
    if not (Map.containsKey location state.Memory) then
        failwithf "setValue failed, location %d out of range when attempting to set to %d, Ptr=%d" location value state.Ptr

    let newMemory =
        state.Memory
        |> Map.remove location
        |> Map.add location value
    { state with Memory = newMemory }


let getInput state =
    match state.Inputs with
    | x::rest ->
        (Some x, { state with Inputs = rest })
    | _ -> None, state


let setOutput value (state: State) =
    { state with Output = Some value }


let rec execute (state: State) : State =
    let m = state.Memory        
    let p = state.Ptr

    let digits = sprintf "%5d" m.[p]
    let length = String.length digits
    let opCode = digits.[(length-2)..(length-1)] |> System.Int32.Parse
    match opCode with
    | 1 -> // Add
        let a = getValue m.[p+1] (getParameterMode digits.[2]) state
        let b = getValue m.[p+2] (getParameterMode digits.[1]) state
        let newState = setValue (a + b) m.[p+3] state
        execute { newState with Ptr = p + 4 }
    | 2 -> // Multiply
        let a = getValue m.[p+1] (getParameterMode digits.[2]) state
        let b = getValue m.[p+2] (getParameterMode digits.[1]) state
        let newState = setValue (a * b) m.[p+3] state
        execute { newState with Ptr = p + 4 }
    | 3 -> // Input
        let input, state' =
            match getInput state with
            | Some x, st -> x, st
            | None, _ -> failwithf "No input available for state %A" state
        let newState = setValue input m.[p+1] state'
        execute { newState with Ptr = p + 2 }
    | 4 -> // Output
        let value = getValue m.[p+1] (getParameterMode digits.[2]) state
        let newState = setOutput value state
        execute { newState with Ptr = p + 2 }
    | 5 -> // jump-if-true
        let p1 = getValue m.[p+1] (getParameterMode digits.[2]) state
        if p1 > 0 then
            let p2 = getValue m.[p+2] (getParameterMode digits.[1]) state
            execute { state with Ptr = p2 }
        else
            execute { state with Ptr = p + 3 }
    | 6 -> // jump-if-false
        let p1 = getValue m.[p+1] (getParameterMode digits.[2]) state
        if p1 = 0 then
            let p2 = getValue m.[p+2] (getParameterMode digits.[1]) state
            execute { state with Ptr = p2 }
        else
            execute { state with Ptr = p + 3 }
    | 7 -> // less then
        let p1 = getValue m.[p+1] (getParameterMode digits.[2]) state
        let p2 = getValue m.[p+2] (getParameterMode digits.[1]) state
        let value = if p1 < p2 then 1 else 0
        let newState = setValue value m.[p+3] state
        execute { newState with Ptr = p + 4 }
    | 8 -> // equals
        let p1 = getValue m.[p+1] (getParameterMode digits.[2]) state
        let p2 = getValue m.[p+2] (getParameterMode digits.[1]) state
        let value = if p1 = p2 then 1 else 0
        let newState = setValue value m.[p+3] state
        execute { newState with Ptr = p + 4 }
    | 99 -> // Halt
        state
    | n ->
        failwithf "execution failed, invalid opcode %d at position %d" opCode p


// from https://stackoverflow.com/a/2184129
let rec insertions x = function
    | []             -> [[x]]
    | (y :: ys) as l -> (x::l)::(List.map (fun x -> y::x) (insertions x ys))

let rec permutations = function
    | []      -> seq [ [] ]
    | x :: xs -> Seq.collect (insertions x) (permutations xs)


let getResult phaseSettingsPerumations instructions =
    //let phaseSettings = [ 4 ; 3 ; 2 ; 1 ; 0 ]
    phaseSettingsPerumations
    |> Seq.map(fun phaseSettings ->
            phaseSettings,
            phaseSettings
            |> List.fold (fun prevOutput phase ->
                            let state = execute { Memory = instructions ; Ptr = 0 ; Inputs = [ phase ; prevOutput ] ; Output = None }
                            match state.Output with
                            | Some x -> x
                            | None -> failwithf "Error, no output available for state %A" state
                            ) 0
            )
    |> Seq.maxBy (snd)
    |> snd

let test () =
    [
         [| 3;15;3;16;1002;16;10;16;1;16;15;15;4;15;99;0;0 |], seq { [ 4;3;2;1;0 ] }, 43210
    ]
    |> List.iter (fun (program, phaseSettings, expected) ->
        let instructions =
            program
            |> Array.mapi (fun i x -> i,x)
            |> Map.ofArray
        let result = getResult phaseSettings instructions
        if result <> expected then
            printfn "Test failed, %A should be %A but was %A" phaseSettings expected result
        else
            printfn "Test passed, %A was %A" phaseSettings expected
        )

let run () =
    let instructions = getData()
    let result = getResult (permutations [ 0;1;2;3;4 ]) instructions    
    printfn "Result: %A"  result
    ()
