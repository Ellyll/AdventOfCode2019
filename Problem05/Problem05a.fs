module Problem05a

type ParameterMode =
    | Immediate
    | Position

type State = { Memory: Map<int,int> ; Ptr: int ; Output: int option }


let getData () =
    let data =
        System.IO.File.ReadAllText("Problem05/Problem05.data").Trim().Split ','
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


let getInput () =
    1


let setOutput value (state: State) =
    { state with Output = Some value }


let rec execute (state: State) : State =
    let m = state.Memory        
    let p = state.Ptr

    let digits = sprintf "%5d" m.[p]
    let length = String.length digits
    let opCode = digits.[(length-2)..(length-1)]
    match opCode with
    | " 1"
    | "01" -> // Add
        let a = getValue m.[p+1] (getParameterMode digits.[2]) state
        let b = getValue m.[p+2] (getParameterMode digits.[1]) state
        let newState = setValue (a + b) m.[p+3] state
        execute { newState with Ptr = p + 4 }
    | " 2"        
    | "02" -> // Multiply
        let a = getValue m.[p+1] (getParameterMode digits.[2]) state
        let b = getValue m.[p+2] (getParameterMode digits.[1]) state
        let newState = setValue (a * b) m.[p+3] state
        execute { newState with Ptr = p + 4 }
    | " 3"
    | "03" -> // Input
        let newState = setValue (getInput ()) m.[p+1] state
        execute { newState with Ptr = p + 2 }
    | " 4"
    | "04" -> // Output
        let value = getValue m.[p+1] (getParameterMode digits.[2]) state
        let newState = setOutput value state
        execute { newState with Ptr = p + 2 }
    | "99" ->
        state
    | n ->
        failwithf "execution failed, invalid opcode %s at position %d" opCode p


let test () =
    printfn "No tests defined"

let run () =
    let instructions = getData()
    let finalState = execute { Memory = instructions ; Ptr = 0 ; Output = None }    
    printfn "Result: %A"  finalState.Output
    ()
