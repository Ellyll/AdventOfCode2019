module Problem07b

type ParameterMode =
    | Immediate
    | Position

type State = { Memory: Map<int,int> ; Ptr: int ; FetchInput: (unit->int) list ; Output: int option ; IsHalted: bool }


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


//let getInput state =
    //Seq.tryExactlyOne state.Inputs
    // match state.Inputs with
    // | x::rest ->
    //     (Some x, { state with Inputs = rest })
    // | _ -> None, state
let getInput state =
    match state.FetchInput with
    | [x] ->
        x (), state // Only one function left, so keep it and state doesn't change
    | x::rest ->
        x (), { state with FetchInput = rest }
    | [] -> failwithf "Unable to get input for state %A" state



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
            let a = getValue m.[p+1] (getParameterMode digits.[2]) state
            let b = getValue m.[p+2] (getParameterMode digits.[1]) state
            let newState = setValue (a + b) m.[p+3] state
            { newState with Ptr = p + 4 }
        | 2 -> // Multiply
            let a = getValue m.[p+1] (getParameterMode digits.[2]) state
            let b = getValue m.[p+2] (getParameterMode digits.[1]) state
            let newState = setValue (a * b) m.[p+3] state
            { newState with Ptr = p + 4 }
        | 3 -> // Input
            // let input, state' =
            //     match getInput state with
            //     | Some x, st -> x, st
            //     | None, _ -> failwithf "No input available for state %A" state
            // let newState = setValue input m.[p+1] state'
            let input, newState = getInput state
            { newState with Ptr = p + 2 }
        | 4 -> // Output
            let value = getValue m.[p+1] (getParameterMode digits.[2]) state
            let newState = setOutput value state
            { newState with Ptr = p + 2 }
        | 5 -> // jump-if-true
            let p1 = getValue m.[p+1] (getParameterMode digits.[2]) state
            if p1 > 0 then
                let p2 = getValue m.[p+2] (getParameterMode digits.[1]) state
                { state with Ptr = p2 }
            else
                { state with Ptr = p + 3 }
        | 6 -> // jump-if-false
            let p1 = getValue m.[p+1] (getParameterMode digits.[2]) state
            if p1 = 0 then
                let p2 = getValue m.[p+2] (getParameterMode digits.[1]) state
                { state with Ptr = p2 }
            else
                { state with Ptr = p + 3 }
        | 7 -> // less then
            let p1 = getValue m.[p+1] (getParameterMode digits.[2]) state
            let p2 = getValue m.[p+2] (getParameterMode digits.[1]) state
            let value = if p1 < p2 then 1 else 0
            let newState = setValue value m.[p+3] state
            { newState with Ptr = p + 4 }
        | 8 -> // equals
            let p1 = getValue m.[p+1] (getParameterMode digits.[2]) state
            let p2 = getValue m.[p+2] (getParameterMode digits.[1]) state
            let value = if p1 = p2 then 1 else 0
            let newState = setValue value m.[p+3] state
            { newState with Ptr = p + 4 }
        | 99 -> // Halt
            { state with IsHalted = true }
        | n ->
            failwithf "execution failed, invalid opcode %d at position %d" opCode p


// from https://stackoverflow.com/a/2184129
let rec insertions x = function
    | []             -> [[x]]
    | (y :: ys) as l -> (x::l)::(List.map (fun x -> y::x) (insertions x ys))

let rec permutations = function
    | []      -> seq [ [] ]
    | x :: xs -> Seq.collect (insertions x) (permutations xs)

let getResultForPhaseSetting (phaseSetting: int []) instructions =
    let getInput i (amps: State []) =
        match amps.[i].Output with
        | Some x -> x
        | None -> 0
    let rec amps =
        [|
            { Memory = instructions ; Ptr = 0 ; FetchInput = [ (fun () -> phaseSetting.[0]) ; (fun () -> getInput 4 amps) ] ; Output = None ; IsHalted = false }
            { Memory = instructions ; Ptr = 0 ; FetchInput = [ (fun () -> phaseSetting.[1]) ; (fun () -> getInput 0 amps) ] ; Output = None ; IsHalted = false }
            { Memory = instructions ; Ptr = 0 ; FetchInput = [ (fun () -> phaseSetting.[2]) ; (fun () -> getInput 1 amps) ] ; Output = None ; IsHalted = false }
            { Memory = instructions ; Ptr = 0 ; FetchInput = [ (fun () -> phaseSetting.[3]) ; (fun () -> getInput 2 amps) ] ; Output = None ; IsHalted = false }
            { Memory = instructions ; Ptr = 0 ; FetchInput = [ (fun () -> phaseSetting.[4]) ; (fun () -> getInput 3 amps) ] ; Output = None ; IsHalted = false }
        |]
    while (not (amps |> Array.forall (fun x -> x.IsHalted))) do
        for i in 0..4 do
            amps.[i] <- execute amps.[i]
    amps.[4].Output // Amp E

let getResult phaseSettingsPerumations instructions =
    //let phaseSettings = [ 4 ; 3 ; 2 ; 1 ; 0 ]
    phaseSettingsPerumations
    |> Seq.map(fun phaseSettings ->
            phaseSettings,
            match getResultForPhaseSetting (phaseSettings |> Seq.toArray) instructions with
            | Some x -> x
            | None -> failwithf "No output for phaseSettings %A" phaseSettings
            )
    |> Seq.maxBy (snd)
    |> snd

let test () =
    [
         [| 3;26;1001;26;-4;26;3;27;1002;27;2;27;1;27;26;27;4;27;1001;28;-1;28;1005;28;6;99;0;0;5 |], seq { [ 9;8;7;6;5] }, 139629729
         [| 3;52;1001;52;-5;52;3;53;1;52;56;54;1007;54;5;55;1005;55;26;1001;54;-5;54;1105;1;12;1;53;54;53;1008;54;0;55;1001;55;1;55;2;53;55;53;4;53;1001;56;-1;56;1005;56;6;99;0;0;0;0;10 |], seq { [ 9;7;8;5;6 ]}, 18216
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
