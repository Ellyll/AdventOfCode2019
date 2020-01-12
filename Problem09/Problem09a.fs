module Problem09a

type ParameterMode =
    | Immediate
    | Position
    | Relative

type State = { Memory: Map<int64,int64> ; Ptr: int64 ; RelativeBaseOffset: int64 ; InputBuffer: int64 list ; Output: int64 option ; IsHalted: bool }

type IntCodeProcessorStatus =
    | Running
    | Halted

type IntCodeProcessorMessage =
    | Input of int64
    | FetchStatus of AsyncReplyChannel<IntCodeProcessorStatus>
    | FetchState of AsyncReplyChannel<State>
    | Die

type OutputCollectorMessage =
    | Fetch of AsyncReplyChannel<int64 list>
    | Add of int64
    | Die


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
    | ' ' -> Position
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


let setValue value parameter parameterMode (state: State) =
    let location =
        match parameterMode with
        | Immediate -> failwithf "Unexpected Immediate parameter mode at Ptr=%d" state.Ptr
        | Position -> parameter
        | Relative -> state.RelativeBaseOffset + parameter
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
            let newState = setValue (a + b) m.[p+3L] (getParameterMode digits.[0]) state
            { newState with Ptr = p + 4L }
        | 2 -> // Multiply
            let a = getValue m.[p+1L] (getParameterMode digits.[2]) state
            let b = getValue m.[p+2L] (getParameterMode digits.[1]) state
            let newState = setValue (a * b) m.[p+3L] (getParameterMode digits.[0]) state
            { newState with Ptr = p + 4L }
        | 3 -> // Input
            match getInput state with
            | Some value, state' ->
                let state'' = setValue value m.[p+1L] (getParameterMode digits.[2]) state'
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
            let newState = setValue value m.[p+3L] (getParameterMode digits.[0]) state
            { newState with Ptr = p + 4L }
        | 8 -> // equals
            let p1 = getValue m.[p+1L] (getParameterMode digits.[2]) state
            let p2 = getValue m.[p+2L] (getParameterMode digits.[1]) state
            let value = if p1 = p2 then 1L else 0L
            let newState = setValue value m.[p+3L] (getParameterMode digits.[0]) state
            { newState with Ptr = p + 4L }
        | 9 -> // adjusts the relative base
            let p1 = getValue m.[p+1L] (getParameterMode digits.[2]) state
            let newRbo = state.RelativeBaseOffset+p1
            { state with RelativeBaseOffset = newRbo ; Ptr = p + 2L }
        | 99 -> // Halt
            { state with IsHalted = true }
        | _ ->
            failwithf "execution failed, invalid opcode %d at position %d" opCode p


let createIntCodeProcessor initialState outputHandler =
    MailboxProcessor.Start (fun inbox ->
        let rec loop state =
            async {
                let timeout = if state.IsHalted then 100 else 0
                let! msg = inbox.TryReceive timeout
                match msg with
                | Some m ->
                    match m with
                    | Input n -> return! loop { state with InputBuffer = state.InputBuffer @ [ n ] }
                    | FetchStatus reply ->
                        let status = if state.IsHalted then Halted else Running
                        reply.Reply status
                        return! loop state
                    | FetchState reply ->
                        reply.Reply state
                        return! loop state
                    | IntCodeProcessorMessage.Die -> return ()
                | None -> // No messages so continue execution as normal                    
                    let state'' = execute state
                    let state''' =
                        match state''.Output with
                        | Some n ->
                            outputHandler n
                            { state'' with Output = None }
                        | None -> state''                    
                    return! loop state'''
            }
        loop initialState
        )


let getResult inputs instructions =
    let outputCollector = MailboxProcessor.Start(fun inbox ->
        let rec loop outputs =
            async {
                let! msg = inbox.Receive()
                match msg with
                | OutputCollectorMessage.Fetch reply ->
                    reply.Reply (outputs |> List.rev)
                    return! loop outputs
                | OutputCollectorMessage.Add n ->
                    return! loop (n::outputs)
                | OutputCollectorMessage.Die ->
                    return ()
            }
        loop []
        )
    let initalState = { Memory = instructions ; Ptr = 0L ; RelativeBaseOffset = 0L; InputBuffer = inputs ; Output = None ; IsHalted = false }
    let processor = createIntCodeProcessor initalState (Add >> outputCollector.Post)
    let rec waitLoop () =
        if (processor.PostAndReply FetchStatus) = Halted then
            let outputs = outputCollector.PostAndReply Fetch
            processor.Post IntCodeProcessorMessage.Die
            outputCollector.Post OutputCollectorMessage.Die
            outputs
        else
            waitLoop ()
    waitLoop ()


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
