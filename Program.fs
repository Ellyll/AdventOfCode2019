open System.Reflection

type Arguments =
    | Test of string
    | Run of string
    | Failure of string

type ExecutionResult =
    | Success
    | Failure of string

let execute moduleName methodName =
    let assembly = Assembly.GetExecutingAssembly()
    let methods =
        match assembly.GetTypes() |> Array.tryFind (fun t -> t.Name = moduleName) with
        | Some moduleType -> moduleType.GetMethods()
        | None -> [||]

    match methods with
    | [| |] -> Failure <| sprintf "No methods found for %s" moduleName
    | _ ->
        match methods |> Array.tryFind (fun method -> method.Name = methodName) with
        | Some method ->
            method.Invoke(null, [||]) |> ignore
            Success
        | None -> Failure <| sprintf "Method %s not found on module %s" methodName moduleName


let problems =
    [ 1..1 ]
    |> List.collect (fun n -> [ 'a' .. 'b' ] |> List.map (fun c -> sprintf "Problem%02d%c" n c))



[<EntryPoint>]
let main argv =

    let result =
        match argv with
        | [| str ; "test" |] when List.contains str problems ->
            execute str "test"
        | [| str ; "run" |] when List.contains str problems ->
            execute str "run"
        | [| str |] when List.contains str problems ->
            execute str "run"
        | [| |] ->
            execute (problems |> List.last) "run"
        | _ -> Failure "Invalid parameters"

    match result with
    | Success -> 0
    | Failure msg ->
        eprintfn "Failure: %s" msg
        1
