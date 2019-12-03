open System.Reflection


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
    let assembly = Assembly.GetExecutingAssembly()
    assembly.GetTypes()
        |> Array.filter (fun t -> t.Name.StartsWith("Problem"))
        |> Array.map (fun t -> t.Name)
        |> Set.ofArray


[<EntryPoint>]
let main argv =
    printfn "Problems loaded: %s" <| System.String.Join(",", problems)
    let result =
        match argv with
        | [| str ; "test" |] when Set.contains str problems ->
            execute str "test"
        | [| str ; "run" |] when Set.contains str problems ->
            execute str "run"
        | [| str |] when Set.contains str problems ->
            execute str "run"
        | [| |] ->
            execute (problems |> Set.maxElement) "run"
        | _ -> Failure <| sprintf "Invalid parameters: %A" argv
    match result with
    | Success -> 0
    | Failure msg ->
        eprintfn "Failure: %s" msg
        1
