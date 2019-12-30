module Problem06b


let getPathToBody (name: string) (orbits: Map<string, Set<string>>) =
    let rec inner path body =
        if body = "COM" then
            "COM"::path
        else
            let parent = orbits |> Map.findKey (fun _ children -> children |> Set.contains body)
            inner (body::path) parent
    inner [] name


let getPathBetweenBodies a b orbits =
    if a = b then // same body
        []
    else    
        let pathA = getPathToBody a orbits
        let pathB = getPathToBody b orbits
        let rec inner (remA: string list) (remB: string list) =
            match remA,remB with
            | [c], [d] when c = d ->
                [c ; d]
            | c::d::restA, e::f::restB when c = e && d <> f ->
                (d::restA |> List.rev) @ (e::f::restB)
            | c::d::restA, e::f::restB when c = e && d = f ->
                inner (d::restA) (f::restB)
            | _ -> failwithf "Path not found between %s and %s" a b
        inner pathA pathB


let getNumberOfOrbitsToTransfer a b orbits =
    let path = getPathBetweenBodies a b orbits
    List.length path - 3


let countOrbits (orbits: Map<string, Set<string>>) =
    let rec inner dist curr =
        dist + (Map.find curr orbits |> Set.toList |> List.sumBy (fun body -> inner (dist+1) body))        
    inner 0 "COM"


let addOrbit (parentBody: string) (childBody: string) (orbits: Map<string, Set<string>>) =
    if Map.containsKey parentBody orbits then
        let children = Map.find parentBody orbits
        orbits
        |> Map.remove parentBody
        |> Map.add parentBody (Set.add childBody children)
    else
        orbits
        |> Map.add parentBody (Set.singleton childBody)


let parseOrbits (text: string) =
    let lines = text.Split "\n"
    let orbits =
        lines
        |> Array.fold (fun orbs line ->
            let items = line.Split ")"
            addOrbit items.[0] items.[1] orbs
            ) Map.empty
    // Add orbits with no children
    orbits
    |> Map.toList
    |> List.map (snd)
    |> List.collect (Set.toList)
    |> Set.ofList
    |> Set.fold (fun orbs body ->
        if Map.containsKey body orbs then
            orbs
        else
            orbs |> Map.add body Set.empty                
        ) orbits


let getData () =
    let text = System.IO.File.ReadAllText("Problem06/Problem06.data")
    parseOrbits text

let getResult orbits =
    getNumberOfOrbitsToTransfer "YOU" "SAN" orbits


let test () =
    [
    @"COM)B
B)C
C)D
D)E
E)F
B)G
G)H
D)I
E)J
J)K
K)L
K)YOU
I)SAN", 4 ]
    |> List.iter (fun (data, expected) ->
        let result = data |> parseOrbits |> getResult
        if result <> expected then
            printfn "Test failed, %A should be %A but was %A" data expected result
        else
            printfn "Test passed, %A was %A" data expected
    ) 

let run () =
    let orbits = getData ()
    let result = getResult orbits
    printfn "Result: %d" result
