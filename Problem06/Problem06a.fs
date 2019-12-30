module Problem06a


let countOrbits (orbits: Map<string, string list>) =
    let rec inner dist curr =
        dist + (Map.find curr orbits |> List.sumBy (fun body -> inner (dist+1) body))        
    inner 0 "COM"


let addOrbit (parentBody: string) (childBody: string) (orbits: Map<string, string list>) =
    if Map.containsKey parentBody orbits then
        let children = Map.find parentBody orbits
        orbits
        |> Map.remove parentBody
        |> Map.add parentBody (childBody::children)
    else
        orbits
        |> Map.add parentBody [ childBody ]



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
    |> List.collect (id)
    |> Set.ofList
    |> Set.fold (fun orbs body ->
        if Map.containsKey body orbs then
            orbs
        else
            orbs |> Map.add body []                
        ) orbits


let getData () =
    let text = System.IO.File.ReadAllText("Problem06/Problem06.data")
    parseOrbits text


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
K)L", 42 ]
    |> List.iter (fun (data, expected) ->
        let result = data |> parseOrbits |> countOrbits
        if result <> expected then
            printfn "Test failed, %A should be %A but was %A" data expected result
        else
            printfn "Test passed, %A was %A" data expected
    ) 

let run () =
    let orbits = getData ()
    let result = countOrbits orbits
    printfn "Result: %d" result
