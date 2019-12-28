module Problem04b

let getData () =
    { 353096..843212 }
    |> Seq.map (sprintf "%d")


let containsAdjacentDigitsNotInGroup (digits: string) =
    let length = String.length digits
    if length < 2 then
        false
    else
        let rec inner i =
            if i >= length then
                false
            else
                if digits.[i-1] = digits.[i]
                    && (i = length-1 || digits.[i+1] <> digits.[i])
                    && (i < 2 || digits.[i-2] <> digits.[i]) then
                    true
                else
                    inner (i + 1)
        inner 1

let containsNeverDecreasingDigits (digits: string) =
    let length = String.length digits
    if length < 2 then
        true
    else
        let rec inner i =
            if i >= length then
                true
            else
                if System.Int32.Parse(digits.[i-1..i-1]) > System.Int32.Parse(digits.[i..i]) then
                    false
                else
                    inner (i + 1)
        inner 1

let isValidPassword (digits: string) =
    containsAdjacentDigitsNotInGroup digits && containsNeverDecreasingDigits digits

let getResult possiblePasswords =
    possiblePasswords
    |> Seq.sumBy (fun pass -> if isValidPassword pass then 1 else 0)

let test () =
    [
        ( "112233", true)
        ( "123444", false)
        ( "111122", true)
    ]
    |> List.iter (fun (input, expected) ->
        let result = isValidPassword input
        if result <> expected then
            printfn "Test failed, %A should be %A but was %A" input expected result
        else
            printfn "Test passed, %A was %A" input expected
    )


let run () =
    let possiblePasswords = getData()
    let result = getResult possiblePasswords
    printfn "Result: %d" result
    ()