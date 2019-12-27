module Problem03a

type Path =
| U of int
| D of int
| L of int
| R of int

type Point =
    { x: int ; y: int }
    override this.ToString() = sprintf "(%d,%d)" this.x this.y

type LineSegment =
    { p1: Point ; p2: Point }
    override this.ToString() = sprintf "(%s,%s)" (this.p1.ToString()) (this.p2.ToString())

type Intersection =
    | LineSegment of LineSegment
    | Point of Point

// a: first bounding box, b: second bounding box
let doBoundingBoxesIntersect (a: Point[]) (b: Point[]) =
    a.[0].x <= b.[1].x 
    && a.[1].x >= b.[0].x 
    && a.[0].y <= b.[1].y
    && a.[1].y >= b.[0].y

let crossProduct (a: Point) (b: Point) =
    (a.x * b.y) - (b.x * a.y)

let isPointOnLine (a: LineSegment) (b: Point) =
    let aTmp = { p1 = { x=0 ; y=0 }; p2 = { x =  a.p2.x - a.p1.x ; y = a.p2.y - a.p1.y } }
    let bTmp = { x = b.x - a.p1.x ; y = b.y - a.p1.y };
    let r = crossProduct aTmp.p2 bTmp;
    abs r = 0

let isPointRightOfLine (a: LineSegment) (b: Point) =
    let aTmp = { p1 = { x=0 ; y=0 } ; p2 = { x = a.p2.x - a.p1.x ; y = a.p2.y - a.p1.y } }
    let bTmp = { x = b.x - a.p1.x ; y = b.y - a.p1.y }
    crossProduct aTmp.p2 bTmp < 0

let lineSegmentTouchesOrCrossesLine (a: LineSegment) (b: LineSegment) =
    isPointOnLine a b.p1
    || isPointOnLine a b.p2
    || isPointRightOfLine a b.p1 
    || isPointRightOfLine a b.p2

let getBoundingBox (a: LineSegment) =
    [|
        { x = min a.p1.x a.p2.x ; y = min a.p1.y a.p2.y }
        { x = max a.p1.x a.p2.x ; y = max a.p1.y a.p2.y }
    |]

let doLinesIntersect (a: LineSegment) (b: LineSegment) =
    let box1 = getBoundingBox a
    let box2 = getBoundingBox b
    doBoundingBoxesIntersect box1 box2
        && lineSegmentTouchesOrCrossesLine a b
        && lineSegmentTouchesOrCrossesLine b a

let getIntersection (a: LineSegment) (b: LineSegment) : Intersection =
    // From: https://martin-thoma.com/how-to-check-if-two-line-segments-intersect/
    (* the intersection [(x1,y1), (x2, y2)]
       it might be a line or a single point. If it is a line,
       then x1 = x2 and y1 = y2.  *)
    //var x1, y1, x2, y2;
    let intersection =
        if a.p1.x = a.p2.x then
            // Case (A)
            // As a is a perfect vertical line, it cannot be represented
            // nicely in a mathematical way. But we directly know that
            //
            let x1 = a.p1.x
            let x2 = x1;
            let (y1, y2) =
                if b.p1.x = b.p2.x then
                    // Case (AA): all x are the same!
                    // Normalize
                    let (aNorm, bNorm) =
                        let a1 = if a.p1.y > a.p2.y then { p1 = a.p2 ; p2 = a.p1 } else a
                        let b2 = if b.p1.y > b.p2.y then { p1 = b.p2 ; p2 = b.p1 } else b
                        if a1.p1.y > b.p1.y then
                            (b2, a1)
                        else
                            (a1, b2)               

                    // Now we know that the y-value of a["first"] is the 
                    // lowest of all 4 y values
                    // this means, we are either in case (AAA):
                    //   a: x--------------x
                    //   b:    x---------------x
                    // or in case (AAB)
                    //   a: x--------------x
                    //   b:    x-------x
                    // in both cases:
                    // get the relavant y intervall
                    (bNorm.p1.y, min aNorm.p2.y bNorm.p2.y)
                else
                    // Case (AB)
                    // we can mathematically represent line b as
                    //     y = m*x + t <=> t = y - m*x
                    // m = (y1-y2)/(x1-x2)
                    //var m, t;
                    let m = (b.p1.y - b.p2.y) /
                            (b.p1.x - b.p2.x)
                    let t = b.p1.y - m*b.p1.x
                    let y = m*x1 + t
                    (y,y)
            { p1 = { x=x1 ; y=y1 } ; p2 = { x=x2 ; y=y2 } }

        elif b.p1.x = b.p2.x then
            // Case (B)
            // essentially the same as Case (AB), but with
            // a and b switched
            let x1 = b.p1.x
            let x2 = x1

            let a1 = b;
            let b1 = a;

            let m = (b1.p1.y - b1.p2.y) /
                    (b1.p1.x - b1.p2.x)
            let t = b1.p1.y - m*b1.p1.x
            let y1 = m*x1 + t;
            let y2 = y1
            { p1 = { x=x1 ; y=y1 } ; p2 = { x=x2 ; y=y2 } }

        else
            // Case (C)
            // Both lines can be represented mathematically
            //var ma, mb, ta, tb;
            let ma = (a.p1.y - a.p2.y) /
                     (a.p1.x - a.p2.x)
            let mb = (b.p1.y - b.p2.y) /
                     (b.p1.x - b.p2.x)
            let ta = a.p1.y - ma*a.p1.x
            let tb = b.p1.y - mb*b.p1.x

            if ma = mb then
                // Case (CA)
                // both lines are in parallel. As we know that they 
                // intersect, the intersection could be a line
                // when we rotated this, it would be the same situation 
                // as in case (AA)

                // Normalize
                let aTmp = if a.p1.x > a.p2.x then { p1 = a.p2 ; p2 = a.p1 } else a
                let bTmp = if b.p1.x > b.p2.x then { p1 = b.p2 ; p2 = b.p1 } else b
                let (aNorm, bNorm) = 
                    if aTmp.p1.x > aTmp.p1.x then
                        bTmp, aTmp
                    else
                        aTmp, bTmp                    
                
               
                // get the relavant x intervall
                let x1 = bNorm.p1.x
                let x2 = min aNorm.p2.x bNorm.p2.x
                let y1 = ma*x1+ta
                let y2 = ma*x2+ta
                { p1 = { x=x1 ; y=y1 } ; p2 = { x=x2 ; y=y2 } }
            else
                // Case (CB): only a point as intersection:
                // y = ma*x+ta
                // y = mb*x+tb
                // ma*x + ta = mb*x + tb
                // (ma-mb)*x = tb - ta
                // x = (tb - ta)/(ma-mb)
                let x1 = (tb-ta)/(ma-mb)
                let y1 = ma*x1+ta
                let x2 = x1
                let y2 = y1
                let point = { x = x1 ; y = y1 }
                { p1 = point; p2 = point }
    if intersection.p1 = intersection.p2 then
        Point intersection.p1
    else
        LineSegment intersection


let getManhattanDistance p1 p2 =
    (abs (p1.x - p2.x)) + (abs (p1.y - p2.y))

let pathsToLines (wire: Path list) : LineSegment list =
    let ls =
        wire
        |> List.fold (fun (lines) path ->
            let line =
                match lines with
                | [] -> { p1 = { x = 0 ; y = 0 } ; p2 = { x = 0 ; y = 0 } }
                | l::_ -> l
            match path with
            | U n -> { p1 = { x = line.p2.x ; y = line.p2.y } ; p2 = { x = line.p2.x ; y = line.p2.y + n }}::lines
            | D n -> { p1 = { x = line.p2.x ; y = line.p2.y } ; p2 = { x = line.p2.x ; y = line.p2.y - n }}::lines
            | L n -> { p1 = { x = line.p2.x ; y = line.p2.y } ; p2 = { x = line.p2.x - n ; y = line.p2.y }}::lines
            | R n -> { p1 = { x = line.p2.x ; y = line.p2.y } ; p2 = { x = line.p2.x + n ; y = line.p2.y }}::lines
            ) ([])
        |> List.rev
    ls

let getWireCrossings (wires: Path list list) =
    let wiresAsSegments = wires |> List.map (pathsToLines)
    let rec inner intersections remainingWires =
        match remainingWires with
        | []
        | [_] -> intersections // all done!
        | curr::rest -> // split into current wire and remaining wires
            let lineSegments = rest |> List.collect (id) // Combine remaining wires into one list of the line segments
            let currIntersections =
                curr
                |> List.fold (fun state w ->
                    let newIntersections =
                        lineSegments                        
                        |> List.choose (fun l ->
                            if doLinesIntersect w l then
                                match getIntersection w l with
                                | Point p when p <> { x=0 ; y=0 } -> Some p // Keep crossovers, exclude origin
                                | _ -> None // Ignore overlaps and origin
                            else
                                None                        
                        )
                    List.append newIntersections state
                    ) []
            inner (List.append currIntersections intersections) rest
    inner [] wiresAsSegments


let getClosestCrossing (wires: (Path list) list) =
    let origin = { x = 0 ; y = 0 }
    let crossings = getWireCrossings wires

    crossings
    |> List.map (fun p -> getManhattanDistance origin p )
    |> List.min


let getData () = 
    let lines = System.IO.File.ReadAllLines("Problem03/Problem03.data")
    lines
    |> Array.map (fun line ->
            line.Split ","
            |> Array.map (fun s ->
                    let a = s.[0]
                    let b = System.Int32.Parse(s.[1..])
                    match a with
                    | 'U' -> U b
                    | 'D' -> D b
                    | 'L' -> L b
                    | 'R' -> R b
                    | c -> failwithf "Invalid character: %c" c
                )
            |> List.ofArray
        )
    |> List.ofArray


let test () =
    [
        ([ [ R 8;U 5;L 5;D 3] ; [ U 7;R 6;D 4;L 4 ] ], 6)
        ([ [ R 75; D 30; R 83; U 83; L 12; D 49; R 71; U 7; L 72 ] ; [ U 62; R 66; U 55; R 34; D 71; R 55; D 58; R 83 ] ], 159)
        ([ [ R 98; U 47; R 26; D 63; R 33; U 87; L 62; D 20; R 33; U 53; R 51 ] ; [ U 98; R 91; D 20; R 16; D 67; R 40; U 7; R 15; U 6; R 7 ] ], 135)
    ]
    |> List.iter (fun (wires, expected) ->
        let result = getClosestCrossing wires
        if result <> expected then
            printfn "Test failed, %A should be %A but was %A" wires expected result
        else
            printfn "Test passed, %A was %A" wires expected
    )


let run () =
    let wires = getData()
    let result = getClosestCrossing wires
    printfn "Result: %d" result
    ()