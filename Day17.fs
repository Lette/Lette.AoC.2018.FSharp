module Day17
    open System
    open Common

    let xSpanPattern = @"y=(\d+), x=(\d+)\.\.(\d+)"
    let ySpanPattern = @"x=(\d+), y=(\d+)\.\.(\d+)"

    let toClayCoords =
        function
        | Regex xSpanPattern [ y; x1; x2 ] -> [ for x in (int x1) .. (int x2) -> (x, int y) ]
        | Regex ySpanPattern [ x; y1; y2 ] -> [ for y in (int y1) .. (int y2) -> (int x, y) ]
        | s -> failwithf "could not parse input: %s" s

    let xs =
        Day17Data.d
        |> splitRows
        |> Array.toList
        |> List.collect toClayCoords

    let findBoundingBox xs =
        let rec inner coords minX minY maxX maxY =
            match coords with
            | []           -> minX - 1, minY, maxX + 1, maxY
            | (x, y) :: cs -> inner cs (min minX x) (min minY y) (max maxX x) (max maxY y)
        inner xs Int32.MaxValue Int32.MaxValue Int32.MinValue Int32.MinValue

    let xMin, yMin, xMax, yMax = findBoundingBox xs

    let createMap () = Array.init (yMax - yMin + 1) (fun _ -> Array.create (xMax - xMin + 1) '.')

    let countWater map =
        let allWater =
            map
            |> Array.map (fun arr -> arr |> Array.toSeq |> Seq.filter (fun c -> c = '|' || c = '~') |> Seq.length)
            |> Array.sum
        
        let standingWater =
            map
            |> Array.map (fun arr -> arr |> Array.toSeq |> Seq.filter (fun c -> c = '~') |> Seq.length)
            |> Array.sum

        allWater, standingWater

    let showMap (map : char [] []) =
        map
        |> Array.iter (Seq.ofArray >> String.Concat >> printfn "%s")

        printfn ""
        printfn "water: %A" (countWater map)

    let showMap' map =
        System.Console.Clear ()
        showMap map
        sleep 250

    let set map x y c =
        Array.set (Array.get map (y - yMin)) (x - xMin) c

    let get map x y =
        if y > yMax then
            'X'
        else
            Array.get (Array.get map (y - yMin)) (x - xMin)

    let rec addClay xs map =
        match xs with
        | [] -> map
        | (x, y) :: cs ->
            set map x y '#'
            addClay cs map

    let below map x y = get map x (y + 1)

    let isTrapped map x y =
        let rec isTrapped x y f = 
            match get map x y, below map x y with
            | '#', _   -> true
            | _  , '.' -> false
            | _  , '|' -> false
            | _  , _   -> isTrapped (f x 1) y f

        (isTrapped x y (-)) && (isTrapped x y (+))

    let fillWithStandingWater map x y =
        let rec fill x y f =
            match get map x y with
            | '#' -> ()
            | _   ->
                set map x y '~'
                fill (f x 1) y f

        set map x y '~'
        fill (x - 1) y (-)
        fill (x + 1) y (+)

    let fillWithRunningWater map x y =
        let setAndReturn x y =
            set map x y '|'
            [(x, y)]

        let rec fill x y f =

            let setAndContinue x y =
                set map x y '|'
                fill (f x 1) y f

            match get map x y, below map x y with
            | '#', _ -> []
            | _  , '#' -> setAndContinue x y
            | _  , '~' -> setAndContinue x y
            | _  , '.' -> setAndReturn x y
            | _  , '|' -> setAndReturn x y
            | a  , b   -> failwithf "unknown case: %A, %A" a b

        set map x y '|'
        (fill (x - 1) y (-)) @ (fill (x + 1) y (+))

    let rec handleBlocking map x y =
        if isTrapped map x y then
            fillWithStandingWater map x y
            handleBlocking map x (y - 1)
        else
            fillWithRunningWater map x y

    let runWater map (x, y) =
        match get map x y, below map x y with
        | '~', _   -> [(x, y - 1)]
        | _,   'X' -> []   // last row, do nothing
        | _,   '.' ->      // falling
            set map x (y + 1) '|'
            [(x, y + 1)]
        | _,   '|' -> []   // running water below
        | _,   '#' -> handleBlocking map x y
        | _,   '~' -> handleBlocking map x y
        | _        -> failwith "shouldn't happen"

    let rec runMap waters map =
        //showMap' map
        match waters with
        | [] -> map
        | w :: ws -> runMap (ws @ (runWater map w)) map
        
    let part1 () =
        createMap ()
        |> addClay xs
        |> (fun map ->
            set map 500 yMin '|'
            map)
        |> runMap [(500, yMin)]
        |> countWater
        |> fst

    let part2 () =
        createMap ()
        |> addClay xs
        |> (fun map ->
            set map 500 yMin '|'
            map)
        |> runMap [(500, yMin)]
        |> countWater
        |> snd

    let show () =
        showDay
            17
            part1 (Some 52800)
            part2 (Some 45210)
