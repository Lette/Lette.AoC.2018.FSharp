module Day15
    open System
    open Common

    type Creature = { Type : char; X : int; Y : int; Hitpoints : int }

    let createElf x y = { Type = 'E'; X = x; Y = y; Hitpoints = 200 }
    let createGoblin x y = { Type = 'G'; X = x; Y = y; Hitpoints = 200 }

    type Cell =
    | Empty
    | Wall
    | Creature of Creature

    let initialData =
        lazy (
            let input =
                Day15Data.d'
                |> splitRows
                |> Array.map Seq.toArray

            let getRawCell x y =
                match x, y with
                | -1, _ -> ' '
                | _     -> input.[y].[x]

            let mutable creatures = []

            let createCell x y =
                match getRawCell x y with
                | '#' -> Wall
                | '.' -> Empty
                | 'E' ->
                    let creature = createElf x y
                    creatures <- creature :: creatures
                    Creature creature
                | 'G' ->
                    let creature = createGoblin x y
                    creatures <- creature :: creatures
                    Creature creature
                | c   -> failwithf "could not parse input: %A" c

            let rows = input |> Array.length
            let cols = input |> Seq.map Array.length |> Seq.max

            (Array2D.init cols rows createCell), creatures
        )

    let printMap (map : Cell [,]) =
        printfn ""
        for y = 0 to (map |> Array2D.length2) - 1 do
            for x = 0 to (map |> Array2D.length1) - 1 do
                let c =
                    match map.[x, y] with
                    | Wall -> '#'
                    | Empty -> '.'
                    | Creature { Type = t } -> t
                printf "%s" (string c)
            printfn ""
        printfn ""
        ()

    let isElf = function { Type = 'E' } -> true | _ -> false
    let isGoblin = function { Type = 'G' } -> true | _ -> false
    let cellIsEmpty = function Empty -> true | _ -> false

    let isSameType units =
        
        let hasElfs us = us |> List.tryFind isElf |> Option.isSome
        let hasGoblins us = us |> List.tryFind isGoblin |> Option.isSome

        (not (hasElfs units)) || (not (hasGoblins units))

    let getUnits map =
        0                

    let sort creatures =
        creatures |> List.sortBy (fun { X = x; Y = y } -> (y, x))

    let getAdjacentCoords { X = x; Y = y } =
        [(x, y - 1); (x - 1, y); (x + 1, y); (x, y + 1)]

    let createDistanceMap (map : char [,]) () =
        Array2D.create (map |> Array2D.length1) (map |> Array2D.length2) None

    let createDistanceMap' =
        let (map, _) = initialData.Value
        createDistanceMap map

    let findClosestTarget { X = x; Y = y} targets =
        let ds = 

    let move creature creatures =
        let opps = creatures |> List.filter (fun c -> c.Type <> creature.Type)
        let targetSquares =
            opps
            |> List.map getAdjacentCoords
            |> List.concat
            |> List.filter (fun (x, y) -> cellIsEmpty x y )

        if targetSquares |> List.isEmpty then
            creature
        else
            let closestTargetOption = findClosestTarget creature targetSquares



    let part1 () =
        let (map, creatures) = initialData.Value
    
        let rec step creatures creaturesDone round =
            printMap map
            match creatures, creaturesDone with
            | [], _ ->
                if isSameType creaturesDone then
                    round, creaturesDone
                else
                    step (sort creaturesDone) [] (round + 1)
            | c :: cs, ds ->

                let c' = move c
                // do stuff with c
                let targets = getTargets c
                // and update cs ds and map

                step cs (c :: ds) round

        step (sort creatures) [] 1
        |> fun (r, cs) -> (r - 1) * (cs |> List.sumBy (fun { Hitpoints = p } -> p))

    let part2 () =
        0

    let show () =
        showDay
            15
            part1 None
            part2 None
