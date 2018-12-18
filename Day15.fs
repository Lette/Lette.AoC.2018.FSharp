module Day15
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
                Day15Data.d
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

    let printCreatureSummary creatures =
        creatures
        |> List.groupBy (fun { Type = t } -> t)
        |> List.sortBy (fun (k, _) -> k)
        |> List.iter (fun (k, vs) ->
            printfn
                "%A:\t%i\t%i"
                k
                (vs |> List.sumBy (fun v -> v.Hitpoints))
                (vs |> List.length)
            )

    let printMap map creatures =
        consoleHome ()
        printfn ""
        for y = 0 to (Array2D.length2 map) - 1 do
            for x = 0 to (Array2D.length1 map) - 1 do
                let c =
                    match Array2D.get map x y with
                    | Wall -> '#'
                    | Empty -> '.'
                    | Creature { Type = t } -> t
                printf "%s" (string c)
            printfn ""
        printfn ""
        printCreatureSummary creatures
        sleep 50
        ()

    let printDistMap dmap =
        printfn ""
        for y = 0 to (Array2D.length2 dmap) - 1 do
            for x = 0 to (Array2D.length1 dmap) - 1 do
                let c =
                    match Array2D.get dmap x y with
                    | None -> '.'
                    | Some d when d >= 10 -> 'X'
                    | Some d -> '0' + char d
                printf "%s" (string c)
            printfn ""
        printfn ""
        ()

    let isElf = function { Type = 'E' } -> true | _ -> false
    let isGoblin = function { Type = 'G' } -> true | _ -> false
    let cellIsEmpty = function Empty -> true | _ -> false
    let coordinateIsEmpty map x y = Array2D.get map x y |> cellIsEmpty
    let coordinatesFrom { X = x; Y = y } = (x, y)

    let isSameType units =
        let hasElves us = us |> List.tryFind isElf |> Option.isSome
        let hasGoblins us = us |> List.tryFind isGoblin |> Option.isSome

        (not (hasElves units)) || (not (hasGoblins units))

    let sort creatures =
        creatures |> List.sortBy (fun { X = x; Y = y } -> (y, x))

    let adjacentCoords (x, y) =
        [
            (x    , y - 1)
            (x - 1, y    )
            (x + 1, y    )
            (x    , y + 1)
        ]

    let adjacentCells map creature =
        let x, y = creature.X, creature.Y
        [
            Array2D.get map  x      (y - 1)
            Array2D.get map (x - 1)  y
            Array2D.get map (x + 1)  y
            Array2D.get map  x      (y + 1)
        ]

    let createDistanceMap map =
        Array2D.create (map |> Array2D.length1) (map |> Array2D.length2) None

    let findEnemiesOf creatureType creatures =
        creatures
        |> List.filter (fun c -> c.Type = creatureType)
        |> List.map (fun c -> (c.X, c.Y))

    let findMoveTargets map enemyLocations =
        enemyLocations
        |> List.map adjacentCoords
        |> List.concat
        |> List.filter (flip (||>) (coordinateIsEmpty map))

    let rec fillMap map dmap candidates dist =
        let rec fillDist map dmap candidates dist acc =
            match candidates with
            | [] -> acc
            | (x, y) :: cs ->
                let acc' =
                    if (coordinateIsEmpty map x y) && (Array2D.get dmap x y = None) then
                        Array2D.set dmap x y (Some dist)
                        List.append (adjacentCoords (x, y)) acc
                    else
                        acc
                fillDist map dmap cs dist acc'

        match candidates with
        | [] -> ()
        | _ ->
            let candidates' = fillDist map dmap candidates dist []
            fillMap map dmap candidates' (dist + 1)

    let findClosestRoute map startCoordinate targetCoordinates =
        let dmap = createDistanceMap map
        let (x0, y0) = startCoordinate
        Array2D.set dmap x0 y0 (Some 0)

        fillMap map dmap (adjacentCoords (x0, y0)) 1

        targetCoordinates
        |> List.map (fun (x, y) -> (x, y, Array2D.get dmap x y))
        |> List.filter (fun (_, _, d)-> Option.isSome d)
        |> List.map (fun (x, y, d) -> (x, y, Option.get d))
        |> List.sortBy (fun (x, y, d) -> (d, y, x))
        |> List.tryHead
        |> Option.map (fun (x, y, _) -> (x, y)) 

    let findClosestMoveTarget map creature moveTargets =
        findClosestRoute map (coordinatesFrom creature) moveTargets

    let findFirstStepOfClosestRoute map creature target =
        let candidateCoordinates =
            coordinatesFrom creature
            |> adjacentCoords
            |> List.filter (flip (||>) (coordinateIsEmpty map))

        findClosestRoute map target candidateCoordinates
        |> Option.get

    let otherType creature =
        (if creature.Type = 'E' then 'G' else 'E')

    let isCreatureOf creatureType =
        function
        | Creature { Type = t } when t = creatureType -> true
        | _                                           -> false

    let isNextToEnemy map creature =
        let enemyType = creature |> otherType
        creature
        |> adjacentCells map
        |> List.exists (isCreatureOf enemyType)

    let moveTo map creature (x, y) =
        let oldX, oldY = creature.X, creature.Y
        Array2D.set map oldX oldY Empty
        let creature' = { creature with X = x; Y = y }
        Array2D.set map x y (Creature creature')
        creature'

    let move map creature creatures =
        if isNextToEnemy map creature then
            creature
        else
            creatures
            |> findEnemiesOf (creature |> otherType)
            |> findMoveTargets map
            |> findClosestMoveTarget map creature
            |> Option.map (findFirstStepOfClosestRoute map creature)
            |> Option.map (moveTo map creature)
            |> Option.defaultValue creature

    let rec run map creatures creaturesDone round =
        match creatures, creaturesDone with
        | [], _ ->
            if isSameType creaturesDone then
                round, creaturesDone
            else
                printMap map creaturesDone
                run map (sort creaturesDone) [] (round + 1)
        | c :: cs, ds ->

            let c' = move map c (cs @ ds)

            run map cs (c' :: ds) round

    let runAll map creatures =
        consoleClear ()
        printMap map creatures
        run map (sort creatures) [] 1

    let outcome finalRound remainingCreatures =
        (finalRound - 1) * (remainingCreatures |> List.sumBy (fun { Hitpoints = p } -> p))

    let part1 () =
        let (map, creatures) = initialData.Value
        runAll map creatures ||> outcome

    let part2 () =
        0

    let show () =
        showDay
            15
            part1 None
            part2 None
