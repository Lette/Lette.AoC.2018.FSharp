module Day18
    open System
    open Common

    let initialMap =
        Day18Data.d
        |> splitRows
        |> Array.map Seq.toArray

    let dy = initialMap |> Array.length
    let dx = initialMap.[0] |> Array.length

    let getCell (map : char [] []) x y =
        match x, y with
        | -1,  _              -> ' '
        |  _, -1              -> ' '
        |  n,  _ when n >= dx -> ' '
        |  _,  m when m >= dy -> ' '
        |  _,  _              -> map.[y].[x]

    let getAdjacentCells map x y =
        let getCell' = getCell map

        [
            getCell' (x - 1) (y - 1)
            getCell' (x) (y - 1)
            getCell' (x + 1) (y - 1)
            getCell' (x - 1) (y)
            getCell' (x + 1) (y)
            getCell' (x - 1) (y + 1)
            getCell' (x) (y + 1)
            getCell' (x + 1) (y + 1)
        ]

    let showMap (map : char [] []) =
        System.Console.SetCursorPosition (0, 0)
        map
        |> Array.map (Array.toSeq >> String.Concat)
        |> Array.iter (printfn "%s")
        printfn ""

    let tickCell map x y =
        let cell = getCell map x y
        let adjacentCells = getAdjacentCells map x y

        match cell with
        | '.' -> if adjacentCells |> List.filter ((=) '|') |> List.truncate 3 |> List.length >= 3 then '|' else '.'
        | '|' -> if adjacentCells |> List.filter ((=) '#') |> List.length >= 3 then '#' else '|'
        | '#' -> if adjacentCells |> List.groupBy id |> List.map (fun (k, _) -> k) |> (fun cs -> (cs |> List.contains '#') && (cs |> List.contains '|')) then '#' else '.'
        | _   -> failwithf "could not parse input: %A" cell

    let tickRow map y =
        Array.init dx (flip (tickCell map) y)

    let tick map =
        Array.init dy (tickRow map)

    let product map =
        let allCells = map |> Array.concat |> Array.toSeq |> Seq.cache
        let woodedAcres = allCells |> Seq.filter ((=) '|') |> Seq.length
        let lumberyards = allCells |> Seq.filter ((=) '#') |> Seq.length

        woodedAcres * lumberyards

    let rec run map n cache =

        match n with
        | 0 -> (map, n, None)
        | _ ->
            let map' = tick map
            let existing = cache |> List.tryFind (fun (map, _) -> map = map')

            match existing with
            | None                        -> run map' (n - 1) ((map', n) :: cache)
            | Some (_, previousIteration) -> (map', n, Some (previousIteration, cache))

    let runAll n =
        let finalMap, finalIteration, repetition = run initialMap n List.empty

        let map =
            match repetition with
            | None                            -> finalMap
            | Some (previousIteration, cache) ->
                cache
                |> List.find (fun (map, ix) -> ix = 1 + previousIteration - (finalIteration % (previousIteration - finalIteration)))
                |> fst

        map |> product

    let part1 () =
        runAll 10

    let part2 () =
        runAll 1000000000

    let show () =
        showDay
            18
            part1 (Some 588436)
            part2 (Some 195290)
