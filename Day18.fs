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
        seq {
            yield getCell' (x - 1) (y - 1)
            yield getCell' (x) (y - 1)
            yield getCell' (x + 1) (y - 1)
            yield getCell' (x - 1) (y)
            yield getCell' (x + 1) (y)
            yield getCell' (x - 1) (y + 1)
            yield getCell' (x) (y + 1)
            yield getCell' (x + 1) (y + 1)
        }

    let showMap (map : char [] []) =
        printfn ""
        map |> Array.map (Array.toSeq >> String.Concat) |> Array.iter (printfn "%s")
        printfn ""

    let tick map =

        let createCell map x y =
            let cell = getCell map x y
            let adjacentCells = getAdjacentCells map x y

            match cell with
            | '.' -> if adjacentCells |> Seq.filter ((=) '|') |> Seq.length >= 3 then '|' else '.'
            | '|' -> if adjacentCells |> Seq.filter ((=) '#') |> Seq.length >= 3 then '#' else '|'
            | '#' -> if adjacentCells |> Seq.groupBy id |> Seq.map (fun (k, v) -> k) |> (fun cs -> (cs |>Seq.contains '#') && (cs |> Seq.contains '|')) then '#' else '.'
            | _   -> failwithf "could not parse input: %A" cell

        let createRow map y =
            Array.init dx (flip (createCell map) y)

        Array.init dy (createRow map)

    let rec run map n =
        match n with
        | 0 -> map
        | _ -> run (tick map) (n - 1)

    let runAll n =
        let map' = run initialMap n

        let allCells = map' |> Array.concat |> Array.toSeq |> Seq.cache
        let woodedAcres = allCells |> Seq.filter ((=) '|') |> Seq.length
        let lumberyards = allCells |> Seq.filter ((=) '#') |> Seq.length

        woodedAcres * lumberyards

    let part1 () =
        runAll 10

    let part2 () =
        //runAll 1000000000
        0

    let show () =
        showDay
            18
            part1 (Some 588436)
            part2 None
