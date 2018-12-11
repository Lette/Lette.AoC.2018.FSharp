module Day10
    open System
    open Common

    let pattern = @"position=< *(-?\d+), *(-?\d+)> velocity=< *(-?\d+), *(-?\d+)>"

    let toTuple s =
        match s with
        | Regex pattern [x; y; dx; dy] -> (int x, int y, int dx, int dy)
        | _                            -> failwithf "could not parse input: %s" s

    let xs =
        lazy (
            Day10Data.d.Split Environment.NewLine
            |> Array.toList
            |> List.map toTuple
        )

    let createMsg data x1 y1 x2 y2 =
        let getChar x y cs =
            if (List.contains (x, y) cs) then
                '#'
            else
                '.'

        let normalizedPixels =
            data
            |> List.map (fun (x, y, _, _) -> (x - x1, y - y1))
            |> List.distinct

        let sx, sy = x2 - x1 + 1, y2 - y1 + 1

        List.init sy (fun y -> Array.init sx (fun x -> (getChar x y normalizedPixels)))
        |> List.map (Array.toSeq >> String.Concat)
        |> joinWithLineBreaks

    let findMessage stars =
        let moveStars stars =
            let folder (acc, minX, minY, maxX, maxY) (x, y, dx, dy) =
                let x', y' = x + dx, y + dy
                (x', y', dx, dy) :: acc,
                min minX x', min minY y', max maxX x', max maxY y'

            stars
            |> List.fold folder ([], 999999, 999999, -999999, -999999)

        let rec run stars seconds =
            if seconds = 100000 then
                failwith "ran out of time!"
            else
                let newStars, x1, y1, x2, y2 = moveStars stars
                let dy = y2 - y1
                if dy < 10 then
                    let msg = createMsg newStars x1 y1 x2 y2
                    (msg, seconds)
                else
                    run newStars (seconds + 1)
        run stars 1

    let part1 () =
        xs.Value
        |> findMessage
        |> fst
        |> (+) Environment.NewLine

    let part2 () =
        xs.Value
        |> findMessage
        |> snd

    let show () =
        showDay
            10
            part1 (Some Day10Data.CRXKEZPZ)
            part2 (Some 10081)
