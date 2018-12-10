module Day10
    open System
    open Common

    let pattern = @"position=< *(-?[0-9]+), *(-?[0-9]+)> velocity=< *(-?[0-9]+), *(-?[0-9]+)>"

    let toTuple s =
        match s with
        | Regex pattern [x; y; dx; dy] -> (int x, int y, int dx, int dy)
        | _ -> failwithf "could not parse input: %s" s

    let xs =
        Day10Data.d.Split Environment.NewLine
        |> Array.toList
        |> List.map toTuple


    let step data =
        let folder (acc, (minX, minY, maxX, maxY)) (x, y, dx, dy) =
            let (newX, newY) = (x + dx, y + dy)
            ((newX, newY, dx, dy) :: acc, (min minX newX, min minY newY, max maxX newX, max maxY newY))

        data
        |> List.fold folder ([], (999999, 999999, -999999, -999999))

    let createMsg data (x1, y1, x2, y2) =

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
        |> List.toArray
        |> (fun ss -> String.Join (Environment.NewLine, ss))

    let findMessage data =
        let rec inner n data =
            if n = 100000 then
                failwith "ran out of time!"
            else
                let newData, (x1, y1, x2, y2) = step data
                let dy = y2 - y1
                if dy < 10 then
                    let msg = createMsg newData (x1, y1, x2, y2)
                    (msg, n)
                else
                    inner (n + 1) newData
        inner 1 data

    let part1 () =
        Environment.NewLine + (findMessage xs |> fst)

    let part2 () =
        findMessage xs |> snd

    let show () =
        showDay
            10
            part1 (Some Day10Data.CRXKEZPZ)
            part2 (Some 10081)
