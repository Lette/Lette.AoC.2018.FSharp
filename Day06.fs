module Day06
    open System
    open Common

    let xs =
        Day06Data.d.Split Environment.NewLine
        |> Array.toList

    let coordPattern = @"(\d+), (\d+)"

    let toTuple i s =
        match s with
        | Regex coordPattern [x; y] -> (i, int x, int y)
        | s -> failwith (sprintf "could not parse input: %s" s)

    let ps = xs |> List.mapi toTuple

    let (minX, minY, maxX, maxY) =
        ps
        |> List.fold (fun (x1, y1, x2, y2) (_, px, py) -> (min x1 px, min y1 py, max x2 px, max y2 py)) (99999, 99999, -99999, -99999)

    let ps' =
        ps
        |> List.map (fun (i, x, y) -> (i, x - (minX - 1), y - (minY - 1)))

    let distance (x1, y1) (x2, y2) =
        (abs (x1 - x2)) + (abs (y1 - y2))

    let closestTo x y =
        let folder (minDist, minDistPoints) (i, a, b) =
            let d = distance (x, y) (a, b)
            match d with
            | _ when d = minDist -> (d, i :: minDistPoints)
            | _ when d < minDist -> (d, [i])
            | _                  -> (minDist, minDistPoints)

        let closestPoints =
            ps'
            |> List.fold folder (1000, [])
            |> snd

        match closestPoints with
        | [i] -> i
        | _   -> -1

    let part1 () =
        let sx, sy = (maxX - minX + 3), (maxY - minY + 3)
        let arr = Array2D.init sx sy closestTo

        let infinites =
            [arr.[0, *]; arr.[*, 0]; arr.[sx - 1, *]; arr.[*, sy - 1]]
            |> List.map Array.toList
            |> List.collect id
            |> List.distinct
            |> List.filter (fun x -> x >= 0)

        arr
        |> Seq.cast<int>
        |> Seq.toList
        |> List.groupBy id
        |> List.filter (fun (k, _) -> not (infinites |> List.contains k))
        |> List.map (fun (_, v) -> v |> List.length)
        |> List.sortByDescending id
        |> List.tryHead
        |> Option.defaultValue -1 

    let totalDistanceTo x y =
        let folder acc (_, a, b) =
            acc + (distance (x, y) (a, b))

        ps'
        |> List.fold folder 0

    let part2 () =
        let sx, sy = (maxX - minX + 1), (maxY - minY + 1)
        let arr = Array2D.init sx sy totalDistanceTo

        arr
        |> Seq.cast<int>
        |> Seq.toList
        |> List.filter (fun i -> i < 10000)
        |> List.length

    let show () =
        showDay
            6
            part1 (Some 4342)
            part2 (Some 42966)
