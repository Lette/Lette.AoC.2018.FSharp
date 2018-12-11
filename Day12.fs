module Day12
    open System
    open Common

    let pattern = @"(\d+)"

    let toStuff s =
        match s with
        | Regex pattern [x] -> int x
        | _ -> failwithf "could not parse input: %s" s

    let xs =
        lazy (
            Day12Data.d.Split Environment.NewLine
            |> Array.toList
            |> List.map toStuff
        )

    let doStuff xs =
        xs |> List.length

    let doMoreStuff xs =
        xs |> List.find (fun _ -> true)

    let part1 () =
        xs.Value
        |> doStuff

    let part2 () =
        xs.Value
        |> doMoreStuff

    let show () =
        showDay
            12
            part1 None
            part2 None
