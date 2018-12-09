module Day07
    open System
    open Common

    let pattern = @"Step ([A-Z]) must be finished before step ([A-Z]) can begin\."

    let toDependency =
        function
        | Regex pattern [before; after] -> (before, after)
        | badInput                      -> failwithf "could not parse input: %s" badInput

    let xs =
        Day07Data.d.Split Environment.NewLine
        |> Array.toList
        |> List.map toDependency
        |> List.distinct

    let allSteps =
        xs
        |> List.map (fun (i, o) -> [i; o])
        |> List.collect id
        |> List.distinct
        |> List.sort

    let stepIsFirst edges step =
        edges |> List.map snd |> List.contains step |> not

    let findNextStep steps edges =
        steps |> List.find (stepIsFirst edges)

    let removeStep step steps =
        steps |> List.filter (fun s -> s <> step)

    let removeManySteps removed steps =
        removed |> List.fold (flip removeStep) steps

    let removeEdges step edges =
        edges |> List.filter (fun (i, o) -> not ((i = step) || (o = step)))

    let removeManyEdges removed edges =
        removed |> List.fold (flip removeEdges) edges

    let part1 () =

        let rec findOrder steps edges acc =
            match steps, edges with
            | [],          [] -> acc
            | [finalStep], [] -> acc + finalStep
            | [],          _  -> failwith "no steps, but still have edges? wtf!"
            | _,           _  ->
                let step = findNextStep steps edges
                let remainingSteps = steps |> removeStep step
                let remainingEdges = edges |> removeEdges step

                findOrder remainingSteps remainingEdges (acc + step)

        findOrder allSteps xs ""

    let part2 () =

        let stepWorkers workers =
            let folder (newWorkers, finished) work =
                match work with
                | Some (step, 1) -> (None               :: newWorkers, step :: finished)
                | Some (step, n) -> (Some (step, n - 1) :: newWorkers,         finished)
                | None           -> (None               :: newWorkers,         finished) 

            workers |> List.fold folder ([], [])

        let workLength (s : string) =
            60 + int (char (s.Substring (0, 1))) - (int 'A') + 1

        let assignAvailableWork workers steps =

            let rec inner workers steps acc =
                match workers, steps with
                | [],           _       -> acc
                | _,            []      -> (workers @ acc)
                | Some w :: ws, _       -> inner ws steps (Some w :: acc)
                | None :: ws,   s :: ss -> inner ws ss (Some (s, workLength s) :: acc)
                
            inner workers steps []

        let rec findTotalTime t work steps edges =

            let work', justFinishedWork = stepWorkers work
            let steps' = steps |> removeManySteps justFinishedWork

            match steps' with
            | [] -> t
            | _  ->
                let edges' = edges |> removeManyEdges justFinishedWork

                let ongoingWork = work' |> List.choose id |> List.map fst
                let availableWork = steps' |> removeManySteps ongoingWork |> List.filter (stepIsFirst edges')
                let work'' = assignAvailableWork work' availableWork

                findTotalTime (t + 1) work'' steps' edges'

        findTotalTime 0 [None; None; None; None; None] allSteps xs

    let show () =
        showDay
            7
            part1 (Some "IJLFUVDACEHGRZPNKQWSBTMXOY")
            part2 (Some 1072)
