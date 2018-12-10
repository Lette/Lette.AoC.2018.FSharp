module Day01
    open System
    open Common

    let xs =
        Day01Data.d.Split Environment.NewLine
        |> Array.toList
        |> List.map int

    let rec loop ys = seq {
        yield! ys
        yield! loop ys
    }

    let runningSum () =
        (loop xs)
        |> Seq.scan (+) 0

    let findFirstDuplicate (xs : int seq) =

        let folder (s, _) item =
            if Set.contains item s then
                (s, Some item)
            else
                (Set.add item s, None)

        xs
        |> Seq.scan folder (Set.empty, None)
        |> Seq.pick (fun (_, dup) -> dup)

    let part1 () =
        xs
        |> List.sum

    let part2 () =
        runningSum ()
        |> findFirstDuplicate

    let show () =
        showDay
            1
            part1 (Some 505)
            part2 (Some 72330)
