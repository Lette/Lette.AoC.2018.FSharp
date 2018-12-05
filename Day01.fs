module Day01
    open System

    let xs =
        Day01Data.d.Split Environment.NewLine
        |> Array.toList
        |> List.map int

    let rec loop ys = seq {
        yield! ys
        yield! loop ys
    }

    let part1 () = xs |> List.sum

    let runningSum () =
        (loop xs)
        |> Seq.scan (+) 0

    let findDup (xs : int seq) =
        let s = Set.empty

        let folder (s, d) item =
            if Set.contains item s then
                (s, item :: d)
            else
                (Set.add item s, d)

        Seq.scan folder (Set.empty, []) xs
        |> Seq.find (fun (_, ds) -> not (List.isEmpty ds))
        |> (fun (_, ds) -> ds)
        |> List.head

    let part2 () =
        runningSum () |> findDup

    let show () =
        printfn "Day 1:"    
        printfn "   Part 1: %i" (part1 ())
        printfn "   Part 2: %i" (part2 ())
