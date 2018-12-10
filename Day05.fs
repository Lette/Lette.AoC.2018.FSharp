module Day05
    open System
    open Common

    let xs = Day05Data.d |> Seq.toList

    let (|IsReducible|IsNotReducible|) (a, b) =
        if ((a <> b) && ((Char.ToLower a) = (Char.ToLower b))) then
            IsReducible
        else
            IsNotReducible

    let reduce xs =
        let rec inner ys (acc, r) =
            match ys with
            | [] -> (acc, r)
            | [y] -> (y :: acc, r)
            | a :: b :: cs ->
                match (a, b) with
                | IsReducible -> inner cs (acc, r + 1)
                | IsNotReducible -> inner (b :: cs) (a :: acc, r)

        let rec outer ys =
            match inner ys ([], 0) with
            | (acc, 0) -> acc
            | (acc, _) -> outer acc

        outer xs

    let part1 () =
        reduce xs
        |> List.length

    let part2 () =
        let polymerTypes =
            xs
            |> List.fold (fun acc curr -> acc |> Set.add (curr |> Char.ToLower)) Set.empty
            |> Set.toList

        let removePolymerType t ps =
            ps |> List.filter (fun p -> (p |> Char.ToLower) <> t)

        polymerTypes
        |> List.map (fun t -> removePolymerType t xs |> reduce |> List.length)
        |> List.min

    let show () =
        showDay
            5
            part1 (Some 11476)
            part2 (Some 5446)
