module Day12
    open System
    open Common

    let initialPots =
        lazy (
            let statePattern = @"initial state: ([\.#]*)"
            let input =
                Day12Data.d
                |> Common.splitRows
                |> Array.head
            match input with
            | Regex statePattern [s] -> s
            | _ -> failwithf "could not parse input: %s" input
        )

    let rules =
        lazy (
            let rulesPattern = @"(.....) => (.)"
            let parseRule input =
                match input with
                | Regex rulesPattern [k; v] -> k, v
                | _ -> failwithf "could not parse input: %s" input
            (Day12Data.d |> Common.splitRows).[2..]
            |> Seq.map parseRule
            |> Map.ofSeq
        )

    let startingState = (initialPots.Value, 0)

    let step (pots, offset) =
        let paddedPots = "...." + pots + "...."
        let subset n = paddedPots.Substring (n, 5)
        let len = paddedPots.Length

        let getPot key =
            rules.Value
            |> Map.find key

        let rec runSubsets acc n =
            if n > (len - 5) then
                acc
            else
                runSubsets (acc + getPot (subset n)) (n + 1)

        let newPots = runSubsets "" 0

        let newPots' = newPots |> trimChars "."
        let offset' = offset - 2 + (newPots.IndexOf "#")

        (newPots', offset')

    let findPotSum pots offset =
        pots
        |> Seq.mapi (fun index pot -> if pot = '#' then index + offset else 0)
        |> Seq.sum

    let part1 () =
        let rec iterate (pots, offset) generation =
            if generation = 0 then
                (pots, offset)
            else
                iterate (step (pots, offset)) (generation - 1)

        let (pots, offset) = iterate startingState 20

        findPotSum pots offset

    let part2 () =
        let rec iterate (pots, offset) generation =
            let (pots', offset') = (step (pots, offset))

            if pots = pots' then
                (pots', offset', offset' - offset, generation)

            else
                iterate (pots', offset') (generation + 1)

        let startingState = (initialPots.Value, 0)
        let (pots, offset, offsetDelta, generation) = iterate startingState 1

        let potSum = findPotSum pots offset |> bigint

        let countPlants pots =
            pots |> Seq.filter (fun pot -> pot = '#') |> Seq.length |> bigint

        potSum + (bigint offsetDelta * (countPlants pots)) * (50000000000I - bigint generation)

    let show () =
        showDay
            12
            part1 (Some 3605)
            part2 (Some 4050000000798I)
