module Day04
    open System
    open Common

    let timestampPattern = @"\[(\d\d\d\d-\d\d-\d\d \d\d:\d\d)\] "
    let beginsShiftPattern = timestampPattern + @"Guard #(\d+) begins shift"
    let fallsAsleepPattern = timestampPattern + @"falls asleep"
    let wakesUpPattern = timestampPattern + @"wakes up"

    type Log =
    | BeginsShift of DateTime * int
    | FallsAsleep of DateTime
    | WakesUp of DateTime

    type Sleep = { Start : DateTime; End : DateTime }
    type Guard = {Id : int; Events : Sleep list}
    let createGuard id events = { Id = id; Events = events |> List.rev }
    let createEvent t1 t2 = { Start = t1; End = t2 }

    let toDateTime d =
        DateTime.ParseExact (d, "yyyy-MM-dd HH:mm", null)

    let toGuardEvent =
        function
        | Regex beginsShiftPattern [timestamp; id] -> BeginsShift (toDateTime timestamp, int id)
        | Regex fallsAsleepPattern [timestamp] -> FallsAsleep (toDateTime timestamp)
        | Regex wakesUpPattern [timestamp] -> WakesUp (toDateTime timestamp)
        | s -> failwith (sprintf "Could not parse input: %s" s)

    let ds =
        Day04Data.d.Split ([| Environment.NewLine |], StringSplitOptions.None)
        |> Array.toList
        |> List.sort
        |> List.map toGuardEvent

    let logByGuardAndDate xs =

        let rec inner ys guards currId currEvents =
            match ys with
            | [] ->
                (createGuard currId currEvents) :: guards
            | BeginsShift (_, id) :: zs ->
                inner zs (createGuard currId currEvents :: guards) id []
            | FallsAsleep f :: WakesUp w :: zs ->
                inner zs guards currId (createEvent f w :: currEvents)
            | _ ->
                 failwith "possibly wrong order or log items"

        let firstId =
            match xs with
            | BeginsShift (_, id) :: _ -> id
            | _                        -> failwith "first log element must be begins shift"

        inner xs [] firstId [] |> List.rev

    let gs = logByGuardAndDate ds

    let minuteMostSleptByGuard guard =
        guard
        |> List.collect  (fun g -> g.Events)
        |> List.map (fun { Start = t1; End = t2 } -> (t1.Minute, t2.Minute))
        |> List.map (fun (m1, m2) -> [ for m in m1 .. (m2 - 1) do yield m ])
        |> List.collect id
        |> List.groupBy id
        |> List.sortByDescending (fun (_, g) -> g |> List.length)
        |> List.tryHead
        |> Option.map (fun (k, g) -> (k, g |> List.length))

    let part1 () =
        let totalSleepPerGuard =
            gs
            |> List.map
                (fun g -> (g, g.Events |> List.sumBy (fun s -> (s.End - s.Start).TotalMinutes)))
            |> List.groupBy (fun (g, _) -> g.Id)
            |> List.map (fun (id, xs) -> (id, xs |> List.sumBy (fun (g, fs) -> fs)))

        let guardThatSleepsMost =
            totalSleepPerGuard
            |> List.maxBy (fun (_, fs) -> fs)
            |> (fun (g, _) -> g)

        let minuteMostSlept =
            gs
            |> List.filter (fun g -> g.Id = guardThatSleepsMost)
            |> minuteMostSleptByGuard
            |> Option.map fst
            |> Option.defaultValue -1

        guardThatSleepsMost * minuteMostSlept
     
    let part2 () =

        gs
        |> List.groupBy (fun g -> g.Id)
        |> List.map (fun (k, g) -> (k, minuteMostSleptByGuard g))
        |> List.choose (fun (k, m) -> match m with None -> None | Some n -> Some (k, n))
        |> List.sortByDescending (fun (_, (_, count)) -> count)
        |> List.tryHead
        |> Option.map (fun (id, (minute, _)) -> id * minute)
        |> Option.defaultValue -1
        
    let show () =
        printfn "Day 4:"
        printfn "   Part 1: %A (11367)" (part1 ())
        printfn "   Part 2: %A (36896)" (part2 ())
