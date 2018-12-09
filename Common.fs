module Common
    open System
    open System.Text.RegularExpressions

    let (|Regex|_|) pattern input =
        let m = Regex.Match(input, pattern)
        if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
        else None

    let cprintf color format =
        let continuation (result : string) =
            let previousColor = Console.ForegroundColor
            try
                Console.ForegroundColor <- color
                Console.Write result
            finally
                Console.ForegroundColor <- previousColor

        Printf.kprintf continuation format

    let showPart n getActual expectedOption =
        printf "   Part %i: " n

        let actual = getActual ()

        let resultOption =
            expectedOption
            |> Option.map (fun expected -> (actual = expected, expected))

        match resultOption with
        | None -> cprintf ConsoleColor.Blue "%A" actual
        | Some (false, expected) -> cprintf ConsoleColor.Red "FAIL: %A is not %A" actual expected
        | Some (true, _) -> cprintf ConsoleColor.Green "PASS: %A" actual

        printfn ""

    let showDay day getActual1 expectedOption1 getActual2 expectedOption2 =
        printfn "Day %i:" day
        showPart 1 getActual1 expectedOption1
        showPart 2 getActual2 expectedOption2

    let flip f a b = f b a
    