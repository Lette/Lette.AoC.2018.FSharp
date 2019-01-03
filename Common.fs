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
    
    let joinStrings sep (xs : string seq) =
        String.Join (sep, xs)

    let joinWithLineBreaks xs = joinStrings Environment.NewLine xs

    let trim (s : string) = s.Trim ()
    let rec trimChars (remove : string) (s : string) =
        if s.StartsWith (remove) then
            trimChars remove (s.Substring (remove.Length))
        else if s.EndsWith (remove) then
            trimChars remove (s.Substring (0, s.Length - remove.Length))
        else
            s
    let splitRows (s : string) = s.Split ([| "\r\n"; "\n" |], StringSplitOptions.None)
    let replace (oldString : string) (newString : string) (s : string) = s.Replace (oldString, newString)

    let consoleClear () = System.Console.Clear ()
    let consoleHome () = System.Console.SetCursorPosition (0, 0)
    let sleep (milliseconds : int) = System.Threading.Thread.Sleep milliseconds

    let divisors n =
        let rec inner n xs k limit =
            seq {
                if k > limit then
                    yield! xs
                else
                    if n % k = 0 then
                        yield k
                        if k = limit then
                            yield! inner n xs (k + 1) limit
                        else
                            yield! inner n ((n / k) :: xs) (k + 1) limit
                    else
                        yield! inner n xs (k + 1) limit
            }
        inner n [] 1 (int (sqrt (float n)))
