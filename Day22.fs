module Day22
    open System
    open Common

    let d' = @"depth: 510
target: 10,10"

    let d = @"depth: 11394
target: 7,701"

    let depthPattern = @"depth: (\d+)"
    let targetPattern = @"target: (\d+),(\d+)"

    let toDepth =
        function
        | Regex depthPattern [d] -> int d
        | s                      -> failwithf "could not parse input: %s" s
    
    let toTarget =
        function
        | Regex targetPattern [x; y] -> (int x, int y)
        | s                          -> failwithf "could not parse input: %s" s

    let input =
        lazy (
            let rows = d' |> splitRows |> Array.toList

            match rows with
            | [d; t] -> (toDepth d, toTarget t)
            | _      -> failwith "no. no! NOOOOO!!!!"
        )
        
    let part1 () =
        0

    let part2 () =
        0

    let show () =
        showDay
            22
            part1 None
            part2 None
