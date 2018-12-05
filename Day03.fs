module Day03
    open System
    open System.Text.RegularExpressions

    type Claim = {
        Id : int
        Position : (int * int)
        Size : (int * int)
    }

    let createClaim =
        function
        | [id; x; y; dx; dy] -> { Id = int id; Position = (int x, int y); Size = (int dx, int dy) }
        | _ -> failwith "Wrong number of matches"

    let claimPattern = @"#(\d+) @ (\d+),(\d+): (\d+)x(\d+)"

    let (|Regex|_|) pattern input =
        let m = Regex.Match(input, pattern)
        if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
        else None

    let parseClaim claimStr =
        match claimStr with
        | Regex claimPattern matches -> createClaim matches
        | _ -> failwith "Bad input"

    let claims =
        Day03Data.d.Split Environment.NewLine
        |> Array.toList
        |> List.map parseClaim

    let toSingleSquareClaims claim =
        let (xStart, yStart) = claim.Position
        let (xSize, ySize) = claim.Size
        let (xEnd, yEnd) = (xStart + xSize - 1, yStart + ySize - 1)
        [ for x in xStart .. xEnd do
          for y in yStart .. yEnd do
          yield (x, y) ]

    let part1 () =
        claims
        |> List.collect toSingleSquareClaims
        |> List.groupBy id
        |> List.where (fun (_, v) -> List.length v >= 2)
        |> List.length

    let part2 () =
        let squaresSinglyClaimed =
            claims
            |> List.collect toSingleSquareClaims
            |> List.groupBy id
            |> List.where (fun (_, v) -> List.length v = 1)
            |> List.map (fun (k, _) -> k)

        claims
        |> List.find (fun claim ->
            toSingleSquareClaims claim
            |> List.forall (fun square -> List.contains square squaresSinglyClaimed))

    let show () =
        printfn "Day 3:"
        printfn "   Part 1: %A" (part1 ())
        printfn "   Part 2: %A" (part2 ())
