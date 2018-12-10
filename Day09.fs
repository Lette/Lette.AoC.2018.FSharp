module Day09
    open Common

    let pattern = @"(\d+) players; last marble is worth (\d+) points"

    let (nPlayers, maxMarble) =
        match Day09Data.d with
        | Regex pattern [p; m] -> (int p, int m)
        | _ -> failwith "could not parse input"

    [<AllowNullLiteralAttribute>]
    type Cell =
        val mutable public Prev : Cell
        val public Marble : int
        val mutable public Next : Cell
        new (m) = { Prev = null; Marble = m; Next = null }
        new (p, m, n) = { Prev = p; Marble = m; Next = n }

    let addAfter (current : Cell) m =
        let before = current
        let after = current.Next
        let cell = new Cell (before, m, after)
        before.Next <- cell
        after.Prev <- cell
        cell

    let remove (current : Cell) =
        let before = current.Prev
        let after = current.Next
        before.Next <- after
        after.Prev <- before
        after

    let rec stepBack n (current : Cell) =
        if n = 0 then
            current
        else
            stepBack (n - 1) (current.Prev)

    let rec stepForward n (current : Cell) =
        if n = 0 then
            current
        else
            stepForward (n - 1) (current.Next)

    let createStartMarble n =
        let startingMarble = new Cell (n)
        startingMarble.Prev <- startingMarble
        startingMarble.Next <- startingMarble
        startingMarble

    let addToScore (players : bigint array) player score =
        players.[player] <- players.[player] + score

    let runMoves n current players maxMarble=
        let rec inner n current =
            if n > maxMarble then
                ()
            else
                if n % 23 = 0 then
                    let c = stepBack 7 current
                    addToScore players (n % nPlayers) (bigint (n + c.Marble))
                    inner (n + 1) (remove c)
                else
                    let c = stepForward 1 current
                    inner (n + 1) (addAfter c n)
        inner n current

    let part1 () =
        let players = Array.create nPlayers 0I
        runMoves 1 (createStartMarble 0) players maxMarble
        players |> Array.max

    let part2 () =
        let players = Array.create nPlayers 0I
        runMoves 1 (createStartMarble 0) players (maxMarble * 100)
        players |> Array.max

    let show () =
        showDay
            9
            part1 (Some 398502I)
            part2 (Some 3352920421I)
