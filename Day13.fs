module Day13
    open System
    open Common

    type Cell =
    | Empty
    | WestEast
    | NorthSouth
    | NorthEast
    | SouthEast
    | SouthWest
    | NorthWest
    | Intersection

    type Heading = North | East | South | West
    type Turn = Left | NoTurn | Right
    type Cart = { X : int; Y : int; Facing : Heading; NextTurn : Turn }

    let initialState =
        lazy (
            let input =
                Day13Data.d
                |> splitRows
                |> Array.map Seq.toArray

            let getRawCell x y =
                match x, y with
                | -1, _ -> ' '
                | _     ->
                    if y >= input.Length then
                        printfn "y = %i, len = %i" y input.Length

                    if x >= input.[y].Length then
                        printfn "x = %i, len = %i" x input.[y].Length

                    input.[y].[x]

            let mutable carts = []

            let createCell x y =
                let createCart heading =
                    { X = x; Y = y; Facing = heading; NextTurn = Left }

                match (getRawCell (x - 1) y), (getRawCell x y) with
                | _  , ' '  -> Empty
                | _  , '-'  -> WestEast
                | _  , '|'  -> NorthSouth
                | '-', '\\' -> SouthWest
                | _  , '\\' -> NorthEast
                | '-', '/'  -> NorthWest
                | _  , '/'  -> SouthEast
                | _  , '+'  -> Intersection
                | _  , '^'  ->
                    carts <- createCart North :: carts
                    NorthSouth
                | _  , '>'  ->
                    carts <- createCart East :: carts
                    WestEast
                | _  , 'v'  ->
                    carts <- createCart South :: carts
                    NorthSouth
                | _  , '<'  ->
                    carts <- createCart West :: carts
                    WestEast
                | _         -> failwithf "could not parse input: %c" (getRawCell x y)

            let rows = input |> Array.length
            let cols = input |> Seq.map Array.length |> Seq.max
            printfn "rows = %i; cols = %i" rows cols

            let mine = Array2D.init rows cols createCell

            (mine, carts)
        )

    let turn (mine : Cell [,]) cart =
        let cell = mine.[cart.X, cart.Y]

        let facing =
            let headingAfterIntersection heading nextTurn =
                match heading, nextTurn with
                | _,     NoTurn -> heading
                | North, Left   -> West
                | North, Right  -> East
                | East,  Left   -> North
                | East,  Right  -> South
                | South, Left   -> East
                | South, Right  -> West
                | West,  Left   -> South
                | West,  Right  -> North

            match cell, cart.Facing with
            | Intersection, _     -> headingAfterIntersection cart.Facing cart.NextTurn
            | NorthEast,    West  -> North
            | NorthEast,    South -> East
            | SouthEast,    West  -> South
            | SouthEast,    North -> East
            | SouthWest,    East  -> South
            | SouthWest,    North -> West
            | NorthWest,    East  -> North
            | NorthWest,    South -> West
            | _                   -> cart.Facing

        let nextTurn =
            match cell, cart.NextTurn with
            | Intersection, Left   -> NoTurn
            | Intersection, NoTurn -> Right
            | Intersection, Right  -> Left 
            | _                    -> cart.NextTurn

        { cart with Facing = facing; NextTurn = nextTurn }

    let move cart =
        let x, y =
            match cart.Facing with
            | North -> cart.X,     cart.Y - 1
            | East  -> cart.X + 1, cart.Y
            | South -> cart.X,     cart.Y + 1
            | West  -> cart.X - 1, cart.Y

        { cart with X = x; Y = y }

    let turn' = turn (initialState.Value |> (fun (mine, _) -> mine))

    let update cart =
        cart |> move |> turn'

    let part1 () =
        let carts = initialState.Value |> snd
        carts

    let part2 () =
        0

    let show () =
        showDay
            13
            part1 None
            part2 None
