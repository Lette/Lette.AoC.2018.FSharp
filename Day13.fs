module Day13
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
                | _     -> input.[y].[x]

            let mutable carts = []

            let createCell x y =
                let createCart heading =
                    { X = x; Y = y; Facing = heading; NextTurn = Left }

                match (getRawCell (x - 1) y), (getRawCell x y) with
                | _  , ' '  -> Empty
                | _  , '-'  -> WestEast
                | _  , '|'  -> NorthSouth
                | '-', '\\' -> SouthWest
                | '+', '\\' -> SouthWest
                | '<', '\\' -> SouthWest
                | '>', '\\' -> SouthWest
                | _  , '\\' -> NorthEast
                | '-', '/'  -> NorthWest
                | '+', '/'  -> NorthWest
                | '>', '/'  -> NorthWest
                | '<', '/'  -> NorthWest
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

            let mine = Array2D.init cols rows createCell

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

    let getCartPosition { X = x; Y = y } =
        (x, y)

    let showCarts carts =
        let getMineChar =
            function
            | Empty        -> ' '
            | NorthSouth   -> '|'
            | WestEast     -> '-'
            | Intersection -> '+'
            | NorthEast    -> '\\'
            | SouthEast    -> '/'
            | SouthWest    -> '\\'
            | NorthWest    -> '/'

        let getCartChar { Facing = heading } =
            match heading with
            | North -> '^'
            | East  -> '>'
            | South -> 'v'
            | West  -> '<'

        let mine = initialState.Value |> fst

        let getChar x y =
            carts
            |> List.tryFind (fun c -> c.X = x && c.Y = y)
            |> Option.map getCartChar
            |> Option.defaultValue (getMineChar mine.[x, y])

        printfn ""
        for y = 0 to Array2D.length2 mine - 1 do
            for x = 0 to Array2D.length1 mine - 1 do
                printf "%c" (getChar x y)
            printfn ""
        printfn ""
    
    let sort carts =
        carts |> List.sortBy (fun cart -> cart.Y, cart.X)

    let part1 () =
        let rec tick carts acc crash =
            match crash with
            | Some pos -> pos
            | None ->
                match carts with
                | [] ->
                    //showCarts acc
                    tick (sort acc) [] None
                | cart :: carts' ->
                    let cart' = update cart
                    let crash =
                        ((cart' :: carts') @ acc)
                        |> List.map getCartPosition
                        |> List.groupBy id
                        |> List.tryFind (fun (_, v) -> List.length v > 1) 
                        |> Option.map (fun (k, _) -> k)

                    tick carts' (cart' :: acc) crash

        let initialCarts = initialState.Value |> snd |> sort

        //showCarts carts
        tick initialCarts [] None
        |> (fun (x, y) -> sprintf "%i,%i" x y)

    let part2 () =
        let rec tick carts acc crash =
            match crash with
            | Some pos ->
                let carts' = carts |> List.filter (fun c -> (c.X, c.Y) <> pos)
                let acc' = acc |> List.filter (fun c -> (c.X, c.Y) <> pos)
                tick carts' acc' None
            | None ->
                match carts with
                | [] ->
                    //showCarts acc
                    match acc with
                    | [c] -> (c.X, c.Y)
                    | _ -> tick (sort acc) [] None
                | cart :: carts' ->
                    let cart' = update cart
                    let crash =
                        ((cart' :: carts') @ acc)
                        |> List.map getCartPosition
                        |> List.groupBy id
                        |> List.tryFind (fun (_, v) -> List.length v > 1) 
                        |> Option.map (fun (k, _) -> k)

                    tick carts' (cart' :: acc) crash

        let initialCarts = initialState.Value |> snd |> sort

        //showCarts carts
        tick initialCarts [] None
        |> (fun (x, y) -> sprintf "%i,%i" x y)

    let show () =
        showDay
            13
            part1 (Some "16,45")
            part2 (Some "21,91")
