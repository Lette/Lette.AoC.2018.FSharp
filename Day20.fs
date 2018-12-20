module Day20
    open Common

    let xs =
        lazy (
            [ for c in Day20Data.d -> c ]
        )

    type Room =
        {
            Pos : (int * int)
            mutable North : Room option
            mutable East : Room option
            mutable South : Room option
            mutable West : Room option
            mutable Weight : int option
        }

    let createRoom pos north east south west =
        { Pos = pos; North = north; East = east; South = south; West = west; Weight = None }
    let createRoomAt pos = createRoom pos None None None None
    let setWeight room weight = room.Weight <- Some weight

    let positionToTheNorth { Pos = (x, y) } = (x,     y - 1)
    let positionToTheEast  { Pos = (x, y) } = (x + 1, y    )
    let positionToTheSouth { Pos = (x, y) } = (x    , y + 1)
    let positionToTheWest  { Pos = (x, y) } = (x - 1, y    )

    let setNorth origin destination = origin.North <- Some destination
    let setEast  origin destination = origin.East  <- Some destination
    let setSouth origin destination = origin.South <- Some destination
    let setWest  origin destination = origin.West  <- Some destination

    let connectRooms setFromOrigin setFromDestination origin destination =
        setFromOrigin      origin      destination
        setFromDestination destination origin

    let connectRoomToTheNorth = connectRooms setNorth setSouth
    let connectRoomToTheEast  = connectRooms setEast  setWest
    let connectRoomToTheSouth = connectRooms setSouth setNorth
    let connectRoomToTheWest  = connectRooms setWest  setEast

    let addRoomToDict dict room = Map.add (room.Pos) room dict

    let go positionRelative connectRoomRelative room dict =
        let pos' = positionRelative room
        match dict |> Map.tryFind pos' with
        | Some room' ->
            connectRoomRelative room room'
            (room', dict)
        | None       ->
            let room' = createRoomAt pos'
            connectRoomRelative room room'
            (room', addRoomToDict dict room')

    let goNorth = go positionToTheNorth connectRoomToTheNorth
    let goEast  = go positionToTheEast  connectRoomToTheEast
    let goSouth = go positionToTheSouth connectRoomToTheSouth
    let goWest  = go positionToTheWest  connectRoomToTheWest

    let runRegex input room dict =

        let rec inner input stack room dict =
            match input with
            | [] -> dict
            | 'N' :: ds -> goNorth room dict ||> inner ds stack
            | 'E' :: ds -> goEast  room dict ||> inner ds stack
            | 'S' :: ds -> goSouth room dict ||> inner ds stack
            | 'W' :: ds -> goWest  room dict ||> inner ds stack
            | '^' :: ds -> inner ds stack room dict
            | '$' :: ds -> inner ds stack room dict
            | '(' :: ds -> inner ds (room :: stack) room dict
            | '|' :: ds -> inner ds stack (List.head stack) dict
            | ')' :: ds -> inner ds (List.tail stack) room dict
            | c   :: _  -> failwithf "could not parse input: %A" c

        inner input [] room dict

    let adjacentRooms room =
        [ room.North; room.East; room.South; room.West ]
        |> List.choose id

    let rec runLevel dict rooms level newCandidates =
        match rooms with
        | []                           -> newCandidates
        | { Weight = Some _ }    :: rs -> runLevel dict rs level newCandidates
        | { Weight = None } as r :: rs ->
            setWeight r level
            runLevel dict rs level ((adjacentRooms r) @ newCandidates)

    let rec runRooms candidateRooms level dict =
        match candidateRooms with
        | [] -> dict
        | rs -> runRooms (runLevel dict rs level []) (level + 1) dict

    let run input =
        let startRoom = createRoomAt (0, 0)

        startRoom
        |> addRoomToDict Map.empty
        |> runRegex input startRoom
        |> runRooms [startRoom] 0

    let part1 () =
        run xs.Value
        |> Map.toList
        |> List.map (fun (_, { Weight = w }) -> w)
        |> List.choose id
        |> List.max
        
    let part2 () =
        run xs.Value
        |> Map.toList
        |> List.map (fun (_, { Weight = w }) -> w)
        |> List.choose id
        |> List.filter ((<=) 1000)
        |> List.length

    let show () =
        showDay
            20
            part1 (Some 3314)
            part2 (Some 8550)
