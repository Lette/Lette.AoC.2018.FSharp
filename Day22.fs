module Day22
    open Common
    open System
    open Checked

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
            let rows = d |> splitRows |> Array.toList

            match rows with
            | [d; t] -> (toDepth d, toTarget t)
            | _      -> failwith "no. no! NOOOOO!!!!"
        )

    type Region =
        {
            GeologicIndex : int
            ErosionLevel : int
            RiskLevel : int
        }
        with static member Zero = { GeologicIndex = 0; ErosionLevel = 0; RiskLevel = 0 }

    let get map x y = Array.get (Array.get map y) x
    let set map x y v = Array.set (Array.get map y) x v

    let createMap sx sy =
        let depth, (tx, ty) = input.Value

        let map = Array.init sy (fun _ -> Array.create sx Region.Zero)

        let getErosionLevel (x, y) =
            get map x y |> fun { ErosionLevel = e } -> e
        
        let geologicIndex pos =
            match pos with
            | (0, 0) -> 0
            | t when t = (tx, ty) -> 0
            | (x, 0) -> x * 16807
            | (0, y) -> y * 48271
            | (x, y) -> getErosionLevel (x - 1, y) * getErosionLevel (x, y - 1)

        for y = 0 to (sy - 1) do
            for x = 0 to (sx - 1) do
                let g = geologicIndex (x, y)
                let e = (g + depth) % 20183
                let r = e % 3
                set map x y { GeologicIndex = g; ErosionLevel = e; RiskLevel = r }

        map
        |> Array.map (Array.map (fun { RiskLevel = r } -> r))

    let part1 () =
        let _, (tx, ty) = input.Value

        let map = createMap (tx + 1) (ty + 1)

        map
        |> Array.sumBy (Array.sum)

    type Minimum =
        {
            ClimbingGear : int
            Torch : int
            Neither : int
        }
        with static member Default = { ClimbingGear = Int32.MaxValue; Torch = Int32.MaxValue; Neither = Int32.MaxValue }

    //                     region
    //                     rocky (0)   wet (1)   narrow (2)
    // tool climbing gear  X           X         -
    //      torch          X           -         X
    //      neither        -           X         X
    //   
    let part2 () =
        let _, (tx, ty) = input.Value

        let m = 2 * max tx ty
        let map = createMap m m

        let minimums = Array.init m (fun _ -> Array.create m None)
        set minimums 0 0 (Some 0)

        //let candidateMoves = [((0, 0), ]

        map
        |> Array.sumBy (Array.sum)

    let show () =
        showDay
            22
            part1 (Some 5637)
            part2 None
