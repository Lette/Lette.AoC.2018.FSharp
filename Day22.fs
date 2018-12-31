module Day22
    open Common
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

    let part1 () =
        let depth, (tx, ty) = input.Value

        let map = Array.init (ty + 1) (fun _ -> Array.create (tx + 1) Region.Zero)

        let get x y = Array.get (Array.get map y) x
        let set x y i = Array.set (Array.get map y) x i

        let getErosionLevel (x, y) =
            get x y |> fun { ErosionLevel = e } -> e
        
        let geologicIndex pos =
            match pos with
            | (0, 0) -> 0
            | t when t = (tx, ty) -> 0
            | (x, 0) -> x * 16807
            | (0, y) -> y * 48271
            | (x, y) -> getErosionLevel (x - 1, y) * getErosionLevel (x, y - 1)

        for y = 0 to ty do
            for x = 0 to tx do
                let g = geologicIndex (x, y)
                let e = (g + depth) % 20183
                let r = e % 3
                set x y { GeologicIndex = g; ErosionLevel = e; RiskLevel = r }

        map
        |> Array.sumBy (Array.sumBy (fun { RiskLevel = r } -> r))

    let part2 () =
        0

    let show () =
        showDay
            22
            part1 (Some 5637)
            part2 None
