module Day11
    open Common

    let size = 300

    let toPowerLevel serial x y =
        let x' = x + 1
        let y' = y + 1
        let rackId = x' + 10
        (((rackId * y' + serial) * rackId) % 1000) / 100 - 5

    let div100 = flip (/) 100
    let mod10 = flip (%) 10
    let hundredsDigit = div100 >> mod10
    let subtract = flip (-)

    let toPowerLevelPointless =
        (fun a ->
            (+) 11 >>
            (fun b -> 
                (+) 1
                >> (*) b
                >> (+) a
                >> (*) b
                >> hundredsDigit
                >> subtract 5
            )
        )

    let getPartialSum (arr : (int * int) [,]) x y  =
        match x, y with
        | -1, _  -> 0
        | _,  -1 -> 0
        | _      -> arr.[x, y] |> snd

    let getSumforSquare arr s x y =
        let x1 = x - 1
        let x2 = x + s - 1
        let y1 = y - 1
        let y2 = y + s - 1

        let f = getPartialSum arr

        let ul = f x1 y1
        let ur = f x2 y1
        let ll = f x1 y2
        let lr = f x2 y2

        lr - ll - ur + ul

    let xs =
        lazy (
            let arr = Array2D.create size size (0, 0)
            let toPowerLevel' = toPowerLevelPointless Day11Data.d
            let getSumForSquare' = getSumforSquare arr 1
            for x = 0 to (size - 1) do
                for y = 0 to (size - 1) do
                    let l = toPowerLevel' x y
                    let s = l - (getSumForSquare' x y)
                    arr.[x, y] <- (l, s)
            arr
        )

    let findMaxForSize n arr =
        let getSumForSquare' = getSumforSquare arr n
        seq {
            for x = 0 to (size - n) do
                for y = 0 to (size - n) do
                    yield (x, y, n, getSumForSquare' x y) }
        |> Seq.maxBy (fun (_, _, _, x) -> x)

    let part1 () =
        xs.Value
        |> findMaxForSize 3
        |> (fun (x, y, _, _) -> sprintf "%i,%i" (x + 1) (y + 1))

    let part2 () =
        let findMaxForEachSize arr =
            let rec inner n acc =
                if n = size + 1 then
                    acc
                else
                    inner (n + 1) (findMaxForSize n arr :: acc)
            inner 1 []

        xs.Value
        |> findMaxForEachSize
        |> List.maxBy (fun (_, _, _, x) -> x)
        |> (fun (x, y, s, _) -> sprintf "%i,%i,%i" (x + 1) (y + 1) s)

    let show () =
        showDay
            11
            part1 (Some "21,72")
            part2 (Some "242,13,9")
