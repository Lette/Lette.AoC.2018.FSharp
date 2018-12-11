module Day11
    open Common

    let toPowerLevel serial x y =
        let x' = x + 1
        let y' = y + 1
        let rackId = x' + 10
        (((rackId * y' + serial) * rackId) % 1000) / 100 - 5

    let xs =
        Array2D.init 300 300 (toPowerLevel Day11Data.d)

    let subGridTotal x y z =
        let mutable sum = 0
        for a = x to (x+(z-1)) do
            for b = y to (y+(z-1)) do
                sum <- sum + xs.[a, b]
        sum

        // seq {
        //     for a in x .. (x+(z-1)) do
        //     for b in y .. (y+(z-1)) do
        //         yield xs.[a, b] }
        // |> Seq.sum

    let t4fourth (_, _, _, x) = x

    let findMaxBySize n =
        seq {
            for x in 0 .. (300 - n) do
            for y in 0 .. (300 - n) do
                yield (x, y, n, subGridTotal x y n) }
        |> Seq.maxBy t4fourth

    let part1 () =

        findMaxBySize 3
        |> (fun (x, y, _, _) -> sprintf "%i,%i" (x + 1) (y + 1))

    let part2 () =

        let rec inner n acc =
            if n = 301 then
                acc
            else
                let m = (findMaxBySize n)
                //printfn "%i: %A" n m
                if t4fourth m < 0 then  // <-- Totally arbitrary exit condition!
                    acc
                else
                    inner (n + 1) (m :: acc)
        
        inner 1 []
        |> List.maxBy t4fourth
        |> (fun (x, y, z, _) -> sprintf "%i,%i,%i" (x + 1) (y + 1) z)

    let show () =
        showDay
            11
            part1 (Some "21,72")
            part2 (Some "242,13,9")
