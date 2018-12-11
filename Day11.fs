module Day11
    open Common

    let toPowerLevel serial x y =
        let x' = x + 1
        let y' = y + 1
        let rackId = x' + 10
        (((rackId * y' + serial) * rackId) % 1000) / 100 - 5

    let subGridTotal x y z (grid : int [,]) =
        [ for a in x .. (x+(z-1)) do
          for b in y .. (y+(z-1)) do
          yield grid.[a, b] ]
        |> Seq.sum

    let xs =
        Array2D.init 300 300 (toPowerLevel Day11Data.d)

    let findMaxBySize n =
        [ for x in 0 .. (300 - n) do
          for y in 0 .. (300 - n) do
          yield (x, y, n, subGridTotal x y n xs)]
        |> List.maxBy (fun (_, _, _, p) -> p)

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
                inner (n + 1) (m :: acc)
        
        inner 1 []
        |> List.maxBy (fun (_, _, _, p) -> p)
        |> (fun (x, y, z, _) -> sprintf "%i,%i,%i" (x + 1) (y + 1) z)

    let show () =
        showDay
            11
            part1 (Some "21,72")
            part2 (Some "242,13,9")
