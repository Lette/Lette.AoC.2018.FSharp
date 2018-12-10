module Day02
    open System
    open Common

    let xs =
        Day02Data.d.Split Environment.NewLine
        |> Array.toList

    let characterDuplicationInfo input =
        let ks =
            input
            |> Seq.groupBy id
            |> Seq.groupBy (fun (_, v) -> Seq.length v)
            |> Seq.map (fun (k, _) -> k)
            |> Set.ofSeq
        
        (if Set.contains 2 ks then 1 else 0), if Set.contains 3 ks then 1 else 0

    let part1 () =
        xs
        |> List.map characterDuplicationInfo
        |> List.reduce (fun (a, b) (c, d) -> (a + c, b + d))
        |> (fun (x, y) -> x * y)

    let part2 () =

        let separation (s1 : string) (s2 : string) =
            let cs1 = s1 |> Seq.toList
            let cs2 = s2 |> Seq.toList

            let charPairs = List.zip cs1 cs2

            let charPairsWithSep =
                charPairs
                |> List.map (fun (c1, c2) -> (c1, c1 = c2))

            let numberOfCharsThatDiffer =
                charPairsWithSep
                |> List.where (fun (_, sameChar) -> not sameChar)
                |> List.length

            (numberOfCharsThatDiffer, charPairsWithSep)

        let (_, info) =
            List.allPairs xs xs
            |> List.map (fun (s1, s2) -> separation s1 s2)
            |> List.find (fun (numberOfCharsThatDiffer, _) -> numberOfCharsThatDiffer = 1)

        info
            |> List.where (fun (_, same) -> same)
            |> List.map (fun (c, _) -> c)
            |> List.fold (fun acc curr -> acc + (string curr)) ""

    let show () =
        showDay
            2
            part1 (Some 6642)
            part2 (Some "cvqlbidheyujgtrswxmckqnap")
