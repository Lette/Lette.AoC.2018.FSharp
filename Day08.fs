module Day08
    open Common

    let xs =
        Day08Data.d.Split ' '
        |> Array.toList
        |> List.map int

    type Node = { Children : Node list; Metadata : int list}

    let buildNodes xs =

        let rec createNode xs =

            let rec createChildren n acc data =
                match n with
                | 0 -> (acc, data)
                | k ->
                    let child, remaining = createNode data
                    createChildren (n - 1) (child :: acc) remaining

            let rec createMetadata n acc data =
                match n, data with
                | 0, _       -> (acc, data)
                | k, d :: ds -> createMetadata (k - 1) (d :: acc) ds
                | _          -> failwith "metadata no fully built"

            match xs with
            | nChild :: nMeta :: ys ->
                let children, remaining = createChildren nChild [] ys
                let metadata, remaining' = createMetadata nMeta [] remaining
                ({ Children = children; Metadata = metadata }, remaining')
            | _ -> failwith "child not fully built"

        createNode xs |> fst

    let rec nodeToList node =
        seq {
            yield node
            for child in node.Children do yield! nodeToList child;
        }

    let part1 () =
        buildNodes xs
        |> nodeToList
        |> Seq.collect (fun n -> n.Metadata)
        |> Seq.sum

    let part2 () =
        0

    let show () =
        showDay
            8
            part1 (Some 40977)
            part2 None
