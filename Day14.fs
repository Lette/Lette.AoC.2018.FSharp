module Day14
    open Common

    let digits n =
        if n < 10uy then
            [n]
        else
            [1uy; (n - 10uy) ]

    let createArray size =
        let arr = Array.create size 0uy
        arr.[0] <- 3uy
        arr.[1] <- 7uy
        arr

    let step (arr : byte []) pos1 pos2 length =
        let sum = arr.[pos1] + arr.[pos2]
        let length' =
            if sum < 10uy then
                arr.[length] <- sum
                length + 1
            else
                arr.[length] <- 1uy
                arr.[length + 1] <- sum - 10uy
                length + 2
        let pos1' = (pos1 + 1 + int arr.[pos1]) % length' 
        let pos2' = (pos2 + 1 + int arr.[pos2]) % length'

        (pos1', pos2', length')

    let part1 () =
        let size = 190221 + 10
        let arr = createArray (size + 1)

        let rec inner pos1 pos2 length = 
            if length >= size then
                arr.[size - 10 .. size - 1]
                |> Array.fold (fun acc curr -> 10 * acc + int curr) 0
            else
                step arr pos1 pos2 length |||> inner

        inner 0 1 2

    let part2 () =
        let size = 200000000
        let arr = createArray size
        let result = [| 1uy; 9uy; 0uy; 2uy; 2uy; 1uy |]
        let resultLength = result |> Array.length

        let rec inner pos1 pos2 length = 

            let pos1', pos2', length' = step arr pos1 pos2 length

            if length' >= resultLength && arr.[length' - resultLength .. length' - 1] = result then
                length' - resultLength
            else if length' >= (resultLength+1) && (length' - length = 2) && arr.[length' - resultLength - 1 .. length' - 2] = result then
                length' - resultLength - 1
            else
                inner pos1' pos2' length'

        inner 0 1 2

    let show () =
        showDay
            14
            part1 (Some 1191216109)
            part2 (Some 20268576)
