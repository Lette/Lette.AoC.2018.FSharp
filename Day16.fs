module Day16
    open Common

    let value    v _      = v
    let register r device = Array.get device r
    let unused   _ _      = 0

    let boolToInt b = if b then 1 else 0
    let helper transformer comparison a b = comparison a b |> transformer
    let helper' = helper boolToInt
    let gt = helper' (>)
    let eq = helper' (=)
    let first a _ = a

    let createZeroDevice () = Array.create 4 0
    let createDevice r0 r1 r2 r3 = [| r0; r1; r2; r3 |]

    let setRegister device register value =
        let device' = Array.copy device
        Array.set device' register value
        device'

    let devicePattern = @"(?:Before: |After:  )\[(\d+), (\d+), (\d+), (\d+)]"
    let instructionPattern = @"(\d+) (\d+) (\d+) (\d+)"

    let toDevice =
        function
        | Regex devicePattern [r0; r1; r2; r3] -> createDevice (int r0) (int r1) (int r2) (int r3)
        | s                                    -> failwithf "could not parse device: %s" s

    let toInstruction =
        function
        | Regex instructionPattern [i0; i1; i2; i3] -> (int i0, int i1, int i2, int i3)
        | s                                         -> failwithf "could not parse instruction: %s" s

    let initialData =
        lazy
        (
            let rows =
                Day16Data.d1 
                |> splitRows
                |> Array.toList

            let rec inner rows acc =
                match rows with
                | []                         -> acc
                | "" :: rs                   -> inner rs acc
                | row1 :: row2 :: row3 :: rs -> inner rs ((toDevice row1, toInstruction row2, toDevice row3) :: acc)
                | _                          -> failwith "could not parse input"
            
            let opcodes =
                [
                    ("addr", register, register, (+))
                    ("addi", register, value,    (+))
                    ("mulr", register, register, (*))
                    ("muli", register, value,    (*))
                    ("banr", register, register, (&&&))
                    ("bani", register, value,    (&&&))
                    ("borr", register, register, (|||))
                    ("bori", register, value,    (|||))
                    ("setr", register, unused,   first)
                    ("seti", value,    unused,   first)
                    ("gtir", value,    register, gt)
                    ("gtri", register, value,    gt)
                    ("gtrr", register, register, gt)
                    ("eqir", value,    register, eq)
                    ("eqri", register, value,    eq)
                    ("eqrr", register, register, eq)
                ]

            let instructions =
                Day16Data.d2 
                |> splitRows
                |> Array.toList
                |> List.map toInstruction

            (inner rows [], opcodes, instructions)
        )

    let testOpcode (deviceBefore, (opcodeNumber, a, b, c), deviceAfter) (opcodeName, p1, p2, impl) =
        let a' = p1 a deviceBefore
        let b' = p2 b deviceBefore
        let result = impl a' b'

        let actualDevice = setRegister deviceBefore c result

        if actualDevice = deviceAfter then
            []
        else
            [(opcodeNumber, opcodeName)]

    let rec testOpcodes input opcodes impossibles =
        match opcodes with
        | []      -> 16 - (impossibles |> List.length), impossibles
        | o :: os -> testOpcodes input os ((testOpcode input o) @ impossibles)

    let rec runInput input opcodes accNum accImpossibles =
        match input with
        | [] -> accNum, accImpossibles
        | i :: is ->
            let numberOfPossibles, impossibles = testOpcodes i opcodes []
            let accNum' = numberOfPossibles :: accNum
            let accImpossibles' = impossibles @ accImpossibles
            runInput is opcodes accNum' accImpossibles'

    let showMappings p a =
        p
        |> List.groupBy fst
        |> List.sortBy fst
        |> List.map (fun (k, vs) -> (k, vs |> List.map snd))
        |> List.iter (fun (number, names) ->
            printf "%2i: " number
            names |> List.sort |> List.iter (printf "%s ")
            printfn ""
        )
        printfn ""
        a
        |> List.sortBy fst
        |> List.iter (fun (number, name) -> printfn "%2i: %s" number name)
        printfn ""

    let reduceSinglePossibility acc possibles =
        //showMappings possibles acc
        let number, name =
            possibles
            |> List.groupBy fst
            |> List.find (fun (_, v) -> v |> List.length = 1)
            |> fun (k, vs) -> (k, vs |> List.head |> snd)

        let acc' =
            (number, name) :: acc
        let possibles' =
            possibles
            |> List.filter (fun (nu, na) -> nu <> number && na <> name)

        (acc', possibles')

    let rec reducePossibles acc possibles =
        match possibles with
        | [] -> acc
        | _  ->
            (acc, possibles)
            ||> reduceSinglePossibility
            ||> reducePossibles

    let findOpcodeByName opcodes name =
        opcodes
        |> List.find (fun (n, _, _, _) -> n = name)

    let findOpcodeMappings opcodes impossibles =
        let allNames =
            opcodes
            |> List.map (fun (name, _, _, _) -> name)
        
        let allTuples =
            [
                for number in 0 .. 15 do
                for name in allNames do
                    yield (number, name)
            ]

        allTuples
        |> List.except impossibles
        |> reducePossibles []
        |> List.map (fun (number, name) -> (number, findOpcodeByName opcodes name))
        |> Map.ofList

    let runInstruction opcodeMappings (n, a, b, c) device =
        let (_, p1, p2, impl) = opcodeMappings |> Map.find n
        let a' = p1 a device
        let b' = p2 b device
        let result = impl a' b'
        setRegister device c result

    let rec runInstructions opcodeMappings instructions device =
        match instructions with
        | []      -> device
        | i :: is -> runInstructions opcodeMappings is (runInstruction opcodeMappings i device)

    let part1 () =
        let (input, opcodes, _) = initialData.Value

        runInput input opcodes [] []
        |> fst
        |> List.filter ((<=) 3)
        |> List.length

    let part2 () =
        let (input, opcodes, instructions) = initialData.Value

        let opcodeMappings =
            runInput input opcodes [] []
            |> snd
            |> List.distinct
            |> findOpcodeMappings opcodes
        
        runInstructions opcodeMappings instructions (createZeroDevice ())
        |> fun device -> Array.get device 0

    let show () =
        showDay
            16
            part1 (Some 677)
            part2 (Some 540)
