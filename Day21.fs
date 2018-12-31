module Day21
    open Common
    open Day20

    let configPattern = @"#ip (\d+)"
    let instructionPattern = @"([a-z]{4}) (\d+) (\d+) (\d+)"

    let toConfig =
        function
        | Regex configPattern [ ip ] -> int ip
        | s -> failwithf "could not parse input: %s" s

    let toInstruction =
        function
        | Regex instructionPattern [ instr; a; b; c ] -> (instr, int64 a, int64 b, int c)
        | s -> failwithf "could not parse input: %s" s

    let createZeroDevice () = Array.create 6 0L

    let setRegister register value device =
        let device' = Array.copy device
        Array.set device' register value
        device'
    let getRegister register device =
        Array.get device register

    let initialData =
        lazy (
            let input =
                Day21Data.d
                |> splitRows
                |> Array.toList
              
            match input with
            | [] -> failwith "what? no input???"
            | configInput :: instructionsInput ->

                let config =
                    configInput
                    |> toConfig

                let value    v _      = v
                let register r device = Array.get device (int r)
                let unused   _ _      = 0L

                let first a _ = a
                let gt a b = if a > b then 1L else 0L
                let eq a b  = if a = b then 1L else 0L

                let opcodes =
                    [
                        ("addr", (register, register, (+)))
                        ("addi", (register, value,    (+)))
                        ("mulr", (register, register, (*)))
                        ("muli", (register, value,    (*)))
                        ("banr", (register, register, (&&&)))
                        ("bani", (register, value,    (&&&)))
                        ("borr", (register, register, (|||)))
                        ("bori", (register, value,    (|||)))
                        ("setr", (register, unused,   first))
                        ("seti", (value,    unused,   first))
                        ("gtir", (value,    register, gt))
                        ("gtri", (register, value,    gt))
                        ("gtrr", (register, register, gt))
                        ("eqir", (value,    register, eq))
                        ("eqri", (register, value,    eq))
                        ("eqrr", (register, register, eq))
                    ]
                    |> Map.ofList

                let preloadOpcode addr (name, a, b, c) =
                    let (p1, p2, impl) = Map.find name opcodes
                    (addr, name, a, b, c, p1, p2, impl)

                let instructions =
                    instructionsInput
                    |> List.map toInstruction
                    |> List.mapi preloadOpcode
                    |> List.toArray

                let initialDevice =
                    createZeroDevice ()
                    |> setRegister config -1L

                (config, instructions, initialDevice)
        )

    let runInstruction (addr, name, a, b, c, p1, p2, impl) device =
        let a' = p1 a device
        let b' = p2 b device
        let result = impl a' b'
        let device' = device |> setRegister c result

        if addr = 28 then
            (device', Some a')
        else
            (device', None)

    let rec step config instructions device =
        let ip = getRegister config device + 1L
        let instruction = Array.get instructions (int ip)

        let d, i =
            device
            |> setRegister config ip
            |> runInstruction instruction

        match i with
        | None   -> step config instructions d
        | Some m -> (d, m)

    let rec run config instructions device =
        seq {
            let device', result = step config instructions device
            yield result
            yield! run config instructions device'
        }

    let part1 () =
        initialData.Value
        |||> run
        |> Seq.head

    let part2 () =
        let scanner (last, values, result) item =
            if Set.contains item values then
                (item, values, Some last)
            else
                (item, Set.add item values, None)

        initialData.Value
        |||> run
        |> Seq.scan scanner (0L, Set.empty, None)
        |> Seq.map (fun (_, _, result) -> result)
        |> Seq.pick id

    let show () =
        showDay
            21
            part1 (Some 3173684L) 
            part2 (Some 12464363L)
