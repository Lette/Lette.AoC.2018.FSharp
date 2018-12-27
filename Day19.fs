module Day19
    open Common
    open Checked

    let configPattern = @"#ip (\d+)"
    let instructionPattern = @"([a-z]{4}) (\d+) (\d+) (\d+)"

    let toConfig =
        function
        | Regex configPattern [ ip ] -> int ip
        | s -> failwithf "could not parse input: %s" s

    let toInstruction =
        function
        | Regex instructionPattern [ instr; a; b; c ] -> (instr, int a, int b, int c)
        | s -> failwithf "could not parse input: %s" s

    let createZeroDevice () = Array.create 6 0

    let setRegister register value device =
        let device' = Array.copy device
        Array.set device' register value
        device'
    let getRegister register device =
        Array.get device register

    let initialData =
        lazy (
            let input =
                Day19Data.d
                |> splitRows
                |> Array.toList
              
            match input with
            | [] -> failwith "what? no input???"
            | configInput :: instructionsInput ->

                let config =
                    configInput
                    |> toConfig

                let value    v _      = v
                let register r device = Array.get device r
                let unused   _ _      = 0

                let first a _ = a
                let gt a b = if a > b then 1 else 0
                let eq a b  = if a = b then 1 else 0

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

                let findOpcode (name, _, _, _) opcodes =
                    Map.find name opcodes

                let preloadOpcode (name, a, b, c) =
                    let (p1, p2, impl) = Map.find name opcodes
                    (a, b, c, p1, p2, impl)

                let instructions =
                    instructionsInput
                    |> List.map (toInstruction >> preloadOpcode)
                    |> List.toArray

                (config, instructions)
        )

    let runInstruction (a, b, c, p1, p2, impl) device =
        let a' = p1 a device
        let b' = p2 b device
        let result = impl a' b'
        device |> setRegister c result

    let rec step config instructions device =
        //printfn "%A" (device)
        let ip = getRegister config device + 1

        if ip >= Array.length instructions then
            device
        else
            let instruction = Array.get instructions ip

            device
            |> setRegister config ip
            |> runInstruction instruction
            |> step config instructions

    let part1 () =
        let (config, instructions) = initialData.Value
        
        createZeroDevice ()
        |> setRegister config -1
        |> step config instructions
        |> getRegister 0

    let part2 () =

        10551364
        |> divisors
        |> Seq.sum

    let show () =
        showDay
            19
            part1 (Some 1694)
            part2 (Some 18964204)
