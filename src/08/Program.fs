open System.IO
open System
open System.Text.RegularExpressions

type OpCode =
    | NOP
    | JMP
    | ACC

type Instruction = { opCode: OpCode; count: int }

type ProgramResult =
    | LoopFailure of int
    | JumpOutOfProgramFailure
    | Success of int

let parseInstruction (line: string) =
    let regex = Regex.Match(line, @"(\w{3}) ([+-]\d+)")

    try
        Some
            ({ opCode =
                   match regex.Groups.[1].Value with
                   | "acc" -> ACC
                   | "jmp" -> JMP
                   | "nop" -> NOP
                   | _ -> failwith "invalid opcode"
               count = int regex.Groups.[2].Value })
    with _ -> None

let parseInput (input: string) =
    input.Split('\n', StringSplitOptions.RemoveEmptyEntries)
    |> Array.map parseInstruction

let executeProgram (flipInstruction: int) (program: Instruction array) =
    let mutable acc = 0
    let mutable currentInstruction = 0
    let mutable visitedInstructions = Set.empty

    while not (visitedInstructions.Contains(currentInstruction))
          && (currentInstruction < program.Length) && (currentInstruction >= 0) do
        let op = program.[currentInstruction]
        visitedInstructions <- visitedInstructions.Add(currentInstruction)
        let flip = currentInstruction = flipInstruction
        currentInstruction <- currentInstruction + 1

        match (op.opCode, flip) with
        | (NOP, false)
        | (JMP, true) -> ()
        | (ACC, _) -> acc <- acc + op.count
        | (JMP, false)
        | (NOP, true) -> currentInstruction <- currentInstruction + op.count - 1

    if currentInstruction = program.Length then Success(acc)
    else if currentInstruction > program.Length || currentInstruction < 0 then JumpOutOfProgramFailure
    else LoopFailure(acc)

let solution1 program =
    program
    |> executeProgram -1

let solution2 (program: Instruction array) =
    seq { for i in 0 .. program.Length - 1 -> i }
        |> Seq.map (fun flipInstruction -> executeProgram flipInstruction program)
        |> Seq.find (fun result -> match result with
                                    | Success(_) -> true
                                    | _ -> false)

let data = File.ReadAllText "input/08"

let program =
    parseInput data
    |> Seq.filter Option.isSome
    |> Seq.map Option.get
    |> Seq.toArray

let sol1 = solution1 program
let sol2 = solution2 program
