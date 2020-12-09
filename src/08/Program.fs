open System.IO
open System
open System.Text.RegularExpressions

type OpCode =
    | NOP
    | JMP
    | ACC

type Instruction = { opCode: OpCode; count: int }

type ProgramResult =
    | InfiniteLoopFailure of int
    | InvalidJumpFailure
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

let executeProgram (program: Instruction array) (flipInstruction: int) =

    let mutable acc = 0
    let mutable currentInstruction = 0
    let mutable visitedInstructions = Set.empty
    let outOfBounds () = currentInstruction < 0 || currentInstruction > program.Length
    let terminated () = currentInstruction = program.Length
    let inInfiniteLoop () = visitedInstructions.Contains(currentInstruction)

    while not (terminated() || inInfiniteLoop() || outOfBounds()) do

        let op = program.[currentInstruction]
        let flip = currentInstruction = flipInstruction

        visitedInstructions <- visitedInstructions.Add(currentInstruction)
        currentInstruction <- currentInstruction + 1

        match (op.opCode, flip) with
        | (NOP, false)
        | (JMP, true) -> ()
        | (ACC, _) -> acc <- acc + op.count
        | (JMP, false)
        | (NOP, true) -> currentInstruction <- currentInstruction + op.count - 1

    match outOfBounds(), inInfiniteLoop() with
    | (true, _) -> InvalidJumpFailure
    | (_, true) -> InfiniteLoopFailure(acc)
    | _ -> Success(acc)


let solution1 program = executeProgram program -1

let solution2 (program: Instruction array) =
    program
    |> Seq.mapi (fun flipInstruction _ -> executeProgram program flipInstruction)
    |> Seq.find (fun result ->
        match result with
        | Success (_) -> true
        | _ -> false)

let data = File.ReadAllText "input/08"

let program =
    parseInput data
    |> Seq.filter Option.isSome
    |> Seq.map Option.get
    |> Seq.toArray

let sol1 = solution1 program
let sol2 = solution2 program
