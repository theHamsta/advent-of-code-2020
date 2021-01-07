open System.IO
open System
open System.Numerics
open System.Text.RegularExpressions

type SetInstruction = { address: uint64; value: uint64 }
type MaskInstruction = { mask: uint64; value: uint64 }

type State =
    { registers: Map<uint64, uint64>
      mask: MaskInstruction }

type FloatingState =
    { registers: Map<uint64*uint64, uint64*uint64>;
      invalidRegisters: Map<uint64, uint64>;
      mask: MaskInstruction }

type Instruction =
    | SetOp of SetInstruction
    | MaskOp of MaskInstruction

let tee thing=
    printfn "%A" thing
    thing

let teeX thing=
    printfn "%X" thing
    thing

let enumerate s = Seq.mapi (fun i b -> (int64 i, b)) s

let (|Regex|_|) pattern input =
    let m = Regex.Match(input, pattern)

    if m.Success
    then Some(List.tail [ for g in m.Groups -> g.Value ])
    else None

let parseMask (maskString: string) =
    Seq.foldBack (fun c (i, inst) ->
        match c with
        | '0' -> i + 1UL, inst
        | '1' -> i + 1UL, { inst with MaskInstruction.value = inst.value ||| (1UL <<< int32 i) }
        | 'X' -> i + 1UL, { inst with MaskInstruction.mask = inst.mask &&& ~~~(1UL <<< int32 i) }
        | _ -> failwith "Invalid char") maskString (0UL, { mask = UInt64.MaxValue; value = 0UL })
    |> snd

let parseLine (line:string) =
    try 
        match line with
        | Regex @"mask = ([01X]+)" [mask] -> parseMask mask |> MaskOp |> Some
        | Regex @"mem\[(\d+)\] = (\d+)" [address; value] -> {address=uint64 address; value=uint64 value} |> SetOp |> Some
        | _ -> None
    with _ -> None

let allZero number =
    (number &&& ((((1UL <<< 35) - 1UL)))) = 0UL
    
let allOne number =
    ~~~number |> allZero

let anyOne = allZero >> not
    
let printMapEntry (address,mask) =
    for i in 0..35 do
        match ((address >>> (35 - i)) &&& 1UL, (mask>>> (35 - i))  &&& 1UL) with
            | _, 1UL -> printf "X"
            | a, 0UL -> printf "%i" a
            | _ -> failwith "???"
    printf "\n"

let setWithMask mask nonMaskValue =
    (mask.value &&& mask.mask) ||| (nonMaskValue &&& ~~~mask.mask)

let setAddresses (mask:MaskInstruction) address value (map: Map<uint64*uint64,uint64*uint64>) (invalidMap: Map<uint64,uint64>) =
    let floating = ~~~(mask.mask)
    let newAddress = mask.value ^^^ address
    let mutable newInvalidMap = invalidMap
    map
            |> Map.map (fun (value,_) (oldAddress, oldFloating) -> 
                let overwriteSomeBits = ~~~(oldAddress ^^^ newAddress) ||| floating ||| oldFloating
                let overwriteAllBits = ~~~(oldAddress ^^^ newAddress) ||| floating
                if allOne overwriteSomeBits then
                    if allOne overwriteAllBits then
                        (0UL, 0UL)
                    else 
                        printfn "old %x new %x overwriteSomeBits %x overwriteAllBits %X" oldAddress newAddress overwriteSomeBits overwriteAllBits
                        printMapEntry (oldAddress, oldFloating)
                        printMapEntry (newAddress, floating)
                        printfn "%i" value
                        let newFloating = oldFloating &&& ~~~floating
                        let newAddressForOld = (oldAddress &&& ~~~oldFloating) ||| (~~~newAddress &&& oldFloating)
                        printMapEntry (newAddressForOld, newFloating)
                        newInvalidMap <- newInvalidMap.Add(value, (newAddressForOld &&& ~~~newFloating) ||| (newAddress &&& newFloating))
                        (newAddressForOld, ~~~newFloating)
                else 
                    (oldAddress, oldFloating))
    |> Map.add (value, newAddress) (newAddress, floating), newInvalidMap

let parseInput (text: string) =
    text.Split("\n", StringSplitOptions.RemoveEmptyEntries)
    |> Seq.map parseLine
    |> Seq.filter Option.isSome
    |> Seq.map Option.get
    
let executeInstruction (state: State) (instruction: Instruction) =
    match instruction with
        | SetOp inst -> { state with registers = state.registers.Add(inst.address, setWithMask state.mask inst.value)  }
        | MaskOp inst -> { state with mask = inst }

let solution1 (instructions: Instruction seq) initialState =
    let finalState = instructions |> Seq.fold executeInstruction initialState 
    finalState.registers
    |> Map.fold (fun acc _ v  -> acc + bigint v) 0I

let executeInstruction2 (state: FloatingState) (instruction: Instruction) =
    match instruction with
        | SetOp inst -> let (newRegisters, newInvalids) = setAddresses state.mask inst.address inst.value state.registers state.invalidRegisters
                        { state with registers = newRegisters; invalidRegisters = newInvalids }
        | MaskOp inst -> { state with mask = inst }

let solution2 (instructions: Instruction seq) initialState =
    let finalState = instructions |> Seq.fold executeInstruction2 initialState 
    printfn "%A" finalState.invalidRegisters
    let sum = finalState.registers |> Map.fold (fun acc k (address,floating) -> acc + if address = 0UL then 0UL else (snd k) * (max 1UL (uint64(1UL <<< (BitOperations.PopCount floating))))) 0UL
    let otherSum = finalState.invalidRegisters |> Map.fold (fun acc k _ -> acc + k) 0UL 
    sum - otherSum
    finalState

let data = File.ReadAllText "input/14_example2"
let instructions = parseInput data
let sol1 = solution1 instructions {registers=Map.empty; mask = { mask = 0UL; value = 0UL }}
let sol2 = solution2 instructions {registers=Map.empty; invalidRegisters=Map.empty; mask = { mask = 0UL; value = 0UL }}

//let product =
//Array.fold (fun a (_, b) -> a * b) 1L busSpec
