open System.IO
open System

// https://stackoverflow.com/questions/1222185/most-elegant-combinations-of-elements-in-f
let rec comb n l = 
    match n, l with
    | 0, _ -> [[]]
    | _, [] -> []
    | k, (x::xs) -> List.map ((@) [x]) (comb (k-1) xs) @ comb k xs

let parseInput (input: string) =
    let splits = input.Split('\n', StringSplitOptions.RemoveEmptyEntries)
    Array.map int64 splits

let rec solution1 (startIndex:int) (preambleLength:int) (numbers:int64 array) =
    let currentIndex = startIndex + preambleLength
    let previousNumbers = numbers.[startIndex..currentIndex - 1]
    let currentNumber = numbers.[currentIndex]
    let hasPair = Array.toList previousNumbers
                  |> comb 2
                  |> List.tryFind (fun x -> (List.sum x) = currentNumber)
                  |> Option.isSome
    match (hasPair, currentIndex + 1 < numbers.Length) with
    | (false, _ ) -> Some(currentNumber)
    | (_, true) -> solution1 (startIndex + 1) preambleLength numbers
    | _ -> None

let findSlice (searchNumber:int64) (numbers: int64 array) =
    let slidingWindows = seq { for windowLength in 2..numbers.Length-1 do 
                                 for i in 0 .. (numbers.Length-windowLength) -> numbers.[i..(i+windowLength-1)] }
    Seq.tryFind (fun x -> Seq.sum x = searchNumber) slidingWindows
    |> Option.map (fun sequence -> (Seq.min sequence) + (Seq.max sequence))

let dataExample = File.ReadAllText "input/09_example"
let sol1Example = parseInput dataExample |> solution1 0 5

let data = File.ReadAllText "input/09"
let sol1 = parseInput data |> solution1 0 25
let sol2 = sol1 |> Option.map (fun x -> findSlice x (parseInput data)) |> Option.flatten
