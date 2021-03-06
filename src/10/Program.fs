open System.IO
open System
open System.Collections.Generic

let solution1 sortedArray = 
    Seq.fold (fun ((diff1, diff3), prev) cur -> match (cur - prev) with
                                                  | 1 -> ((diff1 + 1), diff3), cur
                                                  | 3 -> ((diff1), diff3 + 1), cur
                                                  | _ -> failwith (sprintf "Impossible %i %i" prev cur)) ((0,0), 0) sortedArray
    |> fun ((a, b), _) -> (a * (b+1))

let rec solution2 (numbers: Set<int>) (goal: int) (cache: Dictionary<int,int64>) (prev: int) = 
    let contains i = numbers.Contains(prev + i)
    let nextStep i = if cache.ContainsKey(i) then cache.[i]
                     else let result = (solution2 numbers goal cache i)
                          cache.[i] <- result
                          result
    if prev + 3 = goal then 1L
    else seq { for i in 1..3 -> i}
             |> Seq.filter contains
             |> Seq.sumBy (fun i -> nextStep (prev + i))

let parseInput (input: string) =
    input.Split('\n', StringSplitOptions.RemoveEmptyEntries)
    |> Array.map int

let data = File.ReadAllText "input/10"
let parsed = parseInput data |> Array.sort
let sol1 = solution1 parsed
let sol2 = solution2 (set parsed) (Array.max parsed + 3) (Dictionary()) 0

/// Alternative: differences on sorted array
let rec solution2Alt (sortedArray: int array) (cache: Dictionary<int*int,int64>) prev index = 
    let cur = sortedArray.[index]
    let cachedCall a b = if cache.ContainsKey((a,b)) then
                            cache.[(a,b)]
                         else
                            let result = (solution2Alt sortedArray cache a b)
                            cache.[(a,b)] <- result
                            result

    let take () = cachedCall cur (index + 1)
    let notTake () = cachedCall prev (index + 1)
    let isLast () = sortedArray.Length = index + 1
    match cur - prev with
        | 1
        | 2
        | 3 -> if isLast() then 1L else take() + notTake()
        | _ -> 0L

let sol22 = solution2Alt parsed (Dictionary()) 0 0
