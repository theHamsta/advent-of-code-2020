open System.IO
open System

type PuzzleInput =
    { startTime: int64
      busses: int64 option array }

let rec f n x a = 
    if x = n then
        x::a
    elif n % x = 0L then 
        f (n/x) x (x::a)
    else
        f n (x+1L) a
let factorise n = f n 2L []

let enumerate s = Seq.mapi (fun i b -> (int64 i, b)) s

let parseInput (text: string) =
    let splits =
        text.Split("\n", StringSplitOptions.RemoveEmptyEntries)

    { startTime = int64 splits.[0] |> int64
      busses =
          splits.[1]
              .Split(",", StringSplitOptions.RemoveEmptyEntries)
          |> Array.map (fun b ->
              try
                  Some(int64 b)
              with _ -> None)
          |> Array.map (Option.map int64) }

let calcTime startTime busId =
    let mutable time = busId
    while time < startTime do
        time <- time + busId
    time - startTime

//[333503; 3]
let solution1 (parsed:PuzzleInput) = 
    let availableBusses = parsed.busses |> Seq.filter Option.isSome |> Seq.map Option.get
    availableBusses
    |> Seq.map (fun b -> (calcTime parsed.startTime b), b)
    |> Seq.reduce (fun (a, argA) (b, argB) -> if a < b then (a,argA) else (b,argB))
    |> (fun (time, bus) -> time * bus)

// https://stackoverflow.com/questions/1222185/most-elegant-combinations-of-elements-in-f
let rec comb n l = 
    match n, l with
    | 0, _ -> [[]]
    | _, [] -> []
    | k, (x::xs) -> List.map ((@) [x]) (comb (k-1) xs) @ comb k xs

let numberGenerator solutions prevPeriod curPeriod =
    seq { for s in solutions do
                                let mutable t = 1L
                                let mutable n = s + t * prevPeriod
                                while n < curPeriod do
                                    t <- t + 1L
                                    n <- s + t * prevPeriod
                                    yield n
                    }

let rec solution2 (busSpec: (int64*int64) array) index prevSolutions prevPeriod = 
    if index = busSpec.Length - 1 then
        Seq.head prevSolutions
    else
        printfn "%i %A" index prevSolutions
        let moreBusses = busSpec.[0..index+1]
        let curPeriod = Array.fold (fun a (_,b)  -> a*b) 1L moreBusses
        let solutions = numberGenerator prevSolutions prevPeriod curPeriod
                        |> Seq.filter (fun t -> Array.forall (fun (k,v) -> ((k + t) % v = 0L)) moreBusses)
                        |> List.ofSeq
        solution2 busSpec (index + 1) solutions curPeriod

let data = File.ReadAllText "input/13"
let parsed = parseInput data
let sol1 = solution1 parsed 
let busSpec = parsed.busses
              |> enumerate
              |> Seq.filter (snd >> Option.isSome)
              |> Seq.map (fun (i, b) -> i, Option.get b)
              |> Array.ofSeq

let product = Array.fold (fun a (_,b)  -> a*b) 1L busSpec
let sol2 = solution2 busSpec 2 [1L] 1L
//max: 3048743993142809
//min: 100000000000000
