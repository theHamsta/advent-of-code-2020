open System.IO
open System

let tee thing =
    printfn "%A" thing
    thing

let reduceAnswers reduceSetFn (group: string) =
    group.Split("\n", StringSplitOptions.RemoveEmptyEntries)
    |> Array.map set
    |> Array.reduce reduceSetFn

let solution1 (text: string) =
    text.Split("\n\n", StringSplitOptions.RemoveEmptyEntries)
    |> Array.sumBy ((reduceAnswers Set.union) >> Set.count >> bigint)

let solution2 (text: string) =
    text.Split("\n\n", StringSplitOptions.RemoveEmptyEntries)
    |> Array.sumBy ((reduceAnswers Set.intersect) >> Set.count >> bigint)

#if INTERACTIVE
let data = File.ReadAllText "input/06"

let sol1 = solution1 data
let sol2 = solution2 data
#else

[<EntryPoint>]
let main argv =
    match argv with
    | [| file |] ->
        let data = File.ReadAllText file

        printfn "Solution1: %A" (solution1 data)
        printfn "Solution2: %A" (solution2 data)

        0
    | _ ->
        printfn "Invalid number of arguments: %A" argv
        -1
#endif
