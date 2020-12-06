open System.IO
open System

let tee thing =
    printfn "%A" thing
    thing

let reduceAnswers reduceFn (group: string) =
    group.Split("\n", StringSplitOptions.None)
    |> Array.map (set >> tee)
    |> Array.filter (Set.isEmpty >> not)
    |> Array.reduce reduceFn

let solution1 (text: string) =
    text.Split("\n\n", StringSplitOptions.None)
    |> Array.sumBy ((reduceAnswers Set.union) >> Set.count >> bigint)

let solution2 (text: string) =
    text.Split("\n\n", StringSplitOptions.None)
    |> Array.sumBy ((reduceAnswers Set.intersect) >> Set.count >> tee >> bigint)

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
