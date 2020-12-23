// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System

let input = "193467258" |> List.ofSeq |> List.map (int >> (fun x -> x - int '0'))
let example = "389125467" |> List.ofSeq |> List.map (int >> (fun x -> x - int '0'))

let playGame numRounds startState =
    let mutable state = startState
    for i in 1..numRounds do 
        let current = List.head state
        let pickUp = state |> List.skip 1|> List.take 3
        let rest = state |> List.skip 4
        let dest = (seq { for i in 1..4 -> (current - i - 1 + 9) % 9 + 1 } |> List.ofSeq) |> Seq.find (fun x -> x <> pickUp.[0] && x <> pickUp.[1] && x <> pickUp.[2])
        let pos = Seq.findIndex (fun x -> x = dest) rest

        printfn "Round %i" i
        printfn "State %A" state
        printfn "pickUp %A" pickUp
        printfn "dest %A" dest
        printfn ""
        state <- List.concat [(List.take pos rest);[dest];pickUp;(List.skip (pos + 1) rest);[current]]

    state |> List.append state
    |> List.skipWhile (fun x -> x <> 1) |> List.skip 1 |> List.take 8
    

                                                                                                        
let solution1_example = example |> playGame 100 |> Seq.map string |> String.concat ""
let solution1 = input |> playGame 100 |> Seq.map string |> String.concat ""
