open System.IO

let readLines (filePath:string) = seq {
    use sr = new StreamReader (filePath)
    while not sr.EndOfStream do
        yield sr.ReadLine ()
}
let getData (file) = (readLines file)
                     |> Seq.map int

let loopSeq (data:list<int>) = seq {
       for i in 0..data.Length-3 do
           for j in i+1..data.Length-2 do
               for k in j+2..data.Length-1 do 
                   match data.[i] + data.[j] + data.[k] with
                       | 2020 -> yield data.[i] * data.[j] * data.[k]
                       | _ -> ignore ()
    }

let solution1 (data) = data
                       |> Seq.toList 
                       |> loopSeq
                       |> Seq.head

// https://stackoverflow.com/questions/1222185/most-elegant-combinations-of-elements-in-f
let rec comb n l = 
    match n, l with
    | 0, _ -> [[]]
    | _, [] -> []
    | k, (x::xs) -> List.map ((@) [x]) (comb (k-1) xs) @ comb k xs

let solution2 data = data
                     |> Seq.toList
                     |> comb 3
                     |> List.find (fun x -> List.sum x = 2020)
                     |> List.reduce (*)

[<EntryPoint>]
let main argv =
   match argv with
      | [|file|] -> let data = getData file
                    printfn "%i" (solution1 data)
                    printfn "%i" (solution2 data)
                    0
      | _ -> printfn "Invalid number of arguments: %A" argv
             -1
