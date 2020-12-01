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
           for j in 0..data.Length-2 do
               for k in i+1..data.Length-1 do 
                   match data.[i] + data.[j] + data.[k] with
                       | 2020 -> yield data.[i] * data.[j] * data.[k]
                       | _ -> ignore ()
    }

let solution1 (data) = data
                       |> Seq.toList 
                       |> loopSeq
                       |> Seq.head


[<EntryPoint>]
let main argv =
   match argv with
      | [|file|] -> let data = getData file
                    printfn "%i" (solution1 data)
                    0
      | _ -> printfn "Invalid number of arguments: %A" argv
             -1
