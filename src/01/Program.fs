// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp


//  let data = [1721
//              979
//              366
//              299
//              675
//              1456]

open System.IO

let readLines (filePath:string) = seq {
    use sr = new StreamReader (filePath)
    while not sr.EndOfStream do
        yield sr.ReadLine ()
}
let getData (file) = (readLines file)
                     |> Seq.map int

let loopSeq (data:list<int>) = seq {
       for i in 0..data.Length-2 do
           for j in i+1..data.Length-1 do 
               match data.[i] + data.[j] with
                   | 2020 -> yield data.[i] * data.[j]
                   | _ -> ignore ()
    }

let solution1 (data) = data
                       |> Seq.toList 
                       |> loopSeq
                       |> Seq.head
                       
let solution2 (data) = data
                      |> Seq.allPairs data
                      |> Seq.find (fun (a,b) -> a+b = 2020)
                      |> fun (a, b) -> a*b


[<EntryPoint>]
let main argv =
   match argv with
      | [|file|] -> let data = getData file
                    printfn "%i" (solution1 data)
                    printfn "%i" (solution2 data)
                    0
      | _ -> printfn "Invalid number of arguments: %A" argv
             -1
