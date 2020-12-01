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

let solution2 (data) = data
                      |> Seq.allPairs data
                      |> Seq.find (fun (a,b) -> a+b = 2020)
                      |> fun (a, b) -> a*b


[<EntryPoint>]
let main argv =
   match argv with
      | [|file|] -> let data = getData file
                    printfn "%i" (solution2 data)
                    0
      | _ -> printfn "Invalid number of arguments: %A" argv
             -1
