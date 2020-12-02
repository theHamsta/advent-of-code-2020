open System.IO

let readLines (filePath:string) = seq {
    use sr = new StreamReader (filePath)
    while not sr.EndOfStream do
        yield sr.ReadLine ()
}


type PasswordRecord = { min: int; max: int; char_: char; password: string }


let parse (line:string) = let splits = line.Split [|':';' ';'-'|]
                          try Some({
                                 min= int splits.[0];
                                 max= int splits.[1];
                                 char_= char splits.[2];
                                 password = splits.[4];
                          })
                          with _ -> None

let validate record = let count = (record.password.Split [|record.char_|]).Length - 1
                      count >= record.min && count <= record.max

let validate2 record = (record.password.Chars (record.min - 1) = record.char_) <>
                       (record.password.Chars (record.max - 1) = record.char_)

let getData file = (readLines file)
                   |> Seq.map parse
                   |> Seq.filter Option.isSome
                   |> Seq.map Option.get

let printInput data = Seq.map (printfn "%A") data
                      |> ignore

let solution data = data
                    |> Seq.filter validate
                    |> Seq.length

let solution2 data = data
                    |> Seq.filter validate2
                    |> Seq.length

[<EntryPoint>]
let main argv =
   match argv with
      | [|file|] -> let data = getData file
                    printInput data
                    printfn "%i" (Seq.length data)
                    printfn "solution1 %i" (solution data)
                    printfn "solution2 %i" (solution2 data)
                    0
      | _ -> printfn "Invalid number of arguments: %A" argv
             -1
