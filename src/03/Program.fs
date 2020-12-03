open System.IO

let readLines (filePath:string) = seq {
    use sr = new StreamReader (filePath)
    while not sr.EndOfStream do
        yield sr.ReadLine ()
}

type Field = Snow | Tree


let parseLine (line:string) = line.ToCharArray()
                              |> Array.map (fun c -> match c with
                                                         | '#' -> Tree
                                                         | _ -> Snow)
                                        
let getData file = readLines file
                   |> Seq.map parseLine

let solution (data:seq<array<Field>>) = data
                                        |> Seq.mapi (fun row line -> line.[row*3 % line.Length])
                                        |> Seq.filter (fun field -> field = Tree)
                                        |> Seq.length

let countTrees (data:seq<array<Field>>) moveFn rowPredicate = data
                                                              |> Seq.mapi (fun row line -> line.[(moveFn row) % line.Length], (rowPredicate row))
                                                              |> Seq.filter (fun (field, validLine) -> field = Tree && validLine)
                                                              |> Seq.length

let solution2 data = let moveOffsets = [(1,1); (3,1); (5,1); (7,1); (1,2)]
                     let moveFn dx dy row = dx * (row / dy)
                     let rowPredicate dy row = row % dy = 0
                     moveOffsets
                     |> List.map (fun (dx,dy) -> countTrees data (moveFn dx dy) (rowPredicate dy))
                     |> List.map bigint // Why are we still using int32?
                     |> Seq.reduce (*)

[<EntryPoint>]
let main argv =
   match argv with
      | [|file|] -> let data = getData file
                    printfn "%i" (Seq.length data)
                    printfn "solution1 %i" (solution data)
                    printfn "solution2 %A" (solution2 data)
                    0
      | _ -> printfn "Invalid number of arguments: %A" argv
             -1
