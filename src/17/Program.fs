open System.IO
open System
open System.Collections.Generic
type FieldDict = HashSet<int * int * int * int>

type Field =
    { fieldDict: FieldDict
      min: int * int * int * int 
      max: int * int * int * int } with
    member f.CountActive = f.fieldDict.Count

let parseLine (field: Field) (y: int) (line: string) =
    for x in 0 .. line.Length - 1 do
        match line.Chars(x) with
        | '#' -> ignore(field.fieldDict.Add(x, y, 0, 0))
        | '.' -> ()
        | c -> failwith (sprintf "Invalid char %A" c)

let swap (left : 'a byref) (right : 'a byref) =
      let temp = left
      left <- right
      right <- temp

let OFFSETS = seq { for dx in -1..1 do for dy in -1..1 do for dz in -1..1 do for dw in -1..1 do if dx = 0 && dy = 0 && dz = 0 && dw = 0 then () else yield (dx,dy,dz,dw) } |> List.ofSeq

let step (src: Field) (dst: Field) = 
    let (a,b,c,d) = src.min
    let (A,B,C,D) = src.max
    for x in a-1..A+1 do
        for y in b-1..B+1 do
            for z in c-1..C+1 do
                for w in d-1..D+1 do
                    let previous = src.fieldDict.Contains((x,y,z,w))
                    let mutable neighbors = 0
                    for (dx,dy,dz,dw) in OFFSETS do
                        let offset = (x+dx, y+dy,z+dz,w+dw)
                        if src.fieldDict.Contains(offset) then neighbors <- neighbors + 1 else ()

                    let next = match (previous, neighbors) with
                                | (true, n) when n = 2 || n = 3 -> true
                                | (false, n) when n = 3 -> true
                                | _  -> false

                    if next then
                        dst.fieldDict.Add((x,y,z,w))
                    else
                        dst.fieldDict.Remove((x,y,z,w))
    { dst with min=(a-1,b-1,c-1,d-1); max=(A+1,B+1,C+1,D+1)}


let run (sourceField:Field) numSteps =
    let mutable dst = { sourceField with fieldDict = FieldDict() }
    let mutable src = sourceField
    //printfn "Round 0: %i active" src.CountActive
    for _ in 1..numSteps do
        dst <- step src dst
        swap &src &dst

    src

let mutable field =
    { fieldDict = HashSet<int * int * int * int>()
      min = (0,0,0,0)
      max = (0,0,0,0) }

ignore (
    (File.ReadAllText "input/17")
        .Split("\n", StringSplitOptions.RemoveEmptyEntries)
    |> Array.fold
        (fun y line ->
            parseLine field y line
            y + 1)
        0
)

field <- {field with max = (Seq.fold (fun (a,b,c,d) (x,y,z,w) -> (max a x, max b y, max c z, max d w)) field.max field.fieldDict) }
    
let final1 = (run field 6).CountActive
