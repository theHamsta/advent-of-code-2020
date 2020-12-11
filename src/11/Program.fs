open System.IO
open System

type SeatState =
    | Empty
    | Occupied
    | Floor

let parseLine (line: string) =
    line.ToCharArray()
    |> Seq.map (fun c -> match c with
                         | 'L' -> Empty
                         | '.' -> Floor
                         | _ -> failwith "invalid char")
    |> Array.ofSeq

let swap (left : 'a byref) (right : 'a byref) =
  let temp = left
  left <- right
  right <- temp

let update (src: SeatState array array) (dst: SeatState array array) =
    let mutable changes = 0
    for y in 0..(src.Length-1) do
        for x in 0..(src.[0].Length-1) do
            try 
                let mutable occupied = 0
                let current = src.[y].[x]
                for dy in -1..1 do
                    for dx in -1..1 do
                        try 
                            if (dx = 0 && dy = 0) then
                                ()
                            else 
                                match src.[y+dy].[x+dx] with
                                    | Occupied -> occupied <- occupied + 1
                                    | _ -> ()
                        with _ -> ()
                match (occupied, current) with
                    | (0, Empty) -> dst.[y].[x] <- Occupied
                                    changes <- changes + 1
                    | (c, Occupied) when c >= 4 -> dst.[y].[x] <- Empty
                                                   changes <- changes + 1
                    | (_, current) -> dst.[y].[x] <- current
            with _ -> ()
    changes

let findSeat (src: SeatState array array) x y dx dy =
    let mutable nextSeat = Floor
    let mutable x_ = x
    let mutable y_ = y
    let width = src.[0].Length
    let height = src.Length
    while nextSeat = Floor && x >= 0 && y >= 0 && x < width && y < height do
        x_ <- x_ + dx
        y_ <- y_ + dy
        nextSeat <- src.[y_].[x_]
    nextSeat

let update2 (src: SeatState array array) (dst: SeatState array array) =
    let mutable changes = 0
    for y in 0..(src.Length-1) do
        for x in 0..(src.[0].Length-1) do
            try 
                let mutable occupied = 0
                let current = src.[y].[x]
                for dy in -1..1 do
                    for dx in -1..1 do
                        try 
                            if (dx = 0 && dy = 0) then
                                ()
                            else 
                                match (findSeat src x y dx dy) with
                                    | Occupied -> occupied <- occupied + 1
                                    | _ -> ()
                        with _ -> ()
                match (occupied, current) with
                    | (0, Empty) -> dst.[y].[x] <- Occupied
                                    changes <- changes + 1
                    | (c, Occupied) when c >= 5 -> dst.[y].[x] <- Empty
                                                   changes <- changes + 1
                    | (_, current) -> dst.[y].[x] <- current
            with _ -> ()
    changes

let run (input: SeatState array array) updateFn =
    let mutable dst = input
    let mutable src = Array.init input.Length (fun y -> Array.copy input.[y])
    while updateFn src dst > 0 do
        swap &src &dst
    dst

let parseInput (input: string) =
    input.Split('\n', StringSplitOptions.RemoveEmptyEntries)
   |> Array.map parseLine

let data = File.ReadAllText "input/11"
let parsed = parseInput data
let finalState = run parsed update
let parsed2 = parseInput data
let finalState2 = run parsed2 update2
let solution1 = Array.sumBy (fun x -> x |> Seq.filter (fun elt -> elt = Occupied) |> Seq.length) finalState
let solution2 = Array.sumBy (fun x -> x |> Seq.filter (fun elt -> elt = Occupied) |> Seq.length) finalState2
