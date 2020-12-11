open System.IO
open System

type SeatState =
    | Empty
    | Occupied
    | Floor

let parseLine (line: string) =
    line
    |> Seq.map (fun c -> match c with
                         | 'L' -> Empty
                         | '.' -> Floor
                         | _ -> failwith "invalid char")
    |> Array.ofSeq

let swap (left : 'a byref) (right : 'a byref) =
  let temp = left
  left <- right
  right <- temp

let findSeat1 (src: SeatState array array) x y dx dy =
    src.[y+dy].[x+dx]

let findSeat2 (src: SeatState array array) x y dx dy =
    let mutable nextSeat = Floor
    let mutable x_ = x
    let mutable y_ = y
    let width = src.[0].Length
    let height = src.Length
    while nextSeat = Floor && x_+ dx >= 0 && y_ + dy >= 0 && x_ + dx < width && y_ + dy < height do
        x_ <- x_ + dx
        y_ <- y_ + dy
        nextSeat <- src.[y_].[x_]
    nextSeat

let update findSeatFn neighborTolerance (src: SeatState array array) (dst: SeatState array array) =
    let mutable changes = false
    for y in 0..(src.Length-1) do
        for x in 0..(src.[0].Length-1) do
            let mutable occupied = 0
            let current = src.[y].[x]
            for dy in -1..1 do
                for dx in -1..1 do
                    try 
                        if (dx = 0 && dy = 0) then
                            ()
                        else 
                            match (findSeatFn src x y dx dy) with
                                | Occupied -> occupied <- occupied + 1
                                | _ -> ()
                    with _ -> ()
            match (occupied, current) with
                | (0, Empty) -> dst.[y].[x] <- Occupied
                                changes <- true
                | (c, Occupied) when c >= neighborTolerance -> dst.[y].[x] <- Empty
                                                               changes <- true
                | (_, current) -> dst.[y].[x] <- current
    changes

let run (input: SeatState array array) updateFn =
    let mutable dst = Array.init input.Length (fun y -> Array.copy input.[y])
    let mutable src = Array.init input.Length (fun y -> Array.copy input.[y])
    while updateFn src dst do
        swap &src &dst
    dst

let parseInput (input: string) =
    input.Split('\n', StringSplitOptions.RemoveEmptyEntries)
   |> Array.map parseLine

let data = File.ReadAllText "input/11"
let parsed = parseInput data
let finalState = run parsed (update findSeat1 4)
let finalState2 = run parsed (update findSeat2 5)
let solution1 = Array.sumBy (fun x -> x |> Seq.filter (fun elt -> elt = Occupied) |> Seq.length) finalState
let solution2 = Array.sumBy (fun x -> x |> Seq.filter (fun elt -> elt = Occupied) |> Seq.length) finalState2
