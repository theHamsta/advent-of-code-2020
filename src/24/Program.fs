#if INTERACTIVE
#r "nuget: FParsec"
#endif

open System.IO
open System.Collections.Generic
open FParsec

type Direction =
    | E
    | SE
    | SW
    | W
    | NW
    | NE

type Floor = HashSet<(int64 * int64)>

let createParser =
    let pDirection =
        choice [ (pstring "e")
                 (pstring "se")
                 (pstring "sw")
                 (pstring "w")
                 (pstring "nw")
                 (pstring "ne") ]
        |>> (fun d ->
            match d with
            | "e" -> E
            | "se" -> SE
            | "sw" -> SW
            | "w" -> W
            | "nw" -> NW
            | "ne" -> NE
            | d -> failwith (sprintf "Invalid direction: %A" d))

    let pDirections = (many1 pDirection) .>> newline

    let pInput =
        sepEndBy1 (pDirections) (many newline) .>> eof

    (fun text ->
        match run pInput text with
        | Success (ast, _, _) -> ast
        | Failure (err, _, _) -> failwith err)

let step (x,y) direction =
    let (dx,dy) = match direction with
                    | E -> (2L,0L)
                    | SE -> (1L,-1L)
                    | SW -> (-1L,-1L)
                    | W -> (-2L,0L)
                    | NW -> (-1L,1L)
                    | NE -> (1L,1L)
    (x + dx, y + dy)

let NEIGHBORS = [E ; SE ; SW ; W ; NE ; NW] |> List.map (step (0L,0L))

let followInstructions (input:Direction list list) =
    let floor = Floor()

    for sequence in input do
        let coordinate = sequence |> Seq.fold step (0L,0L)

        if floor.Remove(coordinate) then
            ()
         else
            floor.Add(coordinate) |> ignore
    floor

let swap (left : 'a byref) (right : 'a byref) =
      let temp = left
      left <- right
      right <- temp

let updateGrid (src:Floor) (dst:Floor) =
    dst.Clear()
    for tile in src do
        let tx,ty = tile
        let allNeighbors = (Seq.map (fun (dx,dy) -> tx+dx, ty+dy) NEIGHBORS)
        let blackNeighbors = Seq.filter src.Contains allNeighbors
        let whiteNeighbors = Seq.filter (src.Contains >> not) allNeighbors
        match blackNeighbors |> Seq.length with
        | 0 ->  ()
        | c when c > 2 -> ()
        | _ -> dst.Add(tile) |> ignore

        for whiteNeighbor in whiteNeighbors do
            let tx,ty = whiteNeighbor
            if Seq.filter src.Contains (Seq.map (fun (dx,dy) -> tx+dx, ty+dy) NEIGHBORS) |> Seq.length = 2 then
                dst.Add(whiteNeighbor) |> ignore
            else
                ()

let gameOfTiles numberRounds (initialFloor:Floor) =
    let mutable src = initialFloor
    let mutable dst = Floor()

    for _ in 1..numberRounds do
        updateGrid src dst
        swap &src &dst
        //printfn "%i" src.Count
    src

let parse = (createParser)
assert ((0L,0L) = ((parse "nwwswee\n").[0] |> Seq.fold step (0L,0L)))
let input = File.ReadAllText "input/24" |> parse 
let solution1 = input |> followInstructions |> (fun f -> f.Count)
let solution2 = input |> followInstructions |> gameOfTiles 100 |> (fun f -> f.Count)
