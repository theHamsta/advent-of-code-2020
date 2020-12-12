open System.IO
open System
open System.Text.RegularExpressions

type AbsoluteDirection = N | S | W | E
type RelativeDirection = L | R | F
type NavigationDirection = Absolute of AbsoluteDirection | Relative of RelativeDirection
type NavigationInstruction = { direction: NavigationDirection; count: int64 }

type ShipState = { position: int64*int64; direction: int64 }

let parseInstruction (line: string) =
    let regex = Regex.Match(line, @"(\w)(\d+)")

    Some({ direction = match regex.Groups.[1].Value with
                        | "N" -> Absolute(N)
                        | "S"-> Absolute(S)
                        | "W"-> Absolute(W)
                        | "E"-> Absolute(E)
                        | "L" -> Relative(L)
                        | "R"-> Relative(R)
                        | "F"-> Relative(F)
                        | c -> failwith (sprintf "invalid direction: %A" c) 
                        ;
           count = int64 regex.Groups.[2].Value })


let parseInput (text: string) = 
    text.Split("\n", StringSplitOptions.RemoveEmptyEntries)
    |> Array.map parseInstruction
    |> Seq.filter Option.isSome
    |> Seq.map Option.get

let normalizeDir dir = (dir + 360L) % 360L

let move (position:int64*int64) direction count =
    match direction with
    | 0L -> ((fst position) + count, snd position)
    | 90L -> ((fst position), (snd position)+ count)
    | 180L -> ((fst position) - count, snd position)
    | 270L -> ((fst position) , (snd position) - count)
    | d -> failwith (sprintf "unsupported direction %d" d)
    
let manhatten a b = (abs (fst a - fst b)) + (abs (snd a - snd b))

let followInstructions (initialState: ShipState) (instructions:NavigationInstruction seq) = 
    instructions
    |> Seq.fold (fun (state:ShipState) instruction -> match instruction.direction with
                                                      | Relative r -> match r with
                                                                        | L -> { state with direction = normalizeDir (state.direction + instruction.count) }
                                                                        | R -> { state with direction = normalizeDir (state.direction - instruction.count) }
                                                                        | F -> { state with position = move state.position state.direction instruction.count }
                                                      | Absolute a -> match a with
                                                                        | E -> { state with position = move state.position 0L instruction.count}
                                                                        | N -> { state with position = move state.position 90L instruction.count}
                                                                        | W -> { state with position = move state.position 180L instruction.count}
                                                                        | S -> { state with position = move state.position 270L instruction.count}) initialState

let rotatePos pos rotationDirection =
    let (x,y) = pos
    match rotationDirection with
                    | L -> -y, x
                    | R -> y, -x
                    | _ -> failwith "invalid rotation"

let followInstructions2 (initialState: ShipState) initialStateShip (instructions:NavigationInstruction seq) = 
    instructions
    |> Seq.fold (fun (state:ShipState,shipPosition:ShipState) instruction ->
        printfn "waypoint %A ship %A" state.position shipPosition.position
        match instruction.direction with
          | Relative r -> match r with
                            | L 
                            | R -> { state with position = rotatePos state.position r }, shipPosition
                            | F -> state, { shipPosition with position = (fst shipPosition.position + fst state.position * instruction.count,
                                                                          snd shipPosition.position + snd state.position * instruction.count) }
          | Absolute a -> match a with
                            | E -> { state with position = move state.position 0L instruction.count}, shipPosition
                            | N -> { state with position = move state.position 90L instruction.count}, shipPosition
                            | W -> { state with position = move state.position 180L instruction.count}, shipPosition
                            | S -> { state with position = move state.position 270L instruction.count}, shipPosition)
                (initialState, initialStateShip)
    |> snd


let initialState = { position = (0L,0L); direction = 0L}
let initialStateWaypoint = { position = (10L,1L); direction = 0L}
let data = File.ReadAllText "input/12"
let parsed = parseInput data
let pos1 = followInstructions initialState parsed
let solution1 = manhatten pos1.position initialState.position
let pos2 = followInstructions2 initialStateWaypoint initialState parsed
let solution2 = manhatten pos2.position initialState.position

