#if INTERACTIVE
#r "nuget: FParsec"
#endif

open System.IO
open System
open System.Collections.Generic
open FParsec

type Tile = char [,]

type TileConfiguration =
    { tileId: int
      tileConfiguration: string list
      tile: Tile }

type TileID = int
type Input = Map<TileID, Tile>
type Solution = Map<int * int, TileConfiguration>

let tee thing =
    printfn "%A" thing
    thing

let createParser =
    let str x = pstring x .>> many (pstring " ")

    let pLine =
        many1 (anyOf [ '#'; '.'; 'O' ]) .>> newline

    let pTile = many1 pLine

    let pTileName =
        (str "Tile") >>. pint32
        .>> (str ":")
        .>> skipNewline

    let pTileFragement =
        pTileName .>>. pTile
        |>> (fun (index, parsed) ->
            (index, Array2D.init (List.length parsed.[0]) (List.length parsed) (fun x y -> parsed.[x].[y])))

    let pInput =
        sepEndBy1 pTileFragement (many newline) |>> Map

    (fun text ->
        match run pInput text with
        | Success (ast, _, _) -> ast
        | Failure (err, _, _) -> failwith err)

let rotate90 tileWidth (x, y) = tileWidth - y - 1, x

let enumerate a = Seq.mapi (fun i e -> (i, e)) a

let extractBorders (tile: char [,]) tileId =
    let tileWidth = Array2D.length1 tile
    let rotate = rotate90 tileWidth

    seq {
        for flippX in [ id
                        (fun (x: int) -> tile.GetLength(0) - 1 - x) ] do
            for flippY in [ id
                            (fun (y: int) -> tile.GetLength(1) - 1 - y) ] do
                for outerRotate in [ id
                                     rotate
                                     rotate >> rotate
                                     rotate >> rotate >> rotate ] do
                    let transformed =
                        Array2D.init tileWidth tileWidth (fun x y ->
                            (flippX x, flippY y)
                            |> outerRotate
                            |> (fun (x, y) -> tile.[x, y]))

                    yield
                        (seq {
                            for i in 0 .. 3 ->
                                let charSeq =
                                    seq {
                                        for x in 0 .. (tile.GetLength(i % 2) - 1) ->
                                            match i with
                                            | 0 -> (x, 0)
                                            | 1 -> (tile.GetLength(0) - 1, x)
                                            | 2 -> (x, tile.GetLength(1) - 1)
                                            | 3 -> (0, x)
                                            | _ -> failwith "??"
                                            |> (fun (x, y) -> transformed.[x, y])
                                    }

                                String.Concat charSeq
                         })
                        |> List.ofSeq
                        |> (fun c ->
                            { tileId = tileId
                              tileConfiguration = c
                              tile = transformed })
    }
    |> Set.ofSeq

let TOP = 0
let RIGHT = 1
let BOTTOM = 2
let LEFT = 3

let createBorderLoopUp (borders: Map<TileID, Set<TileConfiguration>>) =
    let lookUps =
        [ Dictionary<string, TileConfiguration Set>()
          Dictionary<string, TileConfiguration Set>()
          Dictionary<string, TileConfiguration Set>()
          Dictionary<string, TileConfiguration Set>() ]

    for kvPair in borders do
        let configurations = kvPair.Value

        for c in configurations do
            for (direction, border) in c.tileConfiguration |> enumerate do
                assert (kvPair.Key = c.tileId)

                if lookUps.[direction].ContainsKey(border)
                then lookUps.[direction].[border] <- lookUps.[direction].[border].Add(c)
                else lookUps.[direction].[border] <- Set.empty.Add(c)

    lookUps

let inverseDirection dir =
    match dir with
    | d when d = TOP -> BOTTOM
    | d when d = RIGHT -> LEFT
    | d when d = BOTTOM -> TOP
    | d when d = LEFT -> RIGHT
    | _ -> failwith "noooo!"

let getNeighbor x y direction (currentSolution: Solution) (lookUp: Dictionary<string, TileConfiguration Set> list) =
    Map.tryFind (x, y) currentSolution
    |> Option.map (fun c ->
        let rtn =
            lookUp.[direction]
                .GetValueOrDefault(c.tileConfiguration.[inverseDirection direction], Set.empty)

        assert (Seq.forall (fun a -> a.tileConfiguration.[direction] = c.tileConfiguration.[inverseDirection direction])
                    rtn)

        rtn)

let rec extendSolution puzzleSizeX
                       puzzleSizeY
                       (currentSolution: Map<int * int, TileConfiguration>)
                       (availableIds: Input)
                       (lookUp: Dictionary<string, TileConfiguration Set> list)
                       =
    let emptyField =
        Seq.tryFind (fun (x, y) -> currentSolution.ContainsKey((x, y)) |> not)
            (seq {
                for y in 0 .. puzzleSizeY - 1 do
                    for x in 0 .. puzzleSizeX - 1 do
                        yield (x, y)
             })

    match emptyField with
    | Some (x, y) ->
        let leftRequirements =
            getNeighbor (x - 1) y LEFT currentSolution lookUp

        let topRequirements =
            getNeighbor x (y - 1) TOP currentSolution lookUp

        let possibilities =
            match (leftRequirements, topRequirements) with
            | Some a, Some b -> Set.intersect a b
            | None, Some b -> b
            | Some a, None -> a
            | _ -> lookUp.[0].Values |> Seq.concat |> Set.ofSeq

        Seq.tryPick (fun p ->
            if availableIds.ContainsKey(p.tileId) then

                extendSolution
                    puzzleSizeX
                    puzzleSizeY
                    (currentSolution.Add((x, y), p))
                    (availableIds.Remove(p.tileId))
                    lookUp
            else
                None) possibilities
    | _ -> Some currentSolution

let solvePuzzle (input: Input) =
    let borders =
        Map.map (fun k v -> extractBorders v k) input

    let tileLookUp = createBorderLoopUp borders

    let puzzleSize =
        round (sqrt (float borders.Count)) |> int

    extendSolution puzzleSize puzzleSize Map.empty input tileLookUp

let addCorners (solution: Solution) =
    let puzzleSize =
        round (sqrt (float solution.Count)) |> int

    int64 solution.[(0, 0)].tileId
    * int64 solution.[(0, puzzleSize - 1)].tileId
    * int64 solution.[(puzzleSize - 1, 0)].tileId
    * int64 solution.[(puzzleSize - 1, puzzleSize - 1)].tileId


let checkSolution (solution: Solution) =
    let puzzleSize =
        round (sqrt (float solution.Count)) |> int

    for x in 0 .. puzzleSize - 1 do
        for y in 1 .. puzzleSize - 1 do
            let top =
                solution.[(x, y - 1)].tileConfiguration.[BOTTOM]

            let bottom =
                solution.[(x, y)].tileConfiguration.[TOP]

            assert (top = bottom)

    for x in 1 .. puzzleSize - 1 do
        for y in 0 .. puzzleSize - 1 do
            let left =
                solution.[(x - 1, y)].tileConfiguration.[RIGHT]

            let right =
                solution.[(x, y)].tileConfiguration.[LEFT]

            assert (left = right)

let assemblePuzzle (solution: Solution) =
    let puzzleSize =
        round (sqrt (float solution.Count)) |> int

    let pieceSize = solution.[(0, 0)].tile.GetLength(0) - 2

    Array2D.init (puzzleSize * pieceSize) (puzzleSize * pieceSize) (fun x y ->
        solution.[(x / pieceSize, y / pieceSize)].tile.[1 + (x % pieceSize), 1 + (y % pieceSize)])

let countWaterRoughness (monsterTemplate:Tile) (totalArea:Tile) =
    let rotatedFlippedMonsters = extractBorders monsterTemplate 0

    let mutable beforeCount = 0
    Array2D.iter (fun x -> if x = '#' then beforeCount <- beforeCount + 1 else ()) totalArea

    // Iteration over slow coordinate goes brrrrrrrr
    for monsterConfig in rotatedFlippedMonsters do
        let monster = monsterConfig.tile
        for x in -(monster.GetLength(0)-1)/2..totalArea.GetLength(0)-1 + (monster.GetLength(0)-1)/2 do
            for y in -(monster.GetLength(1)-1)/2..totalArea.GetLength(1)-1+(monster.GetLength(1)-1)/2 do
                let mutable monsterMatch = true
                for u in -(monster.GetLength(0)-1)/2..(monster.GetLength(0)-1)/2 do
                    for v in -(monster.GetLength(1)-1)/2..(monster.GetLength(1)-1)/2 do
                        let monsterPixel = monster.[u + (monster.GetLength(0)-1)/2, v + (monster.GetLength(1)-1)/2]
                        let seaPixel = if x+u >= 0 && x+u <totalArea.GetLength(0) && y+v >= 0 && y+v < totalArea.GetLength(1) then
                                        totalArea.[x + u, y + v]
                                       else 
                                            '.'

                        match monsterPixel, seaPixel with
                        | '#', '.' -> monsterMatch <- false
                        | _ -> ()
                
                if monsterMatch then
                    for u in -(monster.GetLength(0)-1)/2..(monster.GetLength(0)-1)/2 do
                        for v in -(monster.GetLength(1)-1)/2..(monster.GetLength(1)-1)/2 do
                            let monsterPixel = monster.[u + (monster.GetLength(0)-1)/2, v + (monster.GetLength(1)-1)/2]

                            if x+u >= 0 && x+u < totalArea.GetLength(0) && y+v >= 0 && y+v < totalArea.GetLength(1) then
                                match monsterPixel with
                                | '#' -> totalArea.[x + u, y + v] <- 'O'
                                | _ -> ()
                            else
                                ()
                else ()
    let mutable count = 0
    Array2D.iter (fun x -> if x = '#' then count <- count + 1 else ()) totalArea
    count, beforeCount

let compareArrays (a:Tile) (b:Tile) =
    Array2D.iteri (fun x y _ -> assert (a.[x,y] = b.[x,y])) a

let parse = (createParser)
let data = File.ReadAllText "input/20"
let parsed = parse data

let solvedPuzzle = solvePuzzle parsed
let solution1 = solvedPuzzle |> Option.map addCorners
solvedPuzzle |> Option.map checkSolution |> ignore

let monsterExample =
    (File.ReadAllText "input/seamonster" |> parse).[0]
let monsterTemplate =
    (File.ReadAllText "input/monster-template" |> parse).[0]
let exampleRoughness =countWaterRoughness monsterTemplate monsterExample

let assembledPuzzle = solvedPuzzle |> Option.map assemblePuzzle
let solution2 = assembledPuzzle |> Option.map (countWaterRoughness monsterTemplate)

