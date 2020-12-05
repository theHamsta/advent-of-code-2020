open System.IO

let readLines (filePath: string) =
    seq {
        use sr = new StreamReader(filePath)

        while not sr.EndOfStream do
            yield sr.ReadLine()
    }

type Seat = { row: int; column: int }

let parseNumberString (encodedNumber: string) (oneChar: char) (zeroChar: char) =
    try
        Seq.foldBack (fun character (number: int, value: int) ->
            let nextValue = value * 2

            match character with
            | c when (oneChar = c) -> number + value, nextValue
            | c when (zeroChar = c) -> number, nextValue
            | _ -> failwith "invalid character") encodedNumber (0, 1)

        |> (fst >> Some)
    with _ -> None

let parseSeat (line: string) =
    try
        let rowPart = line.Substring(0, 7)
        let columnPart = line.Substring(7, 3)

        Some
            ({ row = Option.get (parseNumberString rowPart 'B' 'F')
               column = Option.get (parseNumberString columnPart 'R' 'L') })
    with _ -> None

let calcSeatId (seat: Seat) = seat.row * 8 + seat.column

let parseLines (lines: string seq) =
    lines
    |> Seq.map parseSeat
    |> Seq.filter Option.isSome
    |> Seq.map (Option.get >> calcSeatId)

let printInput (lines: string seq) =
    printfn "Number tickets %i" (Seq.length lines)

    for l in lines do
        (parseSeat >> printfn "%A") l

let fillAirplane (seatIds: int seq) = List.sort (List.ofSeq seatIds)

let solution lines = parseLines lines |> Seq.max

let solution2 lines =
    let seats = parseLines lines
    let filledAirplane = fillAirplane seats

    Seq.find
        Option.isSome
        (seq {
            for i in 0 .. filledAirplane.Length - 2 ->
                if filledAirplane.[i + 1] - filledAirplane.[i] = 2
                then Some(filledAirplane.[i] + 1)
                else None
         })
    |> Option.get


[<EntryPoint>]
let main argv =
    match argv with
    | [| file |] ->
        let data = readLines file
        printInput data
        printfn "Solution1: %i" (solution data)
        printfn "Solution2: %i" (solution2 data)
        0
    | _ ->
        printfn "Invalid number of arguments: %A" argv
        -1
