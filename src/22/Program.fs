#if INTERACTIVE
#r "nuget: FParsec"
#endif

open System.IO
open FParsec

type Deck = int32 list

let createParser =
    let str x = pstring x .>> many (pstring " ")
    let ws = many (pstring " ")

    let pPlayer = (str "Player") >>. pint32 .>> ws .>> (str ":") .>> newline
    let pDeck = sepEndBy1 pint32 newline

    let pInput = sepEndBy1 (pPlayer .>>. pDeck) (many newline) .>> eof |>> Map

    (fun text ->
        match run pInput text with
        | Success (ast, _, _) -> ast
        | Failure (err, _, _) -> failwith err)

let rec playGame (input:Map<int32,Deck>) recursive =
    let mutable p1 = input.[1]
    let mutable p2 = input.[2]
    let mutable states = Set.empty
    //let mutable counter = ref 1

    while (p1.IsEmpty || p2.IsEmpty || states.Contains((p1,p2))) |> not do
        states <- states.Add((p1,p2))
        let p1Card = Seq.head p1
        let p2Card = Seq.head p2

        //printfn "Round %i" !counter
        //incr counter
        //printfn "P1 cards %A" p1
        //printfn "P2 cards %A" p2
        //printfn "P1 card %A" p1Card
        //printfn "P2 card %A" p2Card

        let p1Win = match (p1Card >= Seq.length p1 || p2Card >= Seq.length p2 || not recursive), p1Card > p2Card with
                    | (true, true) -> true
                    | (true, false) -> false
                    | (false, _) -> playGame (Map([(1, Seq.take p1Card (List.tail p1) |> List.ofSeq)
                                                   (2, Seq.take p2Card (List.tail p2) |> List.ofSeq)])) true
                                    |> fst
                                    |> (=) 1
        //printfn "P1 wins? %A" p1Win
        if p1Win then 
            p1 <- List.concat [(List.tail p1);[p1Card;p2Card]]
            p2 <- List.tail p2
        else
            p2 <- List.concat [(List.tail p2);[p2Card;p1Card]]
            p1 <- List.tail p1

    if p1.IsEmpty then
        (2, p2)
    else
        (1, p1)

let calcScore (deck:Deck) =
    let numCards = List.length deck
    deck |> Seq.mapi (fun i e -> int64 (numCards - i) * int64 e) |>Seq.sum

let parse = createParser
let input = File.ReadAllText "input/22" |> parse

let solution1 = playGame input false |> snd |> calcScore
let solution2 = playGame input true |> snd |> calcScore
