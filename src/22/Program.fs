#if INTERACTIVE
#r "nuget: FParsec"
#endif

open System.IO
open System.Collections.Generic
open FParsec

let tee thing =
    printfn "%A" thing
    thing

type Deck = int64 list

module Dict =
    let toSeq d =
        d |> Seq.map (fun (KeyValue (k, v)) -> (k, v))

    let toArray (d: IDictionary<_, _>) = d |> toSeq |> Seq.toArray
    let toList (d: IDictionary<_, _>) = d |> toSeq |> Seq.toList

    let ofMap (m: Map<'k, 'v>) =
        new Dictionary<'k, 'v>(m) :> IDictionary<'k, 'v>

    let ofList (l: ('k * 'v) list) =
        new Dictionary<'k, 'v>(l |> Map.ofList) :> IDictionary<'k, 'v>

    let ofSeq (s: ('k * 'v) seq) =
        new Dictionary<'k, 'v>(s |> Map.ofSeq) :> IDictionary<'k, 'v>

    let ofArray (a: ('k * 'v) []) =
        new Dictionary<'k, 'v>(a |> Map.ofArray) :> IDictionary<'k, 'v>

    let values d = d |> toSeq |> Seq.map snd
    let keys d = d |> toSeq |> Seq.map fst

let enumerate a = Seq.mapi (fun i e -> (i, e)) a

let createParser =
    let str x = pstring x .>> many (pstring " ")
    let ws = many (pstring " ")

    let pPlayer = (str "Player") >>. pint64 .>> ws .>> (str ":") .>> newline
    let pDeck = sepEndBy1 pint64 newline

    let pInput = sepEndBy1 (pPlayer .>>. pDeck) (many newline) .>> eof |>> Map

    (fun text ->
        match run pInput text with
        | Success (ast, _, _) -> ast
        | Failure (err, _, _) -> failwith err)

let playGame (input:Map<int64,Deck>) =
    let mutable p1 = input.[1L]
    let mutable p2 = input.[2L]

    while (p1.IsEmpty || p2.IsEmpty) |> not do
        let p1Card = Seq.head p1
        let p2Card = Seq.head p2
        if p1Card > p2Card then
            p1 <- List.concat [(List.tail p1);[p1Card;p2Card]]
            p2 <- List.tail p2
        else
            p2 <- List.concat [(List.tail p2);[p2Card;p1Card]]
            p1 <- List.tail p1
    if p2.IsEmpty then
        (1L, p1)
    else
        (1L, p2)

let calcScore (deck:Deck) =
    let numCards = List.length deck
    deck |> Seq.mapi (fun i e -> int64 (numCards - i) * e) |>Seq.sum

let parse = createParser
let input = File.ReadAllText "input/22" |> parse

let solution1 = playGame input |> snd |> calcScore
