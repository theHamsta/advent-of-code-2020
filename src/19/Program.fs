#if INTERACTIVE
#r "nuget: FParsec"
#endif

open System.IO
open FParsec
open System.Collections.Generic

type Rule = DisjunctionRule of int64 list list | SingleCharRule of char
type Input = {rules: Map<int64,Rule>
              words: string list}
    with static member New (rules,words) = {rules = rules
                                            words = words}

type MaybeParser = JustRule of Rule | JustParser of Parser<unit,unit>

let toParserOption p = match p with
                         | JustRule _ -> None
                         | JustParser p -> Some p
let isParser = toParserOption >> Option.isSome
let toParser = toParserOption >> Option.get

let createParser =
    let ws = many (pstring " ")
    let str text = pstring text .>> ws
    let pConjuction = sepEndBy1 pint64 ws
    let pDisjuction = sepBy1 pConjuction (str "|") .>> ws |>> DisjunctionRule
    let pQuotes = (pstring "\"")
    let pSingleChar =  between pQuotes pQuotes anyChar |>> SingleCharRule
    let pRule = ws >>. pint64 .>> (str ":") .>>. (pDisjuction <|> pSingleChar) .>> ws
    let pRules = sepEndBy1 pRule newline |>> Map
    let pWords = sepEndBy1 (many1Satisfy isLetter) spaces
    let pInput = spaces >>. pRules .>> spaces .>>. pWords .>> spaces .>> eof |>> Input.New

    (fun text ->
        match run pInput text with
        | Success(input, _, _) -> input
        | Failure(err, _, _) -> failwith err)

let (<!>) (p: Parser<_,_>) label : Parser<_,_> =
    p
    //fun stream ->
        //printfn "%A: Entering %s" stream.Position label
        //let reply = p stream
        //printfn "%A: Leaving %s (%A)" stream.Position label reply.Status
        //reply

let solution1 (input:Input) =
    let parsers = Dictionary<int64,Parser<unit,unit>>()
    let indexToParser i = parsers.[i]

    Map.map (fun k rule -> match rule with
                             | SingleCharRule c -> parsers.[k] <- ((skipChar c |> attempt) <!> sprintf "rule %i: %A" k c)
                             | DisjunctionRule rules -> 
                                 // Combine to a single parser with fire!
                                 parsers.[k] <- attempt (parse.Delay(fun () -> choice (Seq.map (fun x -> Seq.map (indexToParser) x |> Seq.reduce (>>?) |> attempt) rules) |> attempt)) <!> sprintf "rule %i" k
                             ) input.rules |> ignore

    let rule0Parser = parsers.[int64 0] .>> eof
    Seq.map (run rule0Parser) input.words
    |> Seq.sumBy (fun result -> match result with
                                | Success _ -> 1
                                | Failure _ -> 0)
    //|> Seq.zip input.words |> List.ofSeq

let data = File.ReadAllText "input/19"
let parser = (createParser)
let sol1 = solution1 (parser data)
let data2 = File.ReadAllText "input/19_example4"
let sol2 = solution1 (parser data2)
