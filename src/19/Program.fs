#if INTERACTIVE
#r "nuget: FParsec"
#endif

open System.IO
open FParsec

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


let solution1 (input:Input) =
    let mutable parsers = Map.empty 
    let mutable maybeParsers = Map.map (fun _ r -> JustRule r) input.rules 
    let indexToMaybeParser i = maybeParsers.[i]
    while parsers.Count < input.rules.Count do
        maybeParsers <- Map.map (fun _ rule -> match rule with
                                                 | JustParser _ -> rule
                                                 | JustRule (SingleCharRule c) -> JustParser (skipChar c)
                                                 | JustRule (DisjunctionRule rules) -> 
                                                     // Functional programming madness!
                                                     let allReferencedRules = Seq.collect (Seq.map indexToMaybeParser) rules
                                                     if Seq.forall isParser allReferencedRules then
                                                         // Combine to a single parser
                                                         choice (Seq.map (fun x -> Seq.map (indexToMaybeParser >> toParser) x
                                                                                   |> Seq.reduce (>>?)) rules)
                                                         |> JustParser
                                                     else rule
                                                 )
                                            maybeParsers
        parsers <- Map.filter (fun _ v -> isParser v) maybeParsers
    let parsers = Map.map (fun _ v -> toParser v) parsers
    let rule0Parser = parsers.[int64 0] .>> eof
    Seq.map (run rule0Parser) input.words // |> List.ofSeq
    |> Seq.sumBy (fun result -> match result with
                                | Success _ -> 1
                                | Failure _ -> 0)

let data = File.ReadAllText "input/19"
let parsed = (createParser) data
let sol1 = solution1 parsed
