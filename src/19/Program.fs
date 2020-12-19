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

let many1TillResult12 p endp =
    many1 (notFollowedBy endp >>. p) .>>. endp 

let resultSatisfies predicate msg (p: Parser<_,_>) : Parser<_,_> =
    let error = messageError msg
    fun stream ->
      let state = stream.State
      let reply = p stream
      if reply.Status <> Ok || predicate reply.Result then reply
      else
          stream.BacktrackTo(state) // backtrack to beginning
          Reply(Error, error)

let solution1 (input:Input) hackRules =
    let parsers = Dictionary<int64,Parser<unit,unit>>()
    let indexToParser i = parsers.[i]

    Map.map (fun k rule -> match rule with
                             | SingleCharRule c -> parsers.[k] <- skipChar c |> attempt
                             | DisjunctionRule rules -> 
                                 // Combine to a single parser with fire!
                                 parsers.[k] <- attempt (parse.Delay(fun () -> choice (Seq.map (fun x -> Seq.map (indexToParser) x |> Seq.reduce (>>?) |> attempt) rules) |> attempt))
                             ) input.rules |> ignore

    let mutable rule0Parser = parsers.[int64 0] .>> eof
    if hackRules then
        parsers.Remove(int64 8) |> ignore
        parsers.Remove(int64 11) |> ignore
        let b = (many1Till (indexToParser 31L >>% 1) eof)
        let a = (many1TillResult12 (indexToParser 42L >>% 1) b) |> resultSatisfies (fun (a,b) -> Seq.sum a = Seq.sum b) "!"
        rule0Parser <- (skipMany1Till (indexToParser 42L) a)
    else
        ()

    Seq.map (run rule0Parser) input.words
    |> Seq.sumBy (fun result -> match result with
                                | Success (result,_,_) -> printfn "%A" result;1
                                | Failure _ -> 0)
    //|> Seq.zip input.words |> List.ofSeq

let data = File.ReadAllText "input/19"
let parser = (createParser)
let sol1 = solution1 (parser data) false
let data2 = File.ReadAllText "input/19"
let sol2 = solution1 (parser data2) true
