open System.IO
open System
open System.Text.RegularExpressions

type Rule = { color: string; contents: Map<string, int> }

let parseRule (line: string) =
    let splits = line.Split(" contain ", StringSplitOptions.RemoveEmptyEntries)
    try 
        let colorPart = splits.[0]
        let contentPart = splits.[1]

        let color = Regex.Match(colorPart, @"(\w+\s+\w+)\sbags").Groups.[1].Value
        let contents = Regex.Matches(contentPart, @"(\d+)\s+(\w+\s+\w+)\sbag[s]?")
                       |> Seq.map (fun m -> ((m.Groups.[2].Value), (int (m.Groups.[1].Value))))
                       |> Map<string, int>

        Some({ color = color;
               contents = contents })

    with _ -> None

let rec depthSearchKey (start:Rule) (rules:Map<string, Rule>) =
    let children = Set.unionMany(seq { for i in start.contents -> depthSearchKey (rules.[i.Key]) rules })
    let self = start.color
    children.Add(self)

let rec depthSearchCount (start:Rule) (rules:Map<string, Rule>) =
    let childrenResult = Seq.sum(seq { for i in start.contents -> (depthSearchCount (rules.[i.Key]) rules) * i.Value })
    1 + childrenResult

let getRuleMap (text: string) = 
    text.Split("\n", StringSplitOptions.RemoveEmptyEntries)
    |> Array.map parseRule
    |> Seq.filter Option.isSome
    |> Seq.map (Option.get >>  (fun r -> (r.color, r)))
    |> Map

let solution1 (text: string) (startColor: string) =
    let rules = getRuleMap text
    rules
    |> Seq.map (fun r -> (depthSearchKey rules.[r.Key] rules).Remove(r.Key))
    |> Seq.filter (fun x -> x.Contains(startColor))
    |> Seq.length

let solution2 (text: string) (startColor:string) =
    let rules = getRuleMap text
    (depthSearchCount rules.[startColor] rules) - 1

let data = File.ReadAllText "input/07"

let sol1 = solution1 data "shiny gold"
let sol2 = solution2 data "shiny gold"
