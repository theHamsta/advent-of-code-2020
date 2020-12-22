#if INTERACTIVE
#r "nuget: FParsec"
#endif

open System.IO
open System.Collections.Generic
open FParsec

let tee thing =
    printfn "%A" thing
    thing

module Dict =
  let toSeq d = d |> Seq.map (fun (KeyValue(k,v)) -> (k,v))
  let toArray (d:IDictionary<_,_>) = d |> toSeq |> Seq.toArray
  let toList (d:IDictionary<_,_>) = d |> toSeq |> Seq.toList
  let ofMap (m:Map<'k,'v>) = new Dictionary<'k,'v>(m) :> IDictionary<'k,'v>
  let ofList (l:('k * 'v) list) = new Dictionary<'k,'v>(l |> Map.ofList) :> IDictionary<'k,'v>
  let ofSeq (s:('k * 'v) seq) = new Dictionary<'k,'v>(s |> Map.ofSeq) :> IDictionary<'k,'v>
  let ofArray (a:('k * 'v) []) = new Dictionary<'k,'v>(a |> Map.ofArray) :> IDictionary<'k,'v>
  let values d = d |> toSeq |> Seq.map snd
  let keys d = d |> toSeq |> Seq.map fst

type Dish =
    { ingredients: string Set
      allergens: string Set }
    static member FromTuple(i, a) =
        { ingredients = Set.ofSeq i
          allergens = Set.ofSeq a }

let createParser =
    let str x = pstring x .>> many (pstring " ")
    let ws = many (pstring " ")
    let pWord = many1Satisfy isLetter

    let pIngredients = sepEndBy1 pWord ws

    let pAllergens =
        (str "(")
        >>. (str "contains")
        >>. sepEndBy1 (pWord .>> ws) (str ",")
        .>> (str ")")

    let pLine =
        pIngredients .>>. pAllergens .>> newline
        |>> Dish.FromTuple

    let pInput = many1 pLine |>> Set.ofSeq

    (fun text ->
        match run pInput text with
        | Success (ast, _, _) -> ast
        | Failure (err, _, _) -> failwith err)


let maybeContainedByTable (dishes: Dish seq) =
    let allergens = dishes |> Seq.map (fun  d -> d.allergens) |> Set.unionMany   
    Seq.map (fun allergen -> allergen, Seq.filter (fun d -> d.allergens.Contains(allergen)) dishes
                                       |> Seq.map (fun d -> d.ingredients)
                                       |> Set.intersectMany) allergens
    |> Map

let safeIngredients (dishes: Dish seq) =
    let allIngredients = dishes |> Seq.map (fun d -> d.ingredients) |> Set.unionMany   
    let suspipiousIngredients = (maybeContainedByTable dishes)
                                |> Dict.values
                                |> Set.unionMany
    Set.difference allIngredients suspipiousIngredients

let countOccurences (dishes: Dish seq) (ingredients: string Set) =
    Seq.sumBy (fun d -> Set.intersect ingredients d.ingredients
                        |> Seq.length) dishes

let solve2 (dishes:Dish seq) =
    let mutable maybeInIngredient = maybeContainedByTable dishes
    let mutable safeAssignments = Map.empty

    // This is exactly the same as with the grammar assignments
    while maybeInIngredient.Count > 0 do
        let (safeAssigned, unknown) = Map.partition (fun _ v -> Seq.length v = 1) maybeInIngredient
        let assignedIngredients = Set.unionMany (Dict.values safeAssigned)
        maybeInIngredient <- Map.map (fun _ v -> Set.difference v assignedIngredients) unknown
        for (k,v) in Dict.toSeq safeAssigned do
            safeAssignments <- safeAssignments.Add(k, Seq.head v)
    safeAssignments

let parse = createParser
let input = File.ReadAllText "input/21" |> parse
let solution1 = safeIngredients input |> countOccurences input
let solution2 = solve2 input |> Dict.toList |> List.sortBy fst |> List.map snd |> String.concat ","
