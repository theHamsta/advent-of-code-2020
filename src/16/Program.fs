open System.IO
open System
open System.Text.RegularExpressions

type Interval =
    { low: int64
      high: int64 }
    member i.IsInside number = number >= i.low && number <= i.high

type Constraint =
    { name: string
      intervals: Interval array }

type Input =
    { myTicket: int64 array
      otherTickets: int64 array array
      constraints: Constraint array }
    member i.AllConstraints =
        Seq.collect (fun c -> c.intervals) i.constraints

    member i.AllTickets = Seq.append i.otherTickets [ i.myTicket ]

let toIndices s =
    s |> Seq.mapi (fun i _ -> int64 i) |> List.ofSeq

let parseInterval (text: string) =
    let splits =
        text.Split("-", StringSplitOptions.RemoveEmptyEntries)

    { low = int64 splits.[0]
      high = int64 splits.[1] }

let commaSplit (x: string) =
    x.Split(",", StringSplitOptions.RemoveEmptyEntries)
    |> Array.map int64

let parseInput text =
    { constraints =
          Regex.Matches(text, @"^(.*)\: (.+)+$", RegexOptions.Multiline)
          |> Seq.map
              (fun m ->
                  m.Groups
                  |> Seq.skip 1
                  |> fun group ->
                      { name = (Seq.head group).Value
                        intervals =
                            group
                            |> Seq.skip 1
                            |> Seq.head
                            |> (fun x ->
                                x.Value.Split(" or ", StringSplitOptions.RemoveEmptyEntries)
                                |> Array.map parseInterval) })
          |> Array.ofSeq
      otherTickets =
          text.Split("\n\n", StringSplitOptions.RemoveEmptyEntries).[2]
              .Split("\n", StringSplitOptions.RemoveEmptyEntries).[1..]
          |> Array.map commaSplit
      myTicket =
          Regex
              .Match(text, @"your ticket:\s+(.*)$", RegexOptions.Multiline)
              .Groups
          |> Seq.skip 1
          |> Seq.map ((fun m -> m.Value) >> commaSplit)
          |> Seq.head }


let invalidTicketEntries (input: Input) =
    input.otherTickets
    |> Seq.map
        (fun ticket ->
            ticket
            |> Seq.filter
                (fun t ->
                    Seq.exists (fun (interval: Interval) -> interval.IsInside(t)) input.AllConstraints
                    |> not))

let validTickets (input: Input) =
    input.otherTickets
    |> Seq.filter
        (fun ticket ->
            ticket
            |> Seq.forall (fun t -> Seq.exists (fun (interval: Interval) -> interval.IsInside(t)) input.AllConstraints))

let reverseMap map: Map<'a,'b> = 
      Map.fold (fun m key value -> m.Add(value,key)) Map.empty map

let solution2 (input: Input) =
    let validTickets =
        validTickets input
        |> Seq.append [ input.myTicket ]

    let allAssignments =
        input.constraints
        |> Seq.map (fun c -> (c, toIndices input.myTicket))
        |> Map

    let possibleAssigments =
        Map.map
            (fun currentConstraint indices ->
                indices
                |> Seq.filter
                    (fun (index: int64) ->
                        Seq.forall
                            (fun (ticket: int64 array) ->
                                Seq.exists
                                    (fun (i: Interval) -> i.IsInside(ticket.[int index]))
                                    currentConstraint.intervals)
                            validTickets))
            allAssignments

    let mutable possibleAssigments = possibleAssigments
    let mutable safeAssignments = Map.empty
    while safeAssignments.Count <> possibleAssigments.Count do
        printfn "%i" safeAssignments.Count
        safeAssignments <- possibleAssigments |> Map.filter (fun _ (indices: int64 seq) -> Seq.length indices = 1) |> Map.map (fun _ v -> Seq.head v) |> reverseMap
        possibleAssigments <- possibleAssigments |> Map.map (fun _ v -> if Seq.length v = 1 then v else Seq.filter (safeAssignments.ContainsKey >> not) v)

    possibleAssigments



let text = File.ReadAllText "input/16"
let parsed = parseInput text

let sol1 =
    invalidTicketEntries parsed |> Seq.sumBy Seq.sum
let sol2 = solution2 parsed |> Map.filter (fun k _ -> k.name.StartsWith("departure")) |> Map.fold (fun acc _ v  -> acc * parsed.myTicket.[Seq.head v |> int]) 1L
