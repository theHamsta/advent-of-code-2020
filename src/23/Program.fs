// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System

let input =
    "193467258"
    |> List.ofSeq
    |> List.map (int64 >> (fun x -> x - int64 '0'))

let example =
    "389125467"
    |> List.ofSeq
    |> List.map (int64 >> (fun x -> x - int64 '0'))

let enumerate s = Seq.mapi (fun i b -> (int i, b)) s

type CyclicList = { next: int64[]
                    mutable current: int64}
with
    static member FromSeq numbers =
        let lastNumber = Seq.last numbers
        let firstNumber = Seq.head numbers

        let nextList: int64[] = Array.zeroCreate (int lastNumber + 1)

        for previous, current in Seq.zip (Seq.skip 0 numbers) (Seq.skip 1 numbers) do
            nextList.[int previous] <- current

        // Make cyclic
        nextList.[int lastNumber] <- firstNumber

        { next = nextList
          current = firstNumber}

    member l.AdvanceCurrent =
            l.current <- l.next.[int l.current]

    member l.Drain3 =
        let next = l.next.[int l.current]
        let one = next
        let next = l.next.[int next]
        let two = next
        let next = l.next.[int next]
        let three = next

        l.next.[int l.current] <- next
        [one; two; three]

    member l.InsertAfter index first last =
        let previousNext = l.next.[int index]
        l.next.[int index] <- first
        l.next.[int last] <- previousNext

let playGame numRounds startState =
    let mutable state = startState

    for _ in 1L .. numRounds do
        let current = List.head state
        let pickUp = state |> List.skip 1 |> List.take 3
        let rest = state |> List.skip 4

        let dest =
            (seq { for i in 1L .. 4L -> (current - i - 1L + 9L) % 9L + 1L }
             |> List.ofSeq)
            |> Seq.find (fun x ->
                x <> pickUp.[0]
                && x <> pickUp.[1]
                && x <> pickUp.[2])

        let pos = Seq.findIndex (fun x -> x = dest) rest

        //printfn "Round %i" i
        //printfn "State %A" state
        //printfn "pickUp %A" pickUp
        //printfn "dest %A" dest
        //printfn ""
        state <-
            List.concat [ (List.take pos rest)
                          [ dest ]
                          pickUp
                          (List.skip (pos + 1) rest)
                          [ current ] ]

    state
    |> List.append state
    |> List.skipWhile (fun x -> x <> 1L)
    |> List.skip 1
    |> List.take 8


let playExtremeGame numRounds lastNumber (startState:int64 list) =
    let numbers = seq { for i in 1L..lastNumber -> if int i <= List.length startState then
                                                    startState.[int i - 1]
                                                   else
                                                    i }

    let myList = CyclicList.FromSeq numbers

    for _ in 1L .. numRounds do
        let current = myList.current
        let pickUp = myList.Drain3

        let dest =
            (seq { for i in 1L .. 4L -> (current - i - 1L + lastNumber) % lastNumber + 1L }
             |> List.ofSeq)
            |> Seq.find (fun x ->
                x <> pickUp.[0]
                && x <> pickUp.[1]
                && x <> pickUp.[2])

        myList.InsertAfter dest pickUp.[0] pickUp.[2]
        myList.AdvanceCurrent

    let next = myList.next.[int 1L]
    let nextNext = myList.next.[int next]
    next * nextNext

let solution1Example =
    example
    |> playGame 100L
    |> Seq.map string
    |> String.concat ""

let solution1 =
    input
    |> playGame 100L
    |> Seq.map string
    |> String.concat ""

let lastNumber = 1000000L
let numberRounds = 10000000L
example |> playExtremeGame numberRounds lastNumber
