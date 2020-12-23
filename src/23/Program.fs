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

type CyclicXorList = { mylist: int64[]
                       mutable previous: int64
                       mutable next: int64 }
with
    static member FromSeq numbers =
        let lastNumber = Seq.last numbers
        let mylist: int64[] = Array.zeroCreate (int lastNumber + 1)

        mylist.[0] <- Seq.head numbers
        mylist.[int (Seq.head numbers)] <- Seq.head (Seq.skip 1 numbers)

        for previous, current, next in Seq.zip3 (Seq.skip 0 numbers) (Seq.skip 1 numbers) (Seq.skip 2 numbers) do
            mylist.[int current] <- next ^^^ previous

        mylist.[int lastNumber] <- (int64 lastNumber - 1L) ^^^ Seq.head numbers
        { mylist = mylist
          previous = 0L
          next = Seq.head numbers}

    member l.Current = l.mylist.[0]
    member l.AdvanceCurrent =
            let current = l.mylist.[0]
            let next = l.previous ^^^ l.mylist.[int l.mylist.[0]]
            let nextnext = current ^^^ l.mylist.[int next]
            l.previous <- current
            l.mylist.[0] <- next
            l.next <- nextnext
    member l.Drain n =
        let mutable previous = 0L
        let mutable current = 0L
        let mutable next = 0L
        for _ in 1..n do
            next <- l.mylist.[int current] ^^^ previous
            previous <- current
            current <- next
        l.mylist.[0] <- l.mylist.[0] ^^^ l.next ^^^ next
                        

type CyclicList = { previous: int64[]
                    next: int64[]
                    mutable current: int64}
with
    static member FromSeq numbers =
        let lastNumber = Seq.last numbers
        let firstNumber = Seq.head numbers

        let previousList: int64[] = Array.zeroCreate (int lastNumber + 1)
        let nextList: int64[] = Array.zeroCreate (int lastNumber + 1)

        for previous, current in Seq.zip (Seq.skip 0 numbers) (Seq.skip 1 numbers) do
            previousList.[int current] <- previous
            nextList.[int previous] <- current

        // Make cyclic
        nextList.[int lastNumber] <- firstNumber
        previousList.[int firstNumber] <- lastNumber

        { next = nextList
          previous = previousList
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
        l.previous.[int next] <- l.current
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

    let mylist: int64[] = Array.zeroCreate (int lastNumber + 1)

    mylist.[0] <- startState.[0]
    mylist.[int startState.[0]] <- startState.[1]

    for previous, current, next in Seq.zip3 (Seq.skip 0 numbers) (Seq.skip 1 numbers) (Seq.skip 2 numbers) do
        mylist.[int current] <- next ^^^ previous

    mylist.[int lastNumber] <- lastNumber - 1L

    let mutable previous = 0L
    let mutable current = 0L
    for _ in 0..30 do
        let next = mylist.[int current] ^^^ previous
        printfn "%i" next
        previous <- current
        current <- next

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
    mylist





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
example |> playExtremeGame 1 lastNumber |> Seq.last
