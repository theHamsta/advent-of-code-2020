let toInput numberString =
    numberString
    |> List.ofSeq
    |> List.map (int64 >> (fun x -> x - int64 '0'))

type CyclicList =
    { next: int64 []
      mutable current: int64 }
    static member FromSeq numbers =
        let firstNumber = Seq.head numbers
        let lastNumber = Seq.last numbers

        let nextList: int64 [] = Array.zeroCreate (int lastNumber + 1)

        for previous, current in Seq.zip numbers (Seq.skip 1 numbers) do
            nextList.[int previous] <- current

        // Make cyclic
        nextList.[int lastNumber] <- firstNumber

        { next = nextList
          current = firstNumber }

    member l.AdvanceCurrent = l.current <- l.next.[int l.current]
    member l.Next number = l.next.[int number]

    member l.Drain3 =
        let one = l.Next(l.current)
        let two = l.Next(one)
        let three = l.Next(two)
        l.next.[int l.current] <- three
        ( one, two, three )

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


let playExtremeGame numRounds lastNumber (startState: int64 list) =
    let numbers =
        seq { for i in 1L .. lastNumber -> if int i <= List.length startState then startState.[int i - 1] else i }

    let myList = CyclicList.FromSeq numbers

    for _ in 1L .. numRounds do
        let current = myList.current
        let (a,b,c) = myList.Drain3

        let dest =
            (seq { for i in 1L .. 4L -> let result = (current - i - 1L)
                                        if result > 0L then result else result + lastNumber })
            |> Seq.find (fun x ->
                x <> a && x <> b && x <> c)

        myList.InsertAfter dest a c
        myList.AdvanceCurrent

    let next = myList.Next(1L)
    let nextNext = myList.Next(next)
    (next, nextNext, next * nextNext)

let solution1Example =
    toInput "389125467"
    |> playGame 100L
    |> Seq.map string
    |> String.concat ""

let solution1 =
    toInput "389125467"
    |> playGame 100L
    |> Seq.map string
    |> String.concat ""

let lastNumber = 1000000L
let numberRounds = 10000000L
//toInput "389125467" |> playExtremeGame numberRounds lastNumber
let solution2 =
    toInput "193467258"
    |> playExtremeGame numberRounds lastNumber
