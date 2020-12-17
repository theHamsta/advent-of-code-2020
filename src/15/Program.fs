#if INTERACTIVE
#r "nuget: ShellProgressBar"
#endif
open System.IO
open System
open ShellProgressBar

let data = File.ReadAllText "input/15"
let startNumbers = data.Split(",", StringSplitOptions.RemoveEmptyEntries) |> Array.map (uint64)

let swappedEnumerate = Seq.mapi (fun i b -> (b, uint64 i))

let getSeq (startNumbers:uint64 array) maxNumber doProgressFn = 
    let length = startNumbers.Length |> uint64
    seq {
        let mutable pastNumbers =  startNumbers |> swappedEnumerate |> Seq.take (startNumbers.Length - 1)|> Map
        let mutable last = startNumbers |> Seq.last
        for i in 0UL..(maxNumber-1UL) -> 
            if i < length then
                startNumbers.[int i]
            else
                doProgressFn()
                let lastIndex = i - 1UL
                let current =  match pastNumbers.TryFind last with
                                | None -> 0UL
                                | Some(index) -> (lastIndex - index)
                pastNumbers <- pastNumbers.Add(last, lastIndex)
                last <- current
                current
                                    
    }


let solution1 = getSeq startNumbers 2020UL id |> Seq.last

let calcSolution2 value =
    use progressBar = new ProgressBar(int value, "Initial message")
    getSeq startNumbers value (fun () -> progressBar.Tick()) |> Seq.last

let solution2 = calcSolution2 30000000UL
