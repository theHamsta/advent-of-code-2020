#if INTERACTIVE
#r "nuget: ShellProgressBar"
#endif
open System.IO
open System
open ShellProgressBar

let data = File.ReadAllText "input/15"
let startNumbers = data.Split(",", StringSplitOptions.RemoveEmptyEntries) |> Array.map (int64 >> bigint)

let swappedEnumerate = Seq.mapi (fun i b -> (b, uint64 i))

let getSeq (startNumbers:bigint array) maxNumber = 
    let length = startNumbers.Length |> uint64
    let progressBar = new ProgressBar(int maxNumber, "Initial message")
    seq {
        let mutable pastNumbers =  startNumbers |> swappedEnumerate |> Seq.take (startNumbers.Length - 1)|> Map
        let mutable last = startNumbers |> Seq.last
        for i in 0UL..(maxNumber-1UL) -> 
            if i < length then
                startNumbers.[int i]
            else
                progressBar.Tick()
                let lastIndex = i - 1UL
                let current =  match pastNumbers.TryFind last with
                                | None -> 0I
                                | Some(index) -> bigint (lastIndex - index)
                pastNumbers <- pastNumbers.Add(last, lastIndex)
                last <- current
                current
                                    
    }


let solution1 = getSeq startNumbers 2020UL |> Seq.last
#if INTERACTIVE
#time
#endif
let solution2 = getSeq startNumbers 30000000UL |> Seq.last
