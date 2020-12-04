open System.IO
open System
open System.Text.RegularExpressions

type Passport = { byr:int; iyr:int; eyr:int; hgt:string; hcl:string; ecl:string; pid:string; cid:option<string> }

let mapmap f = Seq.map (Seq.map f)

let (|Regex|_|) pattern input =
     let m = Regex.Match(input, pattern)
     if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
     else None

let parseRecord (text:string) = let record = text.Split [|' ';'\n'|] 
                                             |> Seq.ofArray
                                             |> Seq.map (fun x -> x.Split [|':'|])
                                             |> Seq.fold (fun (map:Map<string,string>) x -> match x with
                                                                                            | [|key; value|] -> assert not (map.ContainsKey(key))
                                                                                                                map.Add(key,value)
                                                                                            | _ -> map) (Map([]))
                                if 7 <= record.Count && record.Count <= 8 then
                                   try Some({
                                          byr = let y = int record.["byr"]
                                                if y < 1920 || y > 2002 then failwith "Divisor cannot be zero!" else y;
                                          iyr = let y = int record.["iyr"];
                                                if y < 2010 || y > 2020 then failwith "Divisor cannot be zero!" else y;
                                          eyr = let y = int record.["eyr"];
                                                if y < 2020 || y > 2030 then failwith "Divisor cannot be zero!" else y;
                                          hgt = let h = record.["hgt"]
                                                match h with
                                                      | Regex @"(\d+)in" [a] -> if (int a) < 59 || (int a) > 76 then failwith "Divisor cannot be zero!" else h
                                                      | Regex @"(\d+)cm" [a] -> if (int a) < 150 || (int a) > 193 then failwith "Divisor cannot be zero!" else h
                                                      | _ -> failwith "Divisor cannot be zero!";
                                          hcl = let h = record.["hcl"]
                                                match h with
                                                   | Regex @"^#[0-9a-f]{6}$" [] -> h
                                                   | _ -> failwith "Divisor cannot be zero!";
                                          ecl = let h = record.["ecl"]
                                                match h with
                                                      | Regex @"(amb|blu|brn|gry|grn|hzl|oth)" [_] -> h
                                                      | _ -> failwith "Divisor cannot be zero!";
                                          pid = let h = record.["pid"]
                                                match h with
                                                 | Regex @"^\d{9}$" [] -> h
                                                 | _ -> failwith "Divisor cannot be zero!";
                                          cid = try Some(record.["cid"]) with _ -> None;
                                    }) with _ -> None
                                 else None

let parse (text:string) = text.Split("\n\n", StringSplitOptions.None)
                          |> Seq.ofArray
                          |> Seq.map parseRecord

let solution text = parse text
                    |> Seq.filter Option.isSome
                    |> Seq.map Option.get
                    |> Seq.length
[<EntryPoint>]
let main argv =
   match argv with
      | [|file|] -> let data = File.ReadAllText file
                    printfn "%A" (Seq.length (parse data))
                    for f in data.Split("\n\n", StringSplitOptions.None) do
                       (printfn "%A" f)
                    printfn "Solution1 %i" (solution data)
                    0
      | _ -> printfn "Invalid number of arguments: %A" argv
             -1
