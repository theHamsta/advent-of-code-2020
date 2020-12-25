open System
open System.IO


let fastExp b e m = 
    let mutable result = 1L
    let mutable b = b
    let mutable e = e
    let mutable cont = true
    if (1L &&& e) <> 0L then
        result <- b
    while cont do
        if e = 0L then
            cont <- false
        else 
            e <- e >>> 1
            b <- (b * b) % m
            if (e &&& 1L) <> 0L then
                result <- (result * b) % m
            else
                ()
    result

type PublicKey = { value: int64 }
    with
    static member New x = {PublicKey.value = x}

type PrivateKey = { value: int64 }
    with
    member k.Encrypt (x:int64) =
        //let mutable rtn = 1L
        //for _ in 1L..k.value do
            //rtn <- (rtn * x) % PrivateKey.MagicNumber
        //assert (rtn = (fastExp x k.value PrivateKey.MagicNumber))
        fastExp x k.value PrivateKey.MagicNumber
    member k.ToPublicKey = {PublicKey.value = k.Encrypt 7L}
    member k.ToCypherSystem =
        { publicKey = k.ToPublicKey; privateKey = k }

    static member MagicNumber = 20201227L
    static member New x = {PrivateKey.value = x}

and CypherSystem = { publicKey: PublicKey; privateKey: PrivateKey }
    with
    member s.Encrypt x = s.privateKey.Encrypt x
    static member New x = (PrivateKey.New x).ToCypherSystem

let input = (File.ReadAllText "input/25").Split('\n', StringSplitOptions.RemoveEmptyEntries) |> Array.map int64

let solve1 input = 
    let (doorPublic, cardPublic) = match input with
                                    | [| doorPublic; cardPublic |] -> PublicKey.New doorPublic, PublicKey.New cardPublic
                                    | _ -> failwith "Invalid input"
    let bruteForceCyperSystems = seq { for i in 1L..Int64.MaxValue -> CypherSystem.New i }
    let doorSystem = Seq.find (fun s -> s.publicKey = doorPublic) bruteForceCyperSystems

    let encryptionKey = doorSystem.Encrypt cardPublic.value

    //let cardSystem = Seq.find (fun s -> s.publicKey = cardPublic) bruteForceCyperSystems
    //let encryptionKey2 = cardSystem.Encrypt doorPublic.value
    //assert (encryptionKey = encryptionKey2)
    encryptionKey

let solution1 = solve1 input
