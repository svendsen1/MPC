module App

open Open.Numeric.Primes
open MPCcore
open Protocols

let prettyPrintPlayerList (playersList: list<Player>) =       
        playersList |> List.iter (fun x -> 
        printf "Player_%d shares:" (x.PlayerId + 1);
        Map.iter (fun k v -> printf "(%A,%d) " k v) x.Knows;
        printf " Products: %A Sum of products:%d" x.V_m (List.sum x.V_m)
        printfn ""
    )
let prettyPrintUs (Us) = 
    Us 
    |> List.iteri (fun playerIdx pairs ->
        printf "Player_%d multiplication pairs: " (playerIdx + 1)
        if List.isEmpty pairs then
            printf "none"
        else
            pairs 
            |> List.map (fun (i, j) -> sprintf "(s%d,t%d)" i j)
            |> String.concat ", "
            |> printf "%s"
        printfn ""
    )
    printfn""

let getPrimes n =
    Prime.Numbers 
    |> Seq.skip 100000
    |> Seq.take n
    |> Seq.toList


let makeParties (n: int) (p0: bigint) (moduli: bigint list) =
    List.init n (fun i ->
        {
            Index       = i + 1
            Modulus     = List.item i moduli
            Input       = 1I + bigint i
            si          = bigint 0
            ReceivedSt  = []
            ReceivedS2t = []
            Rt = []
            R2t = []

            WireShares = Map.empty
            InputShares = []
            m = 0I
            kingM = []
            broadcastRecived = []
        }
    )
    |> fun parties ->  CRTOffline.pickSi  parties p0

let createSumCircuit n =
    if n < 2 then
        failwith "Need at least two inputs to create an ADD gate."
    
    // We start by adding input1 and input2
    let firstGate = ADD("w1", "input1", "input2")
    
    // Helper to chain the rest: (current index, previous output wire, accumulated gates)
    let rec build (i, prevWire, acc) =
        if i > n then 
            List.rev acc
        else
            let currentOut = sprintf "w%d" (i - 1)
            let currentIn = sprintf "input%d" i
            let nextGate = ADD(currentOut, prevWire, currentIn)
            build (i + 1, currentOut, nextGate :: acc)

    build (3, "w1", [firstGate])

let createAvgCircut n = 
    let lastWire = sprintf "w%d" (n - 1)
    List.append (createSumCircuit n) [MUL("out", lastWire,"avg")]

[<EntryPoint>]
let main argv =
    // NUMBER OF PLAYERS - Choose the primes used as mod i
    let n = 17
    let moduli = getPrimes n |> List.map (fun x -> bigint x)
    let p0 = bigint (Prime.Numbers |> Seq.skip 1249 |> Seq.take 1 |> Seq.toList |> List.item 0)
    let schemeParams = { P0 = p0; Moduli = moduli; L = 30I }
    let c = createAvgCircut n

    let parties = makeParties n p0 moduli 
    List.iter (fun p -> printf "%A " p.Input) parties
    // ------- OFFLINE ----------
    let partiesAfterOffline = CRTOffline.runOfflinePhase parties schemeParams
    // ------- ONLINE ----------
    let result = CRTOnline.runOnlinePhase partiesAfterOffline schemeParams c

    0