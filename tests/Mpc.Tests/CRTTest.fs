module CRTTests

open System
open Xunit

open MPCcore
open Protocols

let makeTestParties (n: int) (p0: bigint) (moduli: bigint list) =
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

            WireShares = Map.ofList["inv3", 34I % p0]
            InputShares = []
            m = 0I
            kingM = []
            broadcastRecived = []
        }
    )
    |> fun parties ->  CRTOffline.pickSi  parties p0

[<Fact>]
let ``Share and Reconstruct`` () =
    // n=3, t=1, secret x=7, p0=11
    let p0 = 11I
    let moduli = [13I; 17I; 19I]
    let L = 30I

    let pAll = ExtendMath.product moduli

    let schemeParams = { P0 = p0; Moduli = moduli; L = L; t = 1}

    Assert.True(CRTShare.verifyParams schemeParams)

    // Share the secret
    let secret = 7I
    let shares = CRTShare.share secret schemeParams
    printfn "Secret:  %A" secret

    // Reconstruct
    let X = CRTReconstruct.crtReconstruct shares moduli
    let recoveredSecret = X % p0
    printfn "Recovered: %A" recoveredSecret

    Assert.True((pAll = 4199I))
    Assert.True((L+1I) * p0 < pAll)
    Assert.True((recoveredSecret = secret))
    printfn "CRT Share and Recover"

[<Fact>]
let ``King Reconstruct From example`` () =
    let pl = {P0 = 101I; Moduli = [103I; 107I; 109I] ; L = 30I; t = 1}
    let m = CRTShare.reconstruct [59I; 19I; 11I] pl

    let mBar = CRTOnline.reFormat m pl.P0 2I
    printfn "m -> %A" m
    printfn "mbar -> %A" mBar

    let res = CRTShare.reconstruct [102I; 86I; 78I] pl
    printfn "res -> %A" res
    Assert.True((res = 9I))

[<Fact>]
let ``Offline shares`` () = 
    printfn "Test: Offline Phase"
    let p0 = 11I
    let moduli = [13I; 17I; 19I]
    let parties = makeTestParties 3 p0 moduli
    let schemeParams = { P0 = p0; Moduli = moduli; L = 30I; t = 1}
    let parties = CRTOffline.pickSi parties (p0 - 1I)
    let parties = CRTOffline.computeShares parties schemeParams
    let vandemonde = CRTOffline.makeVandermonde schemeParams.Moduli.Length 1
    let parties = CRTOffline.compputeMaskingPairs parties vandemonde
    printfn "Each party randomly samples Si:"
    PrettyPrint.printAllSi parties

    let parties = CRTOffline.computeSharesWithPrints parties schemeParams
    
    printfn "Each party generates masking pairs Rt and R2t:"
    PrettyPrint.printAllRs parties
    printfn ""

[<Fact>]
let ``Online phase`` () = 
    printfn "Online phase:"
    let p0 = 101I
    let moduli = [103I; 107I; 109I]
    let parties = makeTestParties 3 p0 moduli
    let schemeParams = { P0 = p0; Moduli = moduli; L = 30I; t = 1}
    let parties = CRTOffline.pickSi parties (p0 - 1I)

    let parties = CRTOffline.computeShares parties schemeParams

    let vandemonde = CRTOffline.makeVandermonde schemeParams.Moduli.Length 1
    let parties = CRTOffline.compputeMaskingPairs parties vandemonde
    // ------- OFFLINE ----------

    let parties = CRTOnline.shareInputWithPrints parties schemeParams
    //parties |> List.iteri (fun i p -> printfn "player %d has %d pairs" i p.R2t.Length)

    let parties = CRTOnline.circuitEmulation ([ADD("w1", "input1", "input2"); ADD("w2", "input3", "w1"); MUL("out", "w2","inv3")]) parties schemeParams
    //let parties = CRTOnline.circuitEmulation ([ADD("w1", "input1", "input2"); ADD("out", "input3", "w1")]) parties schemeParams
    //let parties = CRTOnline.circuitEmulation ([MUL("mulRes", "input1", "input2"); MUL("out", "mulRes", "input3")]) parties schemeParams
    //parties |> List.iteri (fun i p -> printfn "player %d has %d pairs" i p.R2t.Length)
    

    printfn "Shares of each player to perfrom circuit evaluation on:"
    PrettyPrint.printWireShares parties
    printfn " \n "
    let result = CRTOnline.outputReconstruction parties "out" schemeParams
    printfn "Result: %A" result
    Assert.True((result = 2I))

[<Fact>]
let ``Online phase 2`` () = 
    printfn "Online phase:"
    let p0 = 101I
    let moduli = [103I; 107I; 109I]
    let parties = makeTestParties 3 p0 moduli
    let schemeParams = { P0 = p0; Moduli = moduli; L = 30I; t = 1 }
    // ------- OFFLINE ----------
    let parties = CRTOffline.pickSi parties (p0 - 1I)

    let parties = CRTOffline.computeShares parties schemeParams

    let vandemonde = CRTOffline.makeVandermonde schemeParams.Moduli.Length 1
    let partiesAfterOffline = CRTOffline.compputeMaskingPairs parties vandemonde
    // ------- ONLINE ----------
    printfn " \n "
    let result = CRTOnline.runOnlinePhase partiesAfterOffline schemeParams ([ADD("w1", "input1", "input2"); ADD("w2", "input3", "w1"); MUL("out", "w2","inv3")])
    Assert.True((result = 2I))

[<Fact>] 
let ``Matrix Mul`` () = 
    let b = [1I;1I;1I;1I]

    let a1 = CRTOffline.makeVandermonde 4 1
    printfn "v: %A" a1

    let p = CRTOffline.matVecMulMod a1 b 11I
    printf "p:"
    List.iter (fun i -> printf " %A" i) p

[<Fact>]
let ``runOfflinePhases with different t and circuit sizes`` () =
    // Test that runOfflinePhases runs the correct number of rounds
    let p0 = 11I
    let moduli = [13I; 17I; 19I]
    let n = moduli.Length  // 3 parties

    // Test case 1: t=1, circuit with 2 gates -> rounds = ceil(2 / (3-1)) = 1
    let parties1 = makeTestParties 3 p0 moduli
    let params1 = { P0 = p0; Moduli = moduli; L = 30I; t = 1 }
    let circuit1 = [ADD("w1", "input1", "input2"); MUL("out", "w1", "input3")]
    let partiesAfter1 = CRTOffline.runOfflinePhases parties1 params1 circuit1
    let expectedMasking1 = 1 * (n - 1)  // 1 round * 2 masking per party
    partiesAfter1 |> List.iter (fun p ->
        Assert.True(p.Rt.Length = expectedMasking1, sprintf "Party %d: Expected %d Rt values, got %d" p.Index expectedMasking1 p.Rt.Length)
        Assert.True(p.R2t.Length = expectedMasking1, sprintf "Party %d: Expected %d R2t values, got %d" p.Index expectedMasking1 p.R2t.Length)
    )

    // Test case 2: t=1, circuit with 3 gates -> rounds = ceil(3 / 2) = 2
    let parties2 = makeTestParties 3 p0 moduli
    let params2 = { P0 = p0; Moduli = moduli; L = 30I; t = 1 }
    let circuit2 = [ADD("w1", "input1", "input2"); ADD("w2", "w1", "input3"); MUL("out", "w2", "input1")]
    let partiesAfter2 = CRTOffline.runOfflinePhases parties2 params2 circuit2
    let expectedMasking2 = 2 * (n - 1)  // 2 rounds * 2 masking per party
    partiesAfter2 |> List.iter (fun p ->
        Assert.True(p.Rt.Length = expectedMasking2, sprintf "Party %d: Expected %d Rt values, got %d" p.Index expectedMasking2 p.Rt.Length)
        Assert.True(p.R2t.Length = expectedMasking2, sprintf "Party %d: Expected %d R2t values, got %d" p.Index expectedMasking2 p.R2t.Length)
    )

    // Test case 3: t=2, circuit with 3 gates -> rounds = ceil(3 / (3-2)) = 3
    let parties3 = makeTestParties 3 p0 moduli
    let params3 = { P0 = p0; Moduli = moduli; L = 30I; t = 2 }
    let circuit3 = circuit2  // Same 3 gates
    let partiesAfter3 = CRTOffline.runOfflinePhases parties3 params3 circuit3
    let expectedMasking3 = 3 * (n - 2)  // 3 rounds * 1 masking per party (n-t=1)
    partiesAfter3 |> List.iter (fun p ->
        Assert.True(p.Rt.Length = expectedMasking3, sprintf "Party %d: Expected %d Rt values, got %d" p.Index expectedMasking3 p.Rt.Length)
        Assert.True(p.R2t.Length = expectedMasking3, sprintf "Party %d: Expected %d R2t values, got %d" p.Index expectedMasking3 p.R2t.Length)
    )

[<Fact>]
let ``Masking pairs are consumed sequentially`` () =
    // Test that masking pairs are popped during online phase
    let p0 = 101I
    let moduli = [103I; 107I; 109I]
    let parties = makeTestParties 3 p0 moduli
    let schemeParams = { P0 = p0; Moduli = moduli; L = 30I; t = 1 }
    let circuit = [MUL("out", "input1", "input2")]  // 1 MUL gate

    // Run offline: 1 round, 2 masking per party
    let partiesAfterOffline = CRTOffline.runOfflinePhases parties schemeParams circuit
    let initialMasking = partiesAfterOffline.Head.R2t.Length  // Should be 2
    Assert.True(initialMasking.Equals 2, sprintf "Expected 2 initial masking values, got %d" initialMasking)

    // Set up dummy wire shares for testing
    let party = { partiesAfterOffline.Head with WireShares = Map.ofList [("input1", 1I); ("input2", 2I)] }

    // Simulate consumption: Manually call mulMi once (as in the protocol)
    let updatedParty = CRTOnline.mulMi party "input1" "input2"
    let remainingMasking = updatedParty.R2t.Length  // Should be 1
    Assert.True(remainingMasking.Equals 1, sprintf "Expected 1 remaining masking value after consumption, got %d" remainingMasking)

    // Call again to consume the last one
    let updatedParty2 = CRTOnline.mulMi updatedParty "input1" "input2"
    let finalMasking = updatedParty2.R2t.Length  // Should be 0
    Assert.True(finalMasking.Equals 0, sprintf "Expected 0 remaining masking values after full consumption, got %d" finalMasking)