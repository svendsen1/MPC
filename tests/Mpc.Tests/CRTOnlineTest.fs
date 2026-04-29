module CRTOnlineTests

open System
open Xunit

open MPCcore
open Protocols

[<Fact>]
let ``ReFormat`` () =
    // All three properties for every x in Z_p0, with any parameters
    let testReFormat (p0: bigint) (d: bigint) =
        [0I..p0-1I] |> List.iter (fun x ->
            let result = CRTOnline.reFormat x p0 d
            Assert.True(result % d   = 0I,        $"Not divisible by D:   reFormat({x}) = {result}")
            Assert.True(result % p0  = x,         $"Wrong residue mod p0: reFormat({x}) = {result}")
            Assert.True(result >= 0I,             $"Negative output:      reFormat({x}) = {result}")
            Assert.True(result < p0 * d,          $"Out of range:         reFormat({x}) = {result}"))

    // Test for different p0 and diff d - n=3 and n=4
    testReFormat 101I 2I
    testReFormat 103I 2I
 
    testReFormat 101I 12I
    testReFormat 103I 12I

    testReFormat 41849I 12I

[<Fact>]
let ``Input sharing`` () =
    // Helper to verify input sharing properties
    let testInputSharing (inputs: bigint list) (p0: bigint) (moduli: bigint list) (l: bigint) =
        let crtParams = { P0 = p0; Moduli = moduli; L = l }
        let d = CRTOnline.computeD (List.length inputs |> bigint)
        
        let parties = inputs |> List.mapi (fun i input ->
            {   Index = i + 1
                Modulus = moduli.[i]
                Input = input
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
            })
        let partiesAfterSharing = CRTOnline.shareInput parties crtParams
        
        // Property 1 — every party has a share for every input wire
        partiesAfterSharing |> List.iter (fun p ->
            inputs |> List.iteri (fun i _ ->
                Assert.True(
                    p.WireShares.ContainsKey("input" + string (i + 1)),
                    $"Party {p.Index} missing input{i + 1}")))
        
        // Property 2 — all shares are in range [0, p_i]
        partiesAfterSharing |> List.iter (fun p ->
            p.WireShares |> Map.iter (fun wire share ->
                Assert.True(
                    share >= 0I && share < p.Modulus,
                    $"Party {p.Index} share on {wire} = {share} out of range [0, {p.Modulus}]")))
        
        // Property 3 — reconstruction gives back original input
        inputs |> List.iteri (fun i originalInput ->
            let shares = partiesAfterSharing |> List.map (fun p -> 
                p.WireShares.["input" + string (i + 1)])
            let reconstructed = CRTReconstruct.crtReconstruct shares moduli % p0
            Assert.True((reconstructed = originalInput),$"input{i+1}: reconstructed {reconstructed} but expected {originalInput}")
        )

    // Original parameters
    testInputSharing [5I; 5I; 5I]       101I [103I; 107I; 109I] 1000I
    testInputSharing [0I; 0I; 0I]       101I [103I; 107I; 109I] 1000I
    testInputSharing [1I; 1I; 1I]       101I [103I; 107I; 109I] 1000I
    testInputSharing [3I; 4I; 5I]       101I [103I; 107I; 109I] 1000I
    testInputSharing [70I; 71I; 72I]    101I [103I; 107I; 109I] 1000I
    testInputSharing [98I; 99I; 100I]   101I [103I; 107I; 109I] 1000I
    testInputSharing [10I; 11I; 12I]    101I [103I; 107I; 109I] 1000I
    // Different p0 and moduli
    testInputSharing [3I; 4I; 5I]       11I  [13I; 17I; 19I]   100I
    testInputSharing [100I; 200I; 300I] 401I [409I; 419I; 421I] 10000I
    testInputSharing [0I; 0I; 0I]       401I [409I; 419I; 421I] 10000I
    testInputSharing [398I; 399I; 400I] 401I [409I; 419I; 421I] 10000I
    // Different number of parties
    testInputSharing [10I; 20I; 30I; 40I]       101I [103I; 107I; 109I; 113I]             1000I
    testInputSharing [5I; 10I; 15I; 20I; 25I]   101I [103I; 107I; 109I; 113I; 127I]      10000I
    testInputSharing [100I; 200I; 300I; 400I]   401I [409I; 419I; 421I; 431I]            100000I
    // Different L values
    testInputSharing [50I; 60I; 70I]    101I [103I; 107I; 109I] 5000I
    testInputSharing [3I; 4I; 5I]       101I [103I; 107I; 109I] 10I
    // Large prime p0
    testInputSharing [100I; 200I; 300I] 1009I [1013I; 1019I; 1021I] 100000I

[<Fact>]
let ``Add gate`` () =
    let testAddGate (shares1: bigint list) (shares2: bigint list) 
                    (moduli: bigint list) (p0: bigint) (expected: bigint) =
        let crtParams = { P0 = p0; Moduli = moduli; L = 10I }
        let parties = moduli |> List.mapi (fun i input ->
                {   Index = i + 1
                    Modulus = moduli.[i]
                    Input = input
                    si          = bigint 0
                    ReceivedSt  = []
                    ReceivedS2t = []
                    Rt = []
                    R2t = []

                    WireShares = Map.ofList [("a", shares1.[i]); ("b", shares2.[i])]
                    InputShares = []
                    m = 0I
                    kingM = []
                    broadcastRecived = []
                })
        
        let result = CRTOnline.circuitEmulation [ADD("out","a","b")] parties crtParams
        
        // Property 1 — all shares in range
        result |> List.iter (fun p ->
            Assert.True(p.WireShares.["out"] >= 0I && 
                        p.WireShares.["out"] < p.Modulus,
                        $"Party {p.Index} out share out of range"))
        
        // Property 2 — reconstruction gives expected value
        let outShares = result |> List.map (fun p -> p.WireShares.["out"])
        let reconstructed = CRTReconstruct.crtReconstruct outShares moduli % p0
        Assert.True((reconstructed = expected), $"Add gate: reconstructed {reconstructed} but expected {expected}")

    let moduli = [103I; 107I; 109I]
    let p0     = 101I

    // 0 + 0 = 0
    testAddGate [0I; 0I; 0I] [0I; 0I; 0I] moduli p0 0I
    // 50 + 50 = 100
    testAddGate [50I; 50I; 50I] [50I; 50I; 50I] moduli p0 100I
    // 60 + 60 = 120 mod 101 = 19
    testAddGate [60I; 60I; 60I] [60I; 60I; 60I] moduli p0 19I
    // x + 0 = x
    testAddGate [13I; 13I; 13I] [0I; 0I; 0I] moduli p0 13I



[<Fact>]
let ``Circuit emulation - multiplication gate`` () =
    let testMulGate (shares1: bigint list) (shares2: bigint list)
                    (moduli: bigint list) (p0: bigint) 
                    (crtParams: CrtShareParams)
                    (expected: bigint) =
        let parties = moduli |> List.mapi (fun i input ->
            {   Index = i + 1
                Modulus = moduli.[i]
                Input = input
                si = bigint 0
                ReceivedSt  = []
                ReceivedS2t = []
                Rt = []
                R2t = []

                WireShares = Map.ofList [("a", shares1.[i]); ("b", shares2.[i])]
                InputShares = []
                m = 0I
                kingM = []
                broadcastRecived = []
            })

        let partiesAfterOffline = CRTOffline.runOfflinePhase parties crtParams

        let result = CRTOnline.circuitEmulation [MUL("out","a","b")] partiesAfterOffline crtParams

        // Property 1 — all shares in range
        result |> List.iter (fun p ->
            Assert.True(p.WireShares.["out"] >= 0I && 
                        p.WireShares.["out"] < p.Modulus,
                        $"Party {p.Index} out share out of range"))

        // Property 2 — reconstruction gives expected value
        let outShares = result |> List.map (fun p -> p.WireShares.["out"])
        let reconstructed = CRTReconstruct.crtReconstruct outShares moduli % p0
        Assert.True((reconstructed = expected),
                    $"Mul gate: reconstructed {reconstructed} but expected {expected}")
    let moduli  = [103I; 107I; 109I]
    let p0      = 101I
    let d       = CRTOnline.computeD 3I
    let crtParams = { P0 = p0; Moduli = moduli; L = 1000I }


    // 7 * 34 mod 101 = 238 mod 101 = 36  (34 = 3^-1 mod 101, so 7/3 = 36 mod 101... 
    // wait: 7 * 34 = 238, 238 mod 101 = 36)
    // shares of 7: underlying X_w = 411, shares = [102; 90; 84]
    // shares of 34: [34; 34; 34]
    testMulGate [102I; 90I; 84I] [34I; 34I; 34I] moduli p0 crtParams 36I

    // 0 * anything = 0
    testMulGate [0I; 0I; 0I] [34I; 34I; 34I] moduli p0 crtParams 0I