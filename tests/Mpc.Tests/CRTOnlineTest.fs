module CRTTests

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
let ``Circuit emulation - multiplication gate`` () =
    let testMulGate (shares1: bigint list) (shares2: bigint list)
                    (moduli: bigint list) (p0: bigint) 
                    (crtParams: CrtShareParams)
                    (expected: bigint) =
        
        // Run offline phase to generate fresh mask pair
        let n = List.length moduli
        let t = 1
        
        
        // Run multiplication gate
        let result = mulProtocol partiesWithMasks crtParams "a" "b" "out"

        // Property 1 — all shares in range
        result |> List.iter (fun p ->
            Assert.True(
                p.WireShares.["out"] >= 0I && p.WireShares.["out"] < p.Modulus,
                $"Party {p.Index} out share out of range"))

        // Property 2 — reconstruction gives expected value
        let outShares    = result |> List.map (fun p -> p.WireShares.["out"])
        let reconstructed = CRTReconstruct.crtReconstruct outShares moduli % p0
        Assert.True(
            reconstructed = expected,
            $"Mul gate: reconstructed {reconstructed} but expected {expected}")
    
    let moduli    = [103I; 107I; 109I]
    let p0        = 101I
    let crtParams = { P0 = p0; Moduli = moduli; L = 1000I }

    // shares of 7  — underlying X=411: [102; 90; 84]
    // shares of 34 — public scalar:    [34;  34; 34]
    // 7 * 34 mod 101 = 238 mod 101 = 36
    testMulGate [102I; 90I; 84I] [34I; 34I; 34I] moduli p0 crtParams 36I

    // 0 * 34 = 0
    testMulGate [0I; 0I; 0I] [34I; 34I; 34I] moduli p0 crtParams 0I

    // shares of 3 — underlying X=306: [100; 92; 88]
    // shares of 5 — underlying X=106: [3; 106; 106]  
    // 3 * 5 mod 101 = 15
    testMulGate [100I; 92I; 88I] [3I; 106I; 106I] moduli p0 crtParams 15I

    // shares of 10 — underlying X=111: [8; 4; 2]
    // shares of 10 — same
    // 10 * 10 mod 101 = 100
    testMulGate [8I; 4I; 2I] [8I; 4I; 2I] moduli p0 crtParams 100I

    // shares of 50 — underlying X=151: [48; 44; 42]
    // shares of 2  — underlying X=103: [0; 103; 103]... 
    // wait 103 mod 103 = 0, 103 mod 107 = 103, 103 mod 109 = 103
    // 50 * 2 mod 101 = 100
    testMulGate [48I; 44I; 42I] [0I; 103I; 103I] moduli p0 crtParams 100I
