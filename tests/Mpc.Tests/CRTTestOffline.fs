module CRTTestOffline

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
let private assertReceivedSharesDistributed (party: Party) expectedLen =
    Assert.True(party.ReceivedSt.Length = expectedLen, sprintf "Party %d: expected %d ReceivedSt values, got %d" party.Index expectedLen party.ReceivedSt.Length)
    Assert.True(party.ReceivedS2t.Length = expectedLen, sprintf "Party %d: expected %d ReceivedS2t values, got %d" party.Index expectedLen party.ReceivedS2t.Length)

    party.ReceivedSt |> List.iter (fun value -> Assert.True(value >= 0I && value <= party.Modulus - 1I, sprintf "Party %d: ReceivedSt value %A out of range for modulus %A" party.Index value party.Modulus))
    party.ReceivedS2t |> List.iter (fun value -> Assert.True(value >= 0I && value <= party.Modulus - 1I, sprintf "Party %d: ReceivedS2t value %A out of range for modulus %A" party.Index value party.Modulus))

let private assertMaskingPairsValid (party: Party) (t: int) =
    let expectedLen = party.ReceivedSt.Length - t
    Assert.True(party.Rt.Length = expectedLen, sprintf "Party %d: expected %d Rt values, got %d" party.Index expectedLen party.Rt.Length)
    Assert.True(party.R2t.Length = expectedLen, sprintf "Party %d: expected %d R2t values, got %d" party.Index expectedLen party.R2t.Length)

    party.Rt |> List.iter (fun value -> Assert.True(value >= 0I && value <= party.Modulus - 1I, sprintf "Party %d: Rt value %A out of range for modulus %A" party.Index value party.Modulus))
    party.R2t |> List.iter (fun value -> Assert.True(value >= 0I && value <= party.Modulus - 1I, sprintf "Party %d: R2t value %A out of range for modulus %A" party.Index value party.Modulus))

let offlineConfigs =
    [
        { P0 = 11I; Moduli = [13I; 17I; 19I]; L = 30I }
        { P0 = 101I; Moduli = [103I; 107I; 109I; 113I; 127I; 131I; 137I; 139I; 149I]; L = 50000I }
        { P0 = 7I; Moduli = [11I; 13I; 17I; 19I]; L = 10I }
    ]

[<Fact>]
let ``Offline Phase parameter validation`` () =
    let p0 = 11I
    let moduli = [13I; 17I; 19I]
    let schemeParams = { P0 = p0; Moduli = moduli; L = 30I }

    Assert.True(CRTShare.verifyParams schemeParams, sprintf "Expected schemeParams to be valid: %A" schemeParams)

[<Fact>]
let ``Offline Phase Si sharing and reconstruction`` () =
    let p0 = 11I
    let moduli = [13I; 17I; 19I]
    let parties = makeTestParties 3 p0 moduli
    let schemeParams = { P0 = p0; Moduli = moduli; L = 30I }

    let parties = CRTOffline.pickSi parties (p0 - 1I)
    parties |> List.iter (fun party ->
        Assert.True(party.si >= 0I && party.si <= p0 - 2I, sprintf "Party %d: si must be in [0, p0-2], got %A" party.Index party.si)
    )

    let allSharesSt = parties |> List.map (fun p -> CRTShare.share p.si schemeParams)
    let allSharesS2t = parties |> List.map (fun p -> CRTShare.share ((schemeParams.P0 - p.si) % schemeParams.P0) schemeParams)

    allSharesSt |> List.iteri (fun index shares ->
        Assert.True(parties.[index].si % schemeParams.P0 = CRTShare.reconstruct shares schemeParams, sprintf "Share row %d did not reconstruct to si for party %d" index parties.[index].Index)
    )

    allSharesS2t |> List.iteri (fun index shares ->
        Assert.True(((schemeParams.P0 - parties.[index].si) % schemeParams.P0) = CRTShare.reconstruct shares schemeParams, sprintf "Share row %d did not reconstruct to p0 - si for party %d" index parties.[index].Index)
    )

    let parties = CRTOffline.computeShares parties schemeParams
    parties |> List.iter (fun party -> assertReceivedSharesDistributed party schemeParams.Moduli.Length)

[<Fact>]
let ``Offline Phase masking pairs are computed correctly`` () =
    let p0 = 11I
    let moduli = [13I; 17I; 19I]
    let parties = makeTestParties 3 p0 moduli
    let schemeParams = { P0 = p0; Moduli = moduli; L = 30I }

    let parties = CRTOffline.pickSi parties (p0 - 1I)
    let parties = CRTOffline.computeShares parties schemeParams
    let vandemonde = CRTOffline.makeVandermonde schemeParams.Moduli.Length 1

    Assert.True(vandemonde.Length = 2, sprintf "Expected 2 rows in Vandermonde matrix, got %d" vandemonde.Length)
    Assert.All(vandemonde, fun row -> Assert.True(row.Length = moduli.Length, sprintf "Expected Vandermonde row length %d, got %d" moduli.Length row.Length))

    let parties = CRTOffline.compputeMaskingPairs parties vandemonde
    parties |> List.iter (fun party -> assertMaskingPairsValid party 1)

[<Fact>]
let ``Offline Phase works for multiple valid params`` () =
    offlineConfigs
    |> List.iter (fun schemeParams ->
        Assert.True(CRTShare.verifyParams schemeParams, sprintf "Expected valid offline config: %A" schemeParams)

        let parties = makeTestParties schemeParams.Moduli.Length schemeParams.P0 schemeParams.Moduli
        let parties = CRTOffline.pickSi parties (schemeParams.P0 - 1I)
        let parties = CRTOffline.computeShares parties schemeParams
        parties |> List.iter (fun party -> assertReceivedSharesDistributed party schemeParams.Moduli.Length)

        let vandemonde = CRTOffline.makeVandermonde schemeParams.Moduli.Length 1
        let parties = CRTOffline.compputeMaskingPairs parties vandemonde
        parties |> List.iter (fun party -> assertMaskingPairsValid party 1)
    )
[<Fact>]
let ``Offline Phase `` () =
    printfn "Test of the entire offline Phase"
    let p0 = 11I
    let moduli = [13I; 17I; 19I]
    let parties = makeTestParties 3 p0 moduli
    let schemeParams = { P0 = p0; Moduli = moduli; L = 30I }

    PrettyPrint.printCrtShareParams schemeParams

    let parties = CRTOffline.pickSi parties (p0 - 1I)
    printfn ""
    printfn "Each party randomly samples Si:"
    PrettyPrint.printAllSi parties
   
    
    printfn "\n"
    printfn "The shares of Si are computed (note that here shares from all parties are collected, the list of shares at index i is from party i)"
    let parties = CRTOffline.computeSharesWithPrints parties schemeParams

    printfn ""
    printfn "The shares are distributed such that party at index i gets share i from each party"
    PrettyPrint.printAllReceivedShares parties

    let vandemonde = CRTOffline.makeVandermonde schemeParams.Moduli.Length 1

    let parties = CRTOffline.compputeMaskingPairs parties vandemonde
    printfn ""
    printfn "Each party generates masking pairs Rt and R2t by multiplication of the vandemonde matrix under modulo P_i:"
    PrettyPrint.printAllRs parties
    printfn ""

[<Fact>]

let ``Offline phase all in one`` () =
    let p0 = 11I
    let moduli = [13I; 17I; 19I]
    let parties = makeTestParties 3 p0 moduli
    let schemeParams = { P0 = p0; Moduli = moduli; L = 30I }

    let partiesAfterOffline = CRTOffline.runOfflinePhase parties schemeParams
    //PrettyPrint.printAllRs partiesAfterOffline
    partiesAfterOffline |> List.iter (fun party -> assertReceivedSharesDistributed party schemeParams.Moduli.Length)
    partiesAfterOffline |> List.iter (fun party -> assertMaskingPairsValid party 1)
