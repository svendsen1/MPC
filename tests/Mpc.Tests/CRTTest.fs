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
            Input       = 70I + bigint i
            si          = bigint 0
            ReceivedSt  = []
            ReceivedS2t = []
            Rt = []
            R2t = []

            WireShares = Map.empty
            InputShares = []
            m = 0I
            kingM = []
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

    let schemeParams = { P0 = p0; Moduli = moduli; L = L }

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
    let pl = {P0 = 101I; Moduli = [103I; 107I; 109I] ; L = 30I}
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
    let schemeParams = { P0 = p0; Moduli = moduli; L = 30I }
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
    let p0 = 11I
    let moduli = [13I; 17I; 19I]
    let parties = makeTestParties 3 p0 moduli
    let schemeParams = { P0 = p0; Moduli = moduli; L = 30I }
    let parties = CRTOffline.pickSi parties (p0 - 1I)

    let parties = CRTOffline.computeShares parties schemeParams

    let vandemonde = CRTOffline.makeVandermonde schemeParams.Moduli.Length 1
    let parties = CRTOffline.compputeMaskingPairs parties vandemonde
    // ------- OFFLINE ----------

    //parties |> List.iteri (fun i p -> printfn "player %d has %d pairs" i p.R2t.Length)
    //parties |> List.iter (fun p -> PrettyPrint.printMaskingPairs p)
    let parties = CRTOnline.shareInputWithPrints parties schemeParams
    parties |> List.iteri (fun i p -> printfn "player %d has %d pairs" i p.R2t.Length)

    let parties = CRTOnline.circuitEmulation ([ADD("part1", "input1", "input2"); ADD("part2", "input3", "part1")]) parties schemeParams
    parties |> List.iteri (fun i p -> printfn "player %d has %d pairs" i p.R2t.Length)
    //let parties = CRTOnline.circuitEmulation ([MUL("mulRes", "input1", "input2"); MUL("mulRes2", "mulRes", "input3")]) parties schemeParams
    printfn "Shares of each player to perfrom circuit evaluation on:"
    PrettyPrint.printWireShares parties
    printfn " \n "

[<Fact>] 
let ``Matrix Mul`` () = 
    let b = [1I;1I;1I;1I]

    let a1 = CRTOffline.makeVandermonde 4 1
    printfn "v: %A" a1

    let p = CRTOffline.matVecMulMod a1 b 11I
    printf "p:"
    List.iter (fun i -> printf " %A" i) p