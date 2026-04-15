module CRTTests

open System
open Xunit

open MPCcore
open Protocols

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
    let p0 = 11I
    let moduli = [13I; 17I; 19I]
    let parties = CRTOffline.makeTestParties 3 p0 moduli
    let schemeParams = { P0 = p0; Moduli = moduli; L = 30I }
    let parties = CRTOffline.pickSi parties (p0 - 1I)

    let parties = CRTOffline.computeShares parties schemeParams

    CRTOffline.printAllReceivedShares parties

