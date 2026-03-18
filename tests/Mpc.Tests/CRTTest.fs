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