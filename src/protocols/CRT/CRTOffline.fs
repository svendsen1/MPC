namespace Protocols

module CRTOffline =
    open Protocols
    open MPCcore
    let pickSi (parties: Party list) (max : bigint) = 
        List.map (fun p -> {p with si = (ExtendMath.randomBigint max)}) parties

    let computeShares (parties: Party list) (parameters : CrtShareParams) =
        // Make a list of list with St values e.g - [[St 1][St 2][St 3]]
        let allSharesSt =  List.map (fun p -> CRTShare.share p.si parameters) parties
        let allSharesS2t = List.map (fun p -> CRTShare.share (parameters.P0 - p.si) parameters) parties

        (* PRINT_CODE
        // Print allSharesSt
        printfn "allSharesSt:"
        List.iteri (fun i shares ->
            printfn "  Party %d's shares: %A" (i + 1) shares
        ) allSharesSt
        printfn "allSharesS2t:"
        List.iteri (fun i shares ->
            printfn "  Party %d's shares: %A" (i + 1) shares
        ) allSharesS2t
        *)
        // Take a list from the list of list, and get the j'th value. [..., {j},...]. And but in the j'th player
        // recived value spot
        List.map (fun p ->
            let receivedSharesSt = List.map (fun shares -> List.item (p.Index - 1)  shares) allSharesSt
            let receivedSharesS2t = List.map (fun shares -> List.item (p.Index - 1) shares) allSharesS2t
            { p with ReceivedSt = receivedSharesSt 
                     ReceivedS2t = receivedSharesS2t}
        ) parties

    let printReceivedShares (party: Party) =
        printfn "Party %d:" party.Index
        printfn "  ReceivedSt  : %A" party.ReceivedSt
        printfn "  ReceivedS2t : %A" party.ReceivedS2t

    let printAllReceivedShares (parties: Party list) =
        List.iter printReceivedShares parties
    
    let printMaskingPairs (party: Party) =
        printfn "Party %d:" party.Index
        printfn "  Rt  : %A" party.Rt
        printfn "  R2t : %A" party.R2t

    let printAllRs (parties: Party list) =
        List.iter printMaskingPairs parties

    let makeTestParties (n: int) (p0: bigint) (moduli: bigint list) =
        List.init n (fun i ->
            {
                Index       = i + 1
                Modulus     = List.item i moduli
                Input       = bigint (i + 1)
                si          = bigint 0
                ReceivedSt  = []
                ReceivedS2t = []
                MaskPool    = []
                Rt = []
                R2t = []
            }
        )
        |> fun parties ->  pickSi  parties p0
    let makeVandermonde (n: int) (t: int) = 
        let h = n-t-1
        let list = List.init n (fun n -> n + 1)
        let iter = List.init h (fun x -> x)
        let rec inner (iter: int list) (acc: Vmatrix) = 
            match iter with
                | head::tail -> 
                    let list = List.map (fun i -> ExtendMath.pwr i head) list
                    inner tail (list::acc)
                | [] -> acc
        inner iter []

    /// Multiply matrix by column vector, in ring Z_p
    let matVecMulMod (mat: Vmatrix) (vec: bigint list) (p: bigint) : bigint list =
        mat |> List.map (fun row ->
            List.map2 (*) row vec          // multiply elementwise
            |> List.fold (+) 0I            // sum
            |> fun x -> ((x % p) + p) % p // reduce mod p
        )
    
    /// Compute the Rt and R2t values for each party.
    let compputeMaskingPairs (parties: list<Party>) (vande: Vmatrix) = 
        parties |> List.map (fun p -> 
            {p with Rt = matVecMulMod vande p.ReceivedSt p.Modulus
                    R2t = matVecMulMod vande p.ReceivedS2t p.Modulus})