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

        // Print allSharesSt
        printfn "allSharesSt:"
        List.iteri (fun i shares ->
            printfn "  Party %d's shares: %A" (i + 1) shares
        ) allSharesSt
        printfn "allSharesS2t:"
        List.iteri (fun i shares ->
            printfn "  Party %d's shares: %A" (i + 1) shares
        ) allSharesS2t
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
            }
        )
        |> fun parties ->  pickSi  parties p0