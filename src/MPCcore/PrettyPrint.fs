namespace MPCcore

module PrettyPrint = 
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