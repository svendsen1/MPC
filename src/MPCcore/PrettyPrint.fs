namespace MPCcore

module PrettyPrint = 
    let printInput (party: Party) =
        printfn "Party %d: " party.Index
        printfn "Input: %A" party.Input

    let printAllInput (parties: Party list) =
        List.iter printInput parties

    let printCrtShareParams (parameters: CrtShareParams) =
        printfn "Parameters of the protocol:"
        printfn "  P0     = %A" parameters.P0
        printfn "  Moduli = %A" parameters.Moduli
        printfn "  L      = %A" parameters.L

    let printSi (party: Party) =
        printf "Party %d" party.Index
        printfn " Si: %A" party.si

    let printAllSi (parties: Party list) =
        List.iter printSi parties

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
    
    let printWireShares (parties: Party list) =
        parties |> List.iter (fun p ->
            printfn "Player %d:" p.Index

            p.WireShares
            |> Map.iter (fun wire value ->
                printfn "  %s -> %A" wire value
            )

            printfn "" // empty line between players
        )