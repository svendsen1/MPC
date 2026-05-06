namespace Protocols

module CRTOnline =
    open Protocols
    open MPCcore

    let bigint (x:int) = bigint(x)
    let factorial n : bigint = 
        [1I..n] |> List.fold (*) 1I 

    let computeD (n: bigint): bigint = 
        [1I..n-1I] |> List.map factorial 
                   |> List.fold (*) 1I

    // Using CRT-reconstruct to find an x that holds for both:
    // xBar = x (mod p0)
    // xBar = 0 (mod d)
    // CRT-Reconstruct returns an x for which it holds that all remainders 
    // are the same. 
    // Also this holds 
    // 0 <= xBar <= p0 * d
    let reFormat (x: bigint) (p0: bigint) (d: bigint): bigint =
        let l1 = [x;0I] 
        let l2 = [p0; d]
        let xBar = CRTReconstruct.crtReconstruct l1 l2 
        if 0I <= xBar && xBar <= (p0*d) then
            xBar
        else
            failwith "Unable to reFormat"
    let shareInput (parties: list<Party>) (crtParams: CrtShareParams)=
        let d = computeD ((List.length parties) |> bigint)
        parties |> List.fold (fun updatedParties owner -> 
            let xBar = reFormat owner.Input crtParams.P0 d
            let dispList = CRTShare.share xBar crtParams
            updatedParties |> List.map (fun p -> 
                let share = (List.item (p.Index - 1) dispList)
                {p with WireShares = Map.add("input" + string owner.Index) share p.WireShares})
            ) parties
    
    let shareInputWithPrints (parties: list<Party>) (crtParams: CrtShareParams) =
        let d = computeD ((List.length parties) |> bigint)
        parties |> List.fold (fun updatedParties owner -> 
            let xBar = reFormat owner.Input crtParams.P0 d
            printfn "Party: %d" owner.Index
            printfn "Input: %A" owner.Input
            printfn "Reformatted input: %A" xBar
            let dispList = CRTShare.share xBar crtParams
            printfn "Shares of reformatted input: %A" dispList
            updatedParties |> List.map (fun p -> 
                let share = (List.item (p.Index - 1) dispList)
                {p with WireShares = Map.add("input" + string owner.Index) share p.WireShares})
            ) parties
            
    let rec checkRandomness (parties: Party list) = 
        match parties with
        | [] -> true
        | p::rest -> if p.Rt.IsEmpty || p.R2t.IsEmpty then  
                        false
                     else
                        checkRandomness rest

    let mulMi (p: Party) (a: Wire) (b: Wire) = 
        let va = p.WireShares[a]
        let vb = p.WireShares[b]
        let masking = List.head p.R2t
        let mi = (((va*vb) % p.Modulus) + masking) % p.Modulus
        {p with m = mi; R2t = List.tail p.R2t}
    let kingShare (p: Party list) = 
        let M = List.fold (fun acc f -> [f.m]@acc) [] p
        match p with
        | h::rest -> {h with kingM = M}::rest
        | _ -> failwith "Fail kingShare"
    let kingReconstruct (shares: bigint list) (moduli: bigint list) =
        CRTReconstruct.crtReconstruct shares moduli

    let mulProtocol (p: list<Party>) (crtParams: CrtShareParams) (out: Wire) (a: Wire) (b: Wire) =
        // Each party sends va and vb to king (simulate by collecting)
        let sharesA = p |> List.map (fun party -> party.WireShares.[a])
        let sharesB = p |> List.map (fun party -> party.WireShares.[b])
        // King reconstructs a and b
        let aRec = CRTReconstruct.crtReconstruct sharesA crtParams.Moduli % crtParams.P0
        let bRec = CRTReconstruct.crtReconstruct sharesB crtParams.Moduli % crtParams.P0
        let product = aRec * bRec % crtParams.P0
        // King reFormats product
        let d = computeD (List.length p |> bigint)
        let productBar = reFormat product crtParams.P0 d
        // King shares productBar
        let productShares = CRTShare.share productBar crtParams
        // Send shares back to parties
        let pWithShares = List.map2 (fun party share -> {party with WireShares = Map.add out share party.WireShares}) p productShares
        pWithShares

    let circuitEmulation (circut: Circut) (p: list<Party>) (crtParams: CrtShareParams)=
        let rec eval (gates: Circut) (parties: list<Party>) =
            match gates with
            | [] -> parties
            | gate::rest -> let updatedParties =
                                match gate with
                                | Input _ -> parties
                                | ADD (out,a, b) ->
                                    parties |> List.map (fun n -> 
                                        let va = n.WireShares[a]
                                        let vb = n.WireShares[b]
                                        let result = (va + vb) % n.Modulus
                                        {n with WireShares = Map.add out result n.WireShares}
                                    )
                                | MUL (out, a, b) -> mulProtocol parties crtParams out a b
                            eval rest updatedParties
        eval circut p

    let broadcastOutputShares (parties: Party list) (outputWire: string) : Party list =
        // Collect the output share from each party
        let broadcastValues = 
            parties |> List.map (fun p -> p.WireShares.[outputWire])
        
        // Every party receives all broadcasts
        parties |> List.map (fun p ->
            { p with broadcastRecived = broadcastValues }
        )

    let reconstructOutput (party: Party) (parms: CrtShareParams) : bigint =
        let shares = party.broadcastRecived
        //printfn "Reconstructing from shares: %A" shares
        //printfn "Using moduli: %A" parms.Moduli
        
        let X = CRTReconstruct.crtReconstruct shares parms.Moduli
        //printfn "Reconstructed X = %A" X
        //printfn "X mod p0 = %A" (X % parms.P0)
        
        X % parms.P0

    // Full output reconstruction — broadcast + reconstruct
    // Returns the final secret output value
    let outputReconstruction (parties: Party list) 
                            (outputWire: string)
                            (parms: CrtShareParams) : bigint =

        let partiesAfterBroadcast = broadcastOutputShares parties outputWire

        let result = reconstructOutput partiesAfterBroadcast.Head parms

        let allAgree =
            partiesAfterBroadcast 
            |> List.forall (fun p -> reconstructOutput p parms = result)

        if not allAgree then
            failwith "Parties reconstructed different output values — protocol error"

        result
    
    let runOnlinePhase (parties: Party list) (crtParams: CrtShareParams) (circuit: Gate list) : bigint =
    
        // Step 1 - Input sharing
        let partiesWithInputs = shareInput parties crtParams
        
        // Step 2 - Add public constants to wire map (e.g. inv3)
        let partiesWithConstants =
            partiesWithInputs |> List.map (fun p ->
                { p with WireShares = 
                            p.WireShares 
                            |> Map.add "inv3" (ExtendMath.modInverse 3I crtParams.P0 % p.Modulus) })

        // Step 3 - Evaluate circuit
        let partiesAfterCircuit = circuitEmulation circuit partiesWithConstants crtParams

        // Step 4 - Output reconstruction
        let result = outputReconstruction partiesAfterCircuit "out" crtParams

        // Print results
        printfn "=== Online Phase Results ==="
        partiesAfterCircuit |> List.iter (fun p ->
            printfn "Party %d output share: %A" p.Index p.WireShares.["out"])
        printfn "Reconstructed output: %A" result
        printfn "==========================="

        result