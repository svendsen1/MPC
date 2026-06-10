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
    let shareInput (parties: list<Party>) (crtParams: CrtShareParams) =
        let d = computeD (List.length parties |> bigint)
        let allShares =
            parties |> List.map (fun owner ->
                "input" + string owner.Index, CRTShare.share (reFormat owner.Input crtParams.P0 d) crtParams)
        parties |> List.mapi (fun i p ->
            let newShares = List.fold (fun m (key, shares) -> Map.add key (List.item i shares) m) p.WireShares allShares
            { p with WireShares = newShares })

    let shareValue value parties crtParams out =
        let d = computeD (bigint (List.length parties))

        let xBar = reFormat value crtParams.P0 d
        let dispList = CRTShare.share xBar crtParams

        List.zip parties dispList
        |> List.map (fun (p, share) ->
            { p with WireShares = Map.add out share p.WireShares }
        )
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
    let mulMi (p: Party) (a: Wire) (b: Wire) = 
        let va = p.WireShares[a]
        let vb = p.WireShares[b]
        let masking = List.head p.R2t
        let mi = (((va*vb)% p.Modulus) + masking) % p.Modulus
        {p with m = mi; R2t = List.tail p.R2t}
    let kingShare (p: Party list) = 
        let M = List.map (fun party -> party.m) p
        match p with
        | h::rest -> {h with kingM = M}::rest
        | _ -> failwith "Fail kingShare"
    let kingReconstruct (shares: bigint list) (moduli: bigint list) =
        CRTReconstruct.crtReconstruct shares moduli
    let mulProtocol (p: list<Party>) (crtParams: CrtShareParams) (out: Wire) (a: Wire) (b: Wire) =
        // Step 1: Each party computes mi = (va * vb + R2t_head) mod pi
        // and consumes their R2t head
        let partiesWithMi = p |> List.map (fun party -> mulMi party a b)
        // Step 2: King collects all mi values and reconstructs m
        let partiesAfterKingShare = kingShare partiesWithMi
        let M = kingReconstruct partiesAfterKingShare.Head.kingM crtParams.Moduli
        // Step 3: King reformats m
        let d = computeD (List.length p |> bigint)
        let mBar = reFormat M crtParams.P0 d
        // Step 4: Each party sets output share as (mBar + Rt_head) mod pi
        // and consumes their Rt head
        partiesWithMi |> List.map (fun party ->
            let rtHead = List.head party.Rt
            let outputShare = (mBar + rtHead) % party.Modulus
            { party with 
                WireShares = Map.add out outputShare party.WireShares
                Rt = List.tail party.Rt }
        )
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
        let X = CRTReconstruct.crtReconstruct shares parms.Moduli       
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

        // Step 2 - Share public constants to wire map (e.g. inv(n))
        // Use CRT sharing so the constant reconstructs correctly modulo p0.
        let partiesWithConstants =
            shareValue (ExtendMath.modInverse (bigint (List.length parties)) crtParams.P0) partiesWithInputs crtParams "avg"

        // Step 3 - Evaluate circuit
        let partiesAfterCircuit = circuitEmulation circuit partiesWithConstants crtParams

        // Step 4 - Output reconstruction
        let result = outputReconstruction partiesAfterCircuit "out" crtParams

        // Print results
        //printfn "=== Online Phase Results ==="
        //printfn "Reconstructed output: %A" result
        //printfn "==========================="

        result