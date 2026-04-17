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
            xBar % p0
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
        let mi = (((va*vb) % p.Modulus) + (p.R2t |> List.head)) % p.Modulus
        {p with m = mi}
    let kingShare (p: Party list) = 
        let M = List.fold (fun acc f -> [f.m]@acc) [] p
        match p with
        | h::rest -> {h with kingM = M}::rest
        | _ -> failwith "Fail kingShare"
    let kingReconstruct (shares: bigint list) (moduli: bigint list) =
        CRTReconstruct.crtReconstruct shares moduli

    let mulProtocol (p: list<Party>) (crtParams: CrtShareParams) (a: Wire) (b: Wire) (out: Wire) =
        // For simplicity we always choose Party 0 (index 1) as KING

        if checkRandomness p then
            // Part 1 - Each party computes m_i and sends to KING
            let pMi = p |> List.map (fun f -> mulMi f a b)
            let pKingShared = kingShare pMi

            // Part 2 - King reconstruct
            let pKingRecon = match pKingShared with
                                    | h::rest -> let M = kingReconstruct h.kingM crtParams.Moduli
                                                 {h with kingM = [M]}::rest
                                    | _ -> failwith "Failed to KingReconstruct"

            //Part 3 - KingBradcast
            let M = match pKingRecon with | h::_ -> List.head h.kingM | _ -> failwith "Couldn't find M at KING"
            let mModP0 = M % crtParams.P0
            let d = computeD ((List.length p) |> bigint)
            let mBar = reFormat mModP0 crtParams.P0 d
            let pKingBroadcast = List.map (fun p -> {p with m = mBar % p.Modulus}) pKingRecon

            //Part 4 - Each party computes output shares
            let pWithWireShares =  List.map (fun p -> {p with WireShares = Map.add out (p.m + (List.head p.Rt) % p.Modulus) p.WireShares}) pKingBroadcast
            // Remove the used masking pair
            pWithWireShares |> List.map (fun p -> {p with  Rt = List.tail p.Rt 
                                                                R2t = List.tail p.R2t})
        else 
            failwith "Ran out of randomness"

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
                                | MUL (out, a, b) -> mulProtocol parties crtParams a b out
                            eval rest updatedParties
        eval circut p