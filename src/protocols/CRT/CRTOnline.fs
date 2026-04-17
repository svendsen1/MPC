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
        
    
