namespace Protocols

module CRTShare= 
    open MPCcore

    // Verify parameters are valid before using
    let verifyParams (p: CrtShareParams) : bool =
        let pAll = ExtendMath.product p.Moduli
        let correctnessHolds = (p.L + 1I) * p.P0 < pAll
        
        // Check pairwise coprimality
        let allCoprime =
            let allMods = p.P0 :: p.Moduli
            List.forall (fun (a, b) ->
                let (g, _, _) = ExtendMath.extGcd a b
                g = 1I
            ) [ for i in 0..allMods.Length-1 do
                    for j in i+1..allMods.Length-1 do
                        yield (allMods.[i], allMods.[j]) ]
        
        correctnessHolds && allCoprime

    // Share a secret x in Z_p0
    // Returns list of n shares, one per party
    let share (x: bigint) (p: CrtShareParams) : bigint list =
        if x < 0I then
            let failM =  "Secret out of range - x:" + string(x) 
            failwith failM
        
        // Pick random u in {0, ..., L-1}
        let u = ExtendMath.randomBigint p.L
        
        // Lift x to integer X = x + u * p0
        let X = x + u * p.P0
        
        // Each party i gets X mod p_i
        List.map (fun pi -> X % pi) p.Moduli
    // Reconstruct secret from shares
    // Returns x in Z_p0
    let reconstruct (shares: bigint list) (p: CrtShareParams) : bigint =
        // Recover X via CRT
        let X = CRTReconstruct.crtReconstruct shares p.Moduli
        
        // Reduce mod p0 to get secret back
        X % p.P0