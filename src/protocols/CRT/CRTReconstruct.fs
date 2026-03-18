namespace Protocols

module CRTReconstruct =
    open MPCcore
    open Protocols

    // Standard CRT reconstruction
    // Given shares s_i and moduli p_i, find unique X < P_all
    // such that X ≡ s_i (mod p_i) for all i
    let crtReconstruct (shares: bigint list) (moduli: bigint list) : bigint =
        let pAll = ExtendMath.product moduli

        let terms =
            List.map2 (fun s p ->
                let mi = pAll / p           // P_all / p_i
                let yi = ExtendMath.modInverse mi p    // mi^-1 mod p_i
                s * mi * yi                 // s_i * M_i * y_i
            ) shares moduli

        let sum = List.fold (+) 0I terms
        ((sum % pAll) + pAll) % pAll        // reduce mod P_all, ensure positive