namespace Protocols

module CRTOnline =
    open Protocols
    open MPCcore

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
    


