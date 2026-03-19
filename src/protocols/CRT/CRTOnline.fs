namespace Protocols

module CRTOnline =
    open Protocols
    open MPCcore

    let factorial n : bigint = 
        [1I..n] |> List.fold (*) 1I 

    
    let computeD (n: bigint): bigint = 
        [1I..n-1I] |> List.map factorial 
                   |> List.fold (*) 1I




