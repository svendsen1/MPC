namespace Protocols

module CRTOffline =
    open Protocols
    open MPCcore
    let pickSi (parties: Party list) (max : bigint) = 
        List.map (fun p -> {p with si = (ExtendMath.randomBigint max)}) parties

    let computeShares (parties: Party list) (parameters : CrtShareParams) =
        // Make a list of list with St values e.g - [[St 1][St 2][St 3]]
        let allSharesSt =  List.map (fun p -> CRTShare.share p.si parameters) parties
        let allSharesS2t = List.map (fun p -> CRTShare.share (parameters.P0 - p.si) parameters) parties

        // Take a list from the list of list, and get the j'th value. [..., {j},...]. And but in the j'th player
        // recived value spot
        List.map (fun p ->
            let receivedSharesSt = List.map (fun shares -> List.item (p.Index - 1)  shares) allSharesSt
            let receivedSharesS2t = List.map (fun shares -> List.item (p.Index - 1) shares) allSharesS2t
            { p with ReceivedSt = receivedSharesSt 
                     ReceivedS2t = receivedSharesS2t}
        ) parties

    let makeVandermonde (n: int) (t: int) =
        let cols = List.init n (fun i -> i + 1)        // [1; 2; 3]
        let rows = List.init (n - t) (fun exp -> exp)  // [0; 1] — n-t rows
        let rec inner (iter: int list) (acc: Vmatrix) =
            match iter with
            | head :: tail ->
                let row = List.map (fun i -> ExtendMath.pwr i head) cols
                inner tail (row :: acc)
            | [] -> List.rev acc                       // reverse to keep correct order
        inner rows []

    /// Multiply matrix by column vector, in ring Z_p
    let matVecMulMod (mat: Vmatrix) (vec: bigint list) (p: bigint) : bigint list =
        match mat with
        | [] -> []
        | row::_ ->
            if List.length row <> List.length vec then
                failwith "Dimension mismatch in matVecMulMod"
            mat
            |> List.map (fun row ->
                List.map2 (*) row vec
                |> List.fold (+) 0I
                |> fun x -> ((x % p) + p) % p
            )

    /// Compute the Rt and R2t values for each party.
    let compputeMaskingPairs (parties: list<Party>) (vande: Vmatrix) = 
        parties |> List.map (fun p -> 
            {p with Rt = matVecMulMod vande p.ReceivedSt p.Modulus
                    R2t = matVecMulMod vande p.ReceivedS2t p.Modulus})