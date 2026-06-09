namespace Protocols

module ExtendMath =
    open System.Numerics
    open System.Security.Cryptography

    // Extended Euclidean Algorithm
    // Returns (gcd, s, t) such that a*s + b*t = gcd
    let extGcd (a: bigint) (b: bigint) : bigint * bigint * bigint =
        let rec loop (r0: bigint) (r1: bigint) (s0: bigint) (s1: bigint) (t0: bigint) (t1: bigint) =
            if r1 = 0I then
                (r0, s0, t0)
            else
                let q = r0 / r1
                loop r1 (r0 - q * r1) s1 (s0 - q * s1) t1 (t0 - q * t1)
        loop a b 1I 0I 0I 1I
    // Modular inverse of a mod m
    // Throws if inverse doesn't exist
    let modInverse (a: bigint) (m: bigint) : bigint =
        let (g, s, _) = extGcd (((a % m) + m) % m) m
        if g <> 1I then
            failwith "Modular inverse does not exist — a and m are not coprime"
        else
            ((s % m) + m) % m

    // Random bigint in range [0, max]
    let randomBigint (max: bigint) : bigint =
        let rng = RandomNumberGenerator.Create()
        let bytes = Array.zeroCreate (int (bigint.Log(max, 256.0)) + 2)
        let mutable result = max
        while result >= max do
            rng.GetBytes(bytes)
            result <- bigint(bytes) |> abs
        result
    
    /// Compute product of all elements in a list
    let product (xs: bigint list) : bigint =
        List.fold (*) 1I xs
    
    /// Compute the power of 2 ints. Exponential opp is only allowed on floats, so convertion needed
    let pwr (x: int) (y: int) = 
        let result = float(x) ** float(y)
        bigint(result)

    let millerRabin (n: bigint) rounds =
        // write n-1 as 2^r * d
        let mutable r, d = 0, n - 1I
        while d % 2I = 0I do
            r <- r + 1
            d <- d / 2I
        let witnesses = Seq.take rounds (seq { 2I; 3I; 5I; 7I; 11I; 13I; 17I; 19I; 23I }) 
        witnesses |> Seq.forall (fun a ->
            let mutable x = bigint.ModPow(a, d, n)
            if x = 1I || x = n - 1I then true
            else
                let mutable cont = true
                for _ in 1..r-1 do
                    if cont then
                        x <- bigint.ModPow(x, 2I, n)
                        if x = n - 1I then cont <- false
                not cont)