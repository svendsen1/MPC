namespace Protocols

module ExtendMath =

    open System.Numerics
    open System.Security.Cryptography

    // Extended Euclidean Algorithm
    // Returns (gcd, s, t) such that a*s + b*t = gcd
    let rec extGcd (a: bigint) (b: bigint) : bigint * bigint * bigint =
        if b = 0I then
            (a, 1I, 0I)
        else
            let (g, s, t) = extGcd b (a % b)
            (g, t, s - (a / b) * t)

    // Modular inverse of a mod m
    // Throws if inverse doesn't exist
    let modInverse (a: bigint) (m: bigint) : bigint =
        let (g, s, _) = extGcd (((a % m) + m) % m) m
        if g <> 1I then
            failwith "Modular inverse does not exist — a and m are not coprime"
        else
            ((s % m) + m) % m

    // Random bigint in range [0, max)
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
