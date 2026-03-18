namespace MPCcore

type PlayerId = int

type Player = { 
            PlayerId: PlayerId; 
            Knows: Map<string,int>;
            ResV: int option
            V_m: int list
                }

type CrtShareParams = {
    P0     : bigint        // secret field modulus
    Moduli : bigint list   // party moduli p_1 ... p_n
    L      : bigint        // randomness range
}