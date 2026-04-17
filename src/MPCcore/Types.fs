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

type MaskPair = {
    RtShares : bigint list 
    Rt2Shares : bigint list
}

type Party = {
    Index : int  //1-indexed
    Modulus : bigint
    Input : bigint 
    si : bigint
    
    //Offline phase
    ReceivedSt : bigint list
    ReceivedS2t : bigint list

    Rt : bigint list
    R2t : bigint list

    //Online phase
    InputShares : bigint list
    WireShares : Map<string, bigint>
    m : bigint
    kingM : bigint list
}

type Vmatrix = bigint list list

type Wire = string
type Gate = 
    | Input of Wire
    | ADD of Wire * Wire * Wire
    | MUL of Wire * Wire * Wire
type Circut = Gate list
