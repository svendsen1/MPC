namespace MPCcore

type PlayerId = int

type Player = { 
            PlayerId: PlayerId; 
            Knows: Map<string,int>
                }