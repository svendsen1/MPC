namespace MPCcore

module PlayerModule = 
    let makePlayer (id: PlayerId) (knows: (string * int) list) : Player=
        let knowsMap = Map.ofList knows 
        {
            PlayerId = id
            Knows = knowsMap
        }