namespace MPCcore 

// The set of players who are able to obtain the secret 
module AccessStructure = 
    open MPCcore
    
    type AccessStructure = Set<Player>

    let construct playersNo = 
        Set.ofList playersNo