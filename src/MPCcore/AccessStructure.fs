namespace MPCcore 

// The set of players who are able to obtain the secret 
module SecrecyStructure =     
    type SecrecyStructure = Set<Set<Player>>

    // No one player can reconstruct the secret
    let singletonSecrectStructure (players: Player list) = 
        let secrectStructure: SecrecyStructure = Set.empty
        let rec putInSet players = 
            match players with
            | p::tail ->    secrectStructure.Add (Set.singleton p)
                            putInSet tail
            | [] ->         0
            | _ -> failwith "Not a list of players"
        secrectStructure

    // TODO: Make custom secret structures
