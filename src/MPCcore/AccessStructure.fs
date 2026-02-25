namespace MPCcore 

// The set of players who are able to obtain the secret 
module SecrecyStructure =     
    type SecrecyStructure = Set<Set<Player>>

    // No one player can reconstruct the secret
    let singletonSecretStructure (players: Player list) = 
        let rec putInSet (playerList: Player list) (acc: SecrecyStructure) =
            match playerList with
            | p :: tail -> putInSet tail (acc.Add (Set.singleton p))
            | [] -> acc
        putInSet players Set.empty

    // TODO: Make custom secret structures
