namespace Protocols

module SecretShare = 
    open MPCcore
    
    let shareVm (players: Player list) = 
                                    //List.choose removes None auto
        let listV = players |> List.choose (fun p -> p.ResV)
        players |> List.map (fun p -> {p with V_m = listV})
        
    let tSetShare (players: Player list) (sStructure: SecrecyStructure.SecrecyStructure) =
        let secrecyList = Set.toList sStructure
        let playerSet = Set.ofList players
        let rec tHats (list: Set<Player> list) (acc: Set<Player> list) =
            match list with
            | p :: tail -> tHats tail (acc @ [Set.difference playerSet p])
            | [] -> acc
        tHats secrecyList []
