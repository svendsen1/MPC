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
    
    let shareValsT (players: Player list) (name: string) (playersToKnow: Player list) (value: int) (index: int) =
        let rec shareValRec (players) (playersToKnow: Player list) (index: int) =
            match playersToKnow with
            |   p::rest ->  let shareName = name + ( index |> string)
                            let resultPlayers = players |> List.map (fun x ->
                                                        if p.PlayerId = x.PlayerId then 
                                                            {p with Knows = Map.add shareName value x.Knows}
                                                        else 
                                                            x)
                            shareValRec resultPlayers rest index
            |   []  -> players
        shareValRec players playersToKnow index

    let distributeShares (distList: Set<Player> list) (sList: int list) (tList: int list) (players: Player list) =
        let rec distShareRec (distList: Set<Player> list) (sList: int list) (tList: int list) (players: Player list) (index: int)=
            match distList, sList, tList with
            | d :: dTail, s :: sTail, t ::tTail ->  let sSharedPlayers = shareValsT players "s" (Set.toList d) s index
                                                    let tSharedPlayers = shareValsT sSharedPlayers "t" (Set.toList d) t index
                                                    distShareRec dTail sTail tTail tSharedPlayers (index+1)
            | [], [], []                        ->  players
            | _                                 -> failwith "Mismatch in list-lengths"
        distShareRec distList sList tList players 1
