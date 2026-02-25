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

    let pairs n =
        [1..n]
        |> List.collect (fun i ->
            [1..n] |> List.map (fun j -> (i, j))
        )
    
    let getPairs (p: Player) = 
        let keys =
            p.Knows
            |> Map.keys
            |> Seq.toList

        let sIndices =
            keys
            |> List.choose (fun k ->
                if k.StartsWith("s") then
                    Some (int (k.Substring(1)))
                else None)

        let tIndices =
            keys
            |> List.choose (fun k ->
                if k.StartsWith("t") then
                    Some (int (k.Substring(1)))
                else None)

        [ for i in sIndices do
            for j in tIndices do
                yield (i, j) ]
    
    let intersection list1 list2 =
        Set.intersect (Set.ofList list1) (Set.ofList list2)
        |> Set.toList
        
    let makeU (playerCount: int) (players: list<Player>) = 
        let unTakenPairs = pairs playerCount
        let rec iteratePlayers (players: list<Player>) (unTakenPairs: list<int*int>) (acc: list<list<int*int>>) =
            match players with
            |   p::tail ->  let playerPairs = getPairs p
                            let U = intersection unTakenPairs playerPairs
                            iteratePlayers tail (List.except (List.toSeq U) unTakenPairs) (acc @ [U])
            | []        ->  acc
        iteratePlayers players unTakenPairs []
