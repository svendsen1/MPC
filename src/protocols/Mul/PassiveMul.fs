namespace Protocols

module PassiveMul = 
    open MPCcore

    let pasMulPlayer (player: Player) (u: list<int*int>) = 
        let rec pasMulInner (u: list<int*int>) (acc: int) =
            match u with
            |   (sV,tV)::tail ->    let s = Map.find ("s" + (sV |> string)) player.Knows
                                    let t = Map.find ("t" + (tV |> string)) player.Knows
                                    pasMulInner tail (s*t + acc)
            |   []            ->    {player with ResV = Some acc}
        pasMulInner u 0
    
    let passiveMul (players: list<Player>) (Us: list<list<int*int>>) = 
        let rec passiveInner (players: list<Player>) (Us: list<list<int*int>>) (acc: list<Player>) =
            match players, Us with
            | p::pTail, uSet::restU -> passiveInner pTail restU ((pasMulPlayer p uSet)::acc)
            | [],[]              -> List.rev acc
            | _                  -> failwith "List length mismatch - PassiveMul"
        passiveInner players Us []