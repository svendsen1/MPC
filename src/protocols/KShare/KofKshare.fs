namespace Protocols

module KofKshare =
    open MPCcore
    let rand = System.Random()

    // Spltts NO1 into k random shares
    let KShare no1 k = 
        let rec split no1 list acc = 
            match list with
            | _::t when t.Length = 0 -> List.rev ((no1)::acc)  
            | _::t ->   let rnd1 = rand.Next(0, no1)  
                        split (no1 - rnd1) t ((rnd1)::acc)
            | _ -> failwith "Splitting into Kshares failed"

        let list = List.init k (fun i -> (0))
        split no1 list []
    
    // Shares the values to players
    let shareValsK (name: string) (players: Player list) (values: int list): Player list =
        let rec shareValRec (values: int list) (players) (index: int) =
            match values with
            |   v::rest ->  let shareName = name + ((index+1) |> string)
                            let resultPlayers = players |> List.mapi (fun i p ->
                                                        if i = index then 
                                                            {p with Knows = Map.add shareName v p.Knows}
                                                        else 
                                                            p)
                            shareValRec rest resultPlayers (index+1)
            |   []  -> players
        shareValRec values players 0