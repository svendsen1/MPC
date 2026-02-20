namespace Protocols

module Addition= 
    open MPCcore
    
    let add (players: Player list) =
        players 
        |> List.map (fun p -> 
            let sum = 
                p.Knows 
                |> Map.values
                |> Seq.sum
            {p with ResV = Some sum})

