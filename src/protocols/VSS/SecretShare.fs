namespace Protocols

module SecretShare = 
    open MPCcore
    
    let shareVm (players: Player list) = 
                                    //List.choose removes None auto
        let listV = players |> List.choose (fun p -> p.ResV)
        players |> List.map (fun p -> {p with V_m = listV})
        
