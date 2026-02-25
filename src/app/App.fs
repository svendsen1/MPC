namespace App

module App =
    open MPCcore
    open Protocols

    [<EntryPoint>]
    let main argv =
        let players = argv[0]
        //let advers = argv[1]
        printfn "Created players: %A" players
        //printfn "Adversaries: %A" advers
        let playersInt = players |> int

        // Make a list of players
        let playersList = List.init playersInt (fun k -> PlayerModule.makePlayer k [])

        //Split the numbers into shares
        let sList = KofKshare.KShare 10 playersInt
        //sList |> List.iter (fun (x) -> printf " %d " x)

        let tList = KofKshare.KShare 20 playersInt
        //tList |> List.iter (fun (x) -> printf " %d " x)

        //Share the secrect 
        let playersList = KofKshare.shareValsK "s" playersList sList
        let playersList = KofKshare.shareValsK "t" playersList tList
        
        //Use the Add protocol
        let playersList = Addition.add playersList

        //Share the v_m values
        let playersList = SecretShare.shareVm playersList

        // Print the Players and their known values
        playersList |> List.iter (fun x -> 
            printf "Player_%d " (x.PlayerId + 1);
            Map.iter (fun k v -> printf "(%A,%d) " k v) x.Knows;
            printf "v%A" x.V_m  
            printfn ""
        )
        
        0