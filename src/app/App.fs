namespace App

module App =
    open MPCcore
    open Protocols.KofKshare
    open Protocols.Addition

    [<EntryPoint>]
    let main argv =
        let players = argv[0]
        let advers = argv[1]
        printfn "Created players: %A" players
        printfn "Adversaries: %A" advers
        let playersInt = players |> int

        // Make a list of players
        let playersList = List.init playersInt (fun k -> (PlayerModule.makePlayer k []))

        //Split the numbers into shares
        let sList = KShare 10 playersInt
        //sList |> List.iter (fun (x) -> printf " %d " x)

        let tList = KShare 20 playersInt
        //tList |> List.iter (fun (x) -> printf " %d " x)

        //Share the secrect 
        let playersList = shareValsK "s" playersList sList
        let playersList = shareValsK "t" playersList tList
        
        //Use the Add protocol
        let playersList = add playersList
        playersList |> List.iter (fun x -> 
            printf "Player_%d " (x.PlayerId + 1);
            Map.iter (fun k v -> printf "(%A,%d) " k v) x.Knows;
            printf "v_sum %A" x.ResV;  
            printfn ""
        )
        
        0