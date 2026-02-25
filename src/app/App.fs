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
        let playersList = List.init playersInt (fun k -> (PlayerModule.makePlayer k []))

        //Split the numbers into shares
        let sList = KofKshare.KShare 10 playersInt
        //sList |> List.iter (fun (x) -> printf " %d " x)

        let tList = KofKshare.KShare 20 playersInt
        //tList |> List.iter (fun (x) -> printf " %d " x)

        let adversaryStructure= SecrecyStructure.singletonSecretStructure playersList

        let distributionList = SecretShare.tSetShare playersList adversaryStructure 

        //distributionList |> List.iter (fun x -> 
        //    printfn "Distribution Set: %A" (Set.toList x)
        //)

        let playersList = SecretShare.distributeShares distributionList sList tList playersList
        
        // Print the Players and their known values
        playersList |> List.iter (fun x -> 
            printf "Player_%d " (x.PlayerId + 1);
            Map.iter (fun k v -> printf "(%A,%d) " k v) x.Knows;
            printf "v%A" x.V_m  
            printfn ""
        )
        
        0