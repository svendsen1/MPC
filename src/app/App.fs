namespace App

module App =
    open MPCcore
    open Protocols

    let prettyPrintPlayerList (playersList: list<Player>) =       
            playersList |> List.iter (fun x -> 
            printf "Player_%d shares:" (x.PlayerId + 1);
            Map.iter (fun k v -> printf "(%A,%d) " k v) x.Knows;
            printf " Products: %A Sum of products:%d" x.V_m (List.sum x.V_m)
            printfn ""
        )
    let prettyPrintUs (Us) = 
        Us 
        |> List.iteri (fun playerIdx pairs ->
            printf "Player_%d multiplication pairs: " (playerIdx + 1)
            if List.isEmpty pairs then
                printf "none"
            else
                pairs 
                |> List.map (fun (i, j) -> sprintf "(s%d,t%d)" i j)
                |> String.concat ", "
                |> printf "%s"
            printfn ""
        )
        printfn""


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
        printfn "Input 1: 10" 

        let tList = KofKshare.KShare 20 playersInt
        //tList |> List.iter (fun (x) -> printf " %d " x)
        printfn "Input 2: 20"
        printfn ""
        let adversaryStructure= SecrecyStructure.singletonSecretStructure playersList

        let distributionList = SecretShare.tSetShare playersList adversaryStructure 


        let playersList = SecretShare.distributeShares distributionList sList tList playersList

        let Us = SecretShare.makeU playersInt playersList

        prettyPrintUs Us

        let playersList = PassiveMul.passiveMul playersList Us

        let playersList = SecretShare.shareVm playersList

        prettyPrintPlayerList playersList

        
        0