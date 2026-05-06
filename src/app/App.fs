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
        printfn "Input 1: 10" 

        let tList = KofKshare.KShare 20 playersInt
        //tList |> List.iter (fun (x) -> printf " %d " x)
        printfn "Input 2: 20"
        printfn ""
        let adversaryStructure= SecrecyStructure.singletonSecretStructure playersList

        let distributionList = SecretShare.tSetShare playersList adversaryStructure 


        let playersList = SecretShare.distributeShares distributionList sList tList playersList

        let Us = SecretShare.makeU playersInt playersList

        PrettyPrint.prettyPrintUs Us

        let playersList = PassiveMul.passiveMul playersList Us

        let playersList = SecretShare.shareVm playersList

        PrettyPrint.prettyPrintPlayerList playersList

        
        0