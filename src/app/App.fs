namespace App

module App =
    open MPCcore
    open Protocols.KofKshare

    [<EntryPoint>]
    let main argv =
        let players = argv[0]
        let advers = argv[1]
        printfn "Created players: %A" players
        printfn "Adversaries: %A" advers
        let playersInt = players |> int

        //Split the numbers into shares
        let sharesList = KShare 10 10 playersInt
        sharesList |> List.iter (fun (x,y) -> printf " %d,%d " x y)

        //Share the secrect 

        0