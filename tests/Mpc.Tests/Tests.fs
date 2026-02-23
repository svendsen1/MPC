module Tests

open System
open Xunit

open MPCcore
open Protocols

let rec checkSums (players: Player list) (sum: int) = 
    match players with
    | [] -> true
    | p::tail when (List.sum p.V_m) = sum -> checkSums tail sum
    | _ -> false

[<Fact>]
let ``Addition`` () =
    let playersInt = 5

    // Make a list of players
    let playersList = List.init playersInt (fun k -> PlayerModule.makePlayer k [])

    //Split the numbers into shares
    let sList = KofKshare.KShare 10 playersInt
    let tList = KofKshare.KShare 20 playersInt

    //Share the secrect 
    let playersList = KofKshare.shareValsK "s" playersList sList
    let playersList = KofKshare.shareValsK "t" playersList tList
    
    //Use the Add protocol
    let playersList = Addition.add playersList

    //Share the v_m values
    let playersList = SecretShare.shareVm playersList
    
    Assert.True(checkSums playersList 30)