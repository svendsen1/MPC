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

let getShares (player: Player) (name: string) = 
    let rec getRec (knowsList: list<string*int>) (acc: list<int>) =
        match knowsList with
        |   [] -> acc
        |   (str, v)::rest when str.Chars 0 = name.Chars 0 -> getRec rest (v::acc)
        |   _::rest -> getRec rest acc
    getRec (player.Knows |> Map.toList) []

let rec canRecunstructBrute (players: Player list) (secret: int) (name: string)= 
    match players with
    | [] -> false
    | p::_ when List.sum (getShares p name) = secret -> true
    | p::tail -> canRecunstructBrute tail secret name

let getCollectiveKnowledge (playerSet: Set<Player>) (playersList: list<Player>) = 
    let playerToRec = Set.toList playerSet
    let rec setInner (playersToRec: list<Player>) (collectiveKonwledge: list<string*int>) =
        match playersToRec with
        | [] -> collectiveKonwledge
        | p::rest ->    let player = playersList |> List.find (fun x -> x.PlayerId = p.PlayerId)
                        let knowledge = Map.toList player.Knows
                        setInner rest (knowledge@collectiveKonwledge)
    setInner (playerSet |> Set.toList) []

let canRecunstruct (players: Player list) (secStruct: SecrecyStructure.SecrecyStructure) (noPlayers: int) = 
    //
    let playerSets = Set.toList secStruct
    let rec recunstructInner (playerSets: Set<Player> list) =
        match playerSets with
        |   []  -> false 
        |   set::rest ->    let knows = getCollectiveKnowledge set players
                            let uniqueSorted =
                                knows
                                |> List.distinct
                                |> List.sort
                            let sShares = uniqueSorted |> List.filter (fun (name,_) -> name.StartsWith("s"))
                                                             |> List.length
                            let tShares = uniqueSorted |> List.filter (fun (name,_) -> name.StartsWith("t"))
                                                             |> List.length
                            if sShares = noPlayers || tShares = noPlayers then
                                false
                            else
                                recunstructInner rest
    recunstructInner playerSets

[<Fact>]
let ``Addition`` () =
    let playersInt = 5
    let secret1 = 10
    let secret2 = 20

    // Make a list of players
    let playersList = List.init playersInt (fun k -> PlayerModule.makePlayer k [])

    //Split the numbers into shares
    let sList = KofKshare.KShare secret1 playersInt
    let tList = KofKshare.KShare secret2 playersInt

    //Share the secrect 
    let playersList = KofKshare.shareValsK "s" playersList sList
    let playersList = KofKshare.shareValsK "t" playersList tList
    
    //Use the Add protocol
    let playersList = Addition.add playersList

    //Share the v_m values
    let playersList = SecretShare.shareVm playersList
    
    let sum = secret1 + secret2
    Assert.True(checkSums playersList sum)
    
    Assert.False(canRecunstructBrute playersList secret1 "s")
    Assert.False(canRecunstructBrute playersList secret2 "t")

[<Fact>]
let ``Passive Mul`` () = 
    let playersInt = 4
    let secret1 = 10
    let secret2 = 20

    // Make a list of players
    let playersList = List.init playersInt (fun k -> PlayerModule.makePlayer k [])

    //Split the numbers into shares
    let sList = [1;2;3;4]
    let tList = [4;5;6;5]

    let adversaryStructure= SecrecyStructure.singletonSecretStructure playersList

    let distributionList = SecretShare.tSetShare playersList adversaryStructure 

    let playersList = SecretShare.distributeShares distributionList sList tList playersList

    let Us = SecretShare.makeU playersInt playersList

    let playersList = PassiveMul.passiveMul playersList Us

    let playersList = SecretShare.shareVm playersList

    Assert.True(checkSums playersList 200)

    Assert.False(canRecunstruct playersList adversaryStructure playersInt)