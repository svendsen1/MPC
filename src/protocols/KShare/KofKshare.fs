namespace Protocols

open MPCcore

module KofKshare =
    let rand = System.Random()

    // Spltts NO1 and NO2 into k random shares
    let KShare no1 no2 k = 
        let rec split no1 no2 list acc = 
            match list with
            | _::t when t.Length = 0 -> List.rev ((no1,no2)::acc)  
            | _::t ->   let rnd1 = rand.Next(0, no1) 
                        let rnd2 = rand.Next(0, no2) 
                        split (no1 - rnd1) (no2 - rnd2) t ((rnd1,rnd2)::acc)
            | _ -> failwith "Splitting into Kshares failed"

        let list = List.init k (fun i -> (0,0))
        split no1 no2 list []