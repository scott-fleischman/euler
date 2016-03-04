module Problem31

type Way = { sum : int }
let emptyWay = { sum = 0 }
let addCoin c { sum = sum } = { sum = c + sum }
let isMatch n { sum = sum } = n = sum
let combine { sum = sum1 } { sum = sum2 } = { sum = sum1 + sum2 }

let singleCoinWays limit n =
    Seq.initInfinite ((+) 1)
    |> Seq.takeWhile (fun x -> x * n <= limit)
    |> Seq.scan (fun w _ -> addCoin n w) emptyWay
    |> Seq.skip 1
    |> Seq.toList

let rec build n =
    function
    | [] -> []
    | c :: cs as coins ->
        let rest = build n cs
        let current = singleCoinWays n c
        let currentMatches, currentRemaining = current |> List.partition (isMatch n)
        let currentMatchesMore =
            currentRemaining
            |> List.map (fun w -> w, build (n - w.sum) cs)
            |> List.map (fun (w, ws) -> List.map (fun w' -> combine w w') ws)
            |> List.concat
            |> List.where (isMatch n)
        currentMatches @ currentMatchesMore @ rest

let answer =
    let coins = [1; 2; 5; 10; 20; 50; 100; 200]
    let target = 200
    let values = build target coins
    values |> List.length
