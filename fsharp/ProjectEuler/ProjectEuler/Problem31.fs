module Problem31

let valuesToLimit limit n =
    Seq.initInfinite ((+) 1)
    |> Seq.takeWhile (fun x -> x * n <= limit)
    |> Seq.scan (fun xs _ -> n :: xs) []
    |> Seq.skip 1
    |> Seq.toList

let rec build n =
    function
    | [] -> []
    | c :: cs as coins ->
        let rest = build n cs
        let current = valuesToLimit n c
        let currentMatches, currentRemaining = current |> List.partition (fun xs -> List.sum xs = n)
        let currentMatchesMore =
            currentRemaining
            |> List.map (fun xs -> xs, build (n - List.sum xs) cs)
            |> List.map (fun (xs, yss) -> List.map (fun ys -> xs @ ys) yss)
            |> List.concat
            |> List.where (fun xs -> List.sum xs = n)
        currentMatches @ currentMatchesMore @ rest

let answer =
    let coins = [1; 2; 5; 10; 20; 50; 100; 200]
    let target = 200
    let values = build target coins
    values |> List.length
