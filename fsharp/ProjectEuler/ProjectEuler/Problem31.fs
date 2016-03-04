module Problem31

let (/?) n d = n % d = 0
let intermediateSums limit n = [n..n..(limit - 1)]

let rec countWays n =
    function
    | [] -> 0
    | c :: cs ->
        let current = if n /? c then 1 else 0
        let middle = intermediateSums n c |> List.sumBy (fun x -> countWays (n - x) cs)
        let rest = countWays n cs
        current + middle + rest

let answer =
    let coins = [1; 2; 5; 10; 20; 50; 100; 200]
    let target = 200
    countWays target coins
