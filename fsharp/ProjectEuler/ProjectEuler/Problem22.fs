module Problem22

open System.Net
open System.Text.RegularExpressions

let webClient = new WebClient()
let namesUrl = "https://projecteuler.net/project/resources/p022_names.txt"
let splitNames (x : string) = x.Split [| ',' |]
let removeQuotes x = Regex("^\"([A-Z]+)\"").Match(x).Groups.[1].Value
let getCharValue x = int x - int 'A' + 1
let getNameValue = Seq.map getCharValue >> Seq.sum
let getNameScore index value = (index + 1) * value

let result =
    webClient.DownloadString(namesUrl)
    |> splitNames
    |> Array.toSeq
    |> Seq.map removeQuotes
    |> Seq.sort
    |> Seq.map getNameValue
    |> Seq.mapi getNameScore
    |> Seq.sum
