// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.
open PageRank

let ep a p = System.Net.IPEndPoint(System.Net.IPAddress.Parse a,p)
open PageRank.Front
[<EntryPoint>]
let main argv = 
    let crawlers = 
        [|
            6996
            9669
            9002
            9001
            9003
            9004
            9005
            9006
        |]
        |> Array.map IPC.existing_crawler
        |> Array.append
        <|
        [|
            ep "192.168.1.229" 9000
            ep "192.168.1.229" 9002
            //ep "192.168.1.112" 6001
        |]
    let s = WebServer crawlers (System.Net.IPEndPoint(System.Net.IPAddress.Any,65432))
    0 // return an integer exit code

