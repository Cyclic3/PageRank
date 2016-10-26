// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.
open PageRank
open PageRank.Front
open C_Omega
open C_Omega.Helpers
[<EntryPoint>]
let main argv = 
    WebServer [||] (System.Net.IPEndPoint(System.Net.IPAddress.Any,65432))
    0 // return an integer exit code

