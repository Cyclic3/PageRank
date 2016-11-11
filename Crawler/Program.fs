// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.
open PageRank
open PageRank.IPC
[<EntryPoint>]
let main argv = 
    let p1 = new System.Net.Sockets.TcpListener(System.Net.IPEndPoint(System.Net.IPAddress.Any,int argv.[0]))      //IO.Pipes.NamedPipeClientStream(argv.[0])
    //let ep = new System.Net.IPEndPoint(System.Net.IPAddress.Loopback,int argv.[1])
    p1.Start()
    System.Console.Title <- sprintf "Crawler %s" argv.[0]
    printfn "Crawler %s ready" argv.[0]
    while true do 
        //f p1.Pending() then remote <- p1.AcceptTcpClient()
        let remote = p1.AcceptTcpClient()
        remote.SendTimeout <- 1000
        remote.ReceiveTimeout <- 1000
        try
            let v = 
                while remote.Available = 0 do ()
                let b = Array.zeroCreate remote.Available
                remote.GetStream().Read(b,0,b.Length)|>ignore
                b|>System.Text.Encoding.UTF8.GetString
            printfn "Getting %A..." v
            printf "\tParsing..."
            let b = System.UriBuilder(v)
            if b.Scheme <> "http" && b.Scheme <> "https" then b.Scheme <- "http"
            let v = b.Uri
            printfn "done"
            printf "\tCreating request..."
            let r' = sprintf "GET %s HTTP/1.1\r\nHost: %s\r\nConnection: close\r\nUser-Agent: CrawlerThing\r\nAccept: text/html, */*\r\n\r\n" (v.PathAndQuery) v.Host 
            let r = r' |> System.Text.Encoding.UTF8.GetBytes
            printfn "done"
            printf "\tResolving..."
            let ip = System.Net.Dns.GetHostAddresses(v.DnsSafeHost)
            printfn "done [%A]" ip.[0]
            printf "\tConnecting to %A..." ip.[0]
            let client = new System.Net.Sockets.TcpClient()
            client.Client.ReceiveBufferSize <- 67108864
            remote.SendTimeout <- 5000
            client.ReceiveTimeout <- 5000
            let getn,stream = 
                if b.Scheme = "https" then 
                    if not(client.ConnectAsync(ip,443).Wait(5000)) then failwithf "Timed out connection"
                    let s = new System.Net.Security.SslStream(client.GetStream())
                    s.AuthenticateAsClient(v.Host)
                    printfn "secure"
                    (fun () -> client.Available),s :> System.IO.Stream 
                else
                    if not(client.ConnectAsync(ip,80).Wait(5000)) then failwithf "Timed out connection"
                    printfn "done"
                    (fun () -> client.Available),client.GetStream() :> System.IO.Stream
            printf "\tSending request..."
            stream.Write(r,0,r.Length)
            printfn "done"
            printf "\tWaiting for response..."
            let a = async{while client.Available = 0 do ()}
            Async.RunSynchronously(a,10000)
            printfn "done"
            printf "\tReceiving response..."
            let r' = 
                //Credit: http://www.fssnip.net/2t
                let s = seq{while true do yield stream.ReadByte()|>char}
                let read_line() = try System.String(let a = Array.ofSeq(Seq.takeWhile ((<>)'\n') s) in a.[..a.Length-2]) with |_ -> ""
                //printfn "\nLine!";System.String(let v = Seq.takeWhile ((<>)'\n') s in Seq.take (Seq.length v - 2) v|>Array.ofSeq)
                let read_block() = let s' = seq{while true do yield read_line()} in Seq.takeWhile((<>) "") s'|>Array.ofSeq
                let start,header = 
                    let block = read_block()
                    block.[0],
                        block.[1..] 
                        |>Seq.map(fun i -> let i' = i.IndexOf(":") in i.[..i'-1],i.[i'+2..])|>Map.ofSeq
                let s = 
                    if Seq.contains ("Transfer-Encoding","chunked") <| Map.toSeq(header) then
                        let rec inner str = 
                            let n' = read_line()
                            let n = System.Convert.ToInt32(n',16)
                            if n = 0 then 
                                str 
                            else
                            inner(str+System.String(let v = Seq.take (n+2) s |> Array.ofSeq in v.[..v.Length-2]))
                        inner ""
                    else 
                        let b = Array.zeroCreate(getn())
                        stream.Read(b,0,b.Length)|>ignore
                        System.Text.Encoding.UTF8.GetString(b)
                start+(header |> Map.toSeq |> Seq.map (fun (i,j) -> i + ": " + j + "\r\n") |> String.concat "") + "\r\n" + s.Replace("\r","").Replace("\n","")
                |> System.Text.Encoding.UTF8.GetBytes
                |> Array.takeWhile ((<>)0uy)
            printfn "done"
            client.Close()
            remote.GetStream().Write(r',0,r'.Length)
            remote.Close()
            printfn "done"

            //HtmlDocument.Load(v).ToString()|>w
            //System.Net.WebClient().DownloadString v |> w
        with |e -> printfn "failed"; printfn "failed: %A" e; try remote.GetStream().Write("!"B,0,1); with |_ -> ()
    
    0 // return an integer exit code


