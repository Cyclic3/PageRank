namespace PageRank 
module IPC = 
    let mutable CrawlerLocation = "../../../Crawler/bin/Debug/Crawler.exe"  //"~/PageRank/Crawler/bin/Debug/Crawler.exe"
    type Crawler = System.Net.IPEndPoint
    let extract (str:string) = 
        let v = str.IndexOf("\r\n\r\n")
        str.[v+4..]  
    let existing_crawler(port) : Crawler = System.Net.IPEndPoint(System.Net.IPAddress.Loopback,port)
    (* 
    let gen_crawler() : Crawler = 
        let ep = System.Net.IPEndPoint(System.Net.IPAddress.Loopback,0)
        let l = new System.Net.Sockets.UdpClient(System.Net.IPEndPoint(System.Net.IPAddress.Loopback,0))
        let s = new System.Diagnostics.ProcessStartInfo("mono",CrawlerLocation+" "+string ep.Port+" "+string (l.Client.LocalEndPoint:?>System.Net.IPEndPoint).Port,CreateNoWindow=false,UseShellExecute=false)
        System.Diagnostics.Process.Start(s)|>ignore
        let v = System.Net.IPEndPoint(0L,0) |> ref
        l.Connect ep
        (fun a -> let b = System.Text.Encoding.UTF8.GetBytes(a:string) in l.Send(b,b.Length) |> ignore), (fun() -> l.Receive v |> System.Text.Encoding.UTF8.GetString)*)
[<AutoOpen>]
module Core =
    let rand = System.Random()
    let randin (a:'a[]) = a.[rand.Next(0,a.Length)]
    let side f a = f a;a
    let say s a = printfn "%s" s;a
    let just f a = f();a
    //Having a type abbreviation for these means I dio not have to replace each time I change my mind
    type Page = string
    type Rank = float//System.Decimal
    //I translated the psudocode from https://en.wikipedia.org/wiki/Quicksort#Lomuto_partition_scheme, and then applied some custom parallelisation, and then inverted it
    let quicksort keyf (arr:'a[]) =
        ///The swap function for quicksort
        let swap(a:'a[],i:int,j:int) = let k = a.[i] in a.[i] <- a.[j]; a.[j] <- k
        let rec partition(a:'a[],lo,hi) =
            let pivot = keyf a.[hi]
            let mutable i = lo
            for j = lo to hi-1 do
                if keyf(a.[j]) >= pivot then
                    swap(a,i,j)
                    i <- i + 1
            swap(a,i,hi)
            i
        let n = ref 0
        let rec quicksort (a:'a[],lo,hi) =
            if lo < hi then
                System.Threading.Interlocked.Increment(n)|>ignore
                System.Threading.Interlocked.Increment(n)|>ignore
                let p = partition(a,lo,hi)
                async{quicksort(a,lo,p-1);System.Threading.Interlocked.Decrement(n)|>ignore}|>Async.Start
                async{quicksort(a,p+1,hi);System.Threading.Interlocked.Decrement(n)|>ignore}|>Async.Start
        quicksort(arr,0,arr.Length-1)
        //Wait until there are no more jobs left
        while !n>0 do ()
        arr
    ///The main PageRank function
    let PageRank(pages : Map<string,string[]*string>, max) : Map<string,Rank*string>=
        ///The number of pages
        let N = float pages.Count
        //The damping factor. According to wikipedia, there is an 85% chance that a client will stop at any hyperlink level.
        let d = 0.85
        let iter = ref 10000
        let rec rank : string * Map<string,string[]*string> * int -> Rank = function
            |_,_,0 -> 1. //This is the initial value. 'All pages are created equal' or something...
            |pi,p,t ->
                let v = 
                    let f = function 
                        |pj,([||],s) ->
                            rank(                   //Rank:
                                pj,                 //relative to the page 'pj'
                                pages.Remove pj,    //look through everything but pj

                                t-1                 //Decrement the iteration count
                            )
                        |pj,(i,s) -> if Array.contains pi i then rank(pj, pages.Remove pj, t-1) / (float(i.Length)) else 0.
                    Array.Parallel.map (fun i -> async{return f i})<| Map.toArray p //This seems to be faster than running it all under 1 parallel (by about 9x)
                    |> Async.Parallel
                    |> Async.RunSynchronously
                    |> Array.sum
                (1. - d) + (d * v)
        Array.Parallel.map (fun (i,(_,s)) -> i,(rank(i, Map.remove i pages, max),s)) (Map.toArray pages) |> Map.ofArray
    //A quick function to iterate through lists, and taking the first n to fit a function
    let get_n f n s = 
        let rec inner = function
            |_   ,v,0
            |[]  ,v,_ -> v|>List.rev|>Seq.ofList
            |a::b,v,i when f a -> inner(b,a::v,i-1)
            |_::b,v,i -> inner(b,v,i)
        inner(List.ofSeq s,[],n)
module Agents = 
    open FSharp.Data
    open IPC
    let rec crawler_get(uri:string,crawler:Crawler) = 
        let client = new System.Net.Sockets.TcpClient()
        client.Connect crawler
        let b = System.Text.Encoding.UTF8.GetBytes uri
        client.GetStream().Write(b,0,b.Length)
        while client.Available = 0 do ()
        let response = Array.zeroCreate (client.Available)
        client.GetStream().Read(response,0,response.Length)|>ignore
        let v = System.Text.Encoding.UTF8.GetString response
        if v = "!" then failwithf "Error in crawler"
        //v|>HtmlDocument.Parse
        let i = v.IndexOf('\r')-1
        let code = 
            v.[..i].Split(' ')
            |> Array.find(fun s -> Array.contains s.[0] [|'0'..'9'|])
            |> int
        
        match code with
        |200 -> 
            let d = v.[i+2..]|>extract//|>HtmlDocument.Parse
            Choice1Of2 d
        |300 -> 
            let i = v.ToLower().IndexOf("location: ")
            let v' = v.[i+10..]
            let v'' = v'.[..v'.IndexOf('\r')-1]
            //printfn "300 -> %s" v''
            Choice2Of2 v''
        |301 -> 
            let i = v.ToLower().IndexOf("location: ")
            let v' = v.[i+10..]
            let v'' = v'.[..v'.IndexOf('\r')-1]
            //printfn "301 -> %s" v''
            Choice2Of2 v''
        |302 -> 
            let i = v.ToLower().IndexOf("location: ")
            let v' = v.[i+10..]
            let v'' = v'.[..v'.IndexOf('\r')-1]
            //printfn "302 -> %s" v''
            Choice2Of2 v''
        |303 -> 
            let i = v.ToLower().IndexOf("location: ")
            let v' = v.[i+10..]
            let v'' = v'.[..v'.IndexOf('\r')-1]
            //printfn "303 -> %s" v''
            Choice2Of2 v''
        |307 -> 
            let i = v.ToLower().IndexOf("location: ")
            let v' = v.[i+10..]
            let v'' = v'.[..v'.IndexOf('\r')-1]
            //printfn "303 -> %s" v''
            Choice2Of2 v''
        |308 -> 
            let i = v.ToLower().IndexOf("location: ")
            let v' = v.[i+10..]
            let v'' = v'.[..v'.IndexOf('\r')-1]
            //printfn "303 -> %s" v''
            Choice2Of2 v''
        |403 -> failwithf "403: %A" uri
        |404 -> failwithf "404: %A" uri
        |503 -> failwithf "503: %A" uri
        |i -> printfn "%A, %A" i v|> failwith "=("

        |> function
            |Choice1Of2 s -> s
            |Choice2Of2 v'' -> if uri = v'' then failwith "Cyclic!" else v''

    let crawler_getinf(uri:string,crawler:Crawler, t:System.Threading.CancellationToken, d:System.Collections.Generic.HashSet<_>) = 
        let e = new Event<string*string[]*string>()
        let e' = e.Publish
        let a = 
            async{
                let rec inner todo =
                    t.ThrowIfCancellationRequested()
                    if todo = "" then failwithf "Bad uri!"
                    let g = crawler_get(todo,crawler)
                    //v.Descendants "a" 
                    //|> Array.ofSeq 
                    //|> Array.Parallel.choose (fun i -> i.TryGetAttribute("href")) 
                    System.Text.RegularExpressions.Regex("href=[\\\"\\']").Matches(g)
                    |> Seq.cast<System.Text.RegularExpressions.Match>
                    |> Array.ofSeq
                    |> Array.Parallel.map(fun i -> try let s' = g.[i.Index+6..] in s'.[..s'.IndexOfAny([|''';'\"'|])-1] with |_ -> "")//"
                    |> function
                        |[||] -> failwithf "Tree end @ %A" todo
                        |i    -> i
                    |> Array.distinct
                    |> Array.Parallel.choose (fun i -> 
                        let v = 
                            i
                            |> fun s -> let i = s.IndexOf "?" in if i = -1 then s else s.[..i-1]
                            |> fun s -> let i = s.IndexOf "#" in if i = -1 then s elif i = 0 then "" else s.[..i-1]
                        if v.Length = 0 then None //Dodgy URL
                        elif v.StartsWith("http") then Some v //http(s):// will match this
                        elif v.[0] = '/' then //For urls relative to the root of the website
                            let b = System.UriBuilder(todo)
                            if b.Scheme <> "http" && b.Scheme <> "https" then b.Scheme <- "http"
                            System.Uri(b.Uri,v)|>string|>Some //This constructor of System.Uri constructs a Uri based on the first argument, relative to the second argument
                            //b.Uri.ToString() |> Some
                        elif v.StartsWith("javascript") then None //We do not care about javascript function calls
                        else //Urls relative to the current path
                            let b = System.UriBuilder()
                            System.Uri(System.Uri(todo),v)|>string|>Some //This constructor of System.Uri constructs a Uri based on the first argument, relative to the second argument
                    ) 
                    |> Array.filter(fun i -> if i|>d.Contains then false else d.Add i|>ignore; true)
                    |> fun j -> e.Trigger(todo,j,g);j
                    //|> fun j -> printfn "%A" j.Length; j
                    |> Array.Parallel.iter (fun i -> try inner i with |e -> ())//printfn "ERR:\n%A" e)
                    
                inner(uri)
            }
        a,e'//a,e'
    let crawlers_get_depth(start:string,crawlers:Crawler[], pages:System.Collections.Generic.Dictionary<string,string[]*string>, depth:int64) = 
        let d = System.Collections.Generic.HashSet(pages.Keys)
        let depth = ref depth
        let free = System.Collections.Generic.Queue(crawlers)
        let get_free f = 
            let rec crawler = lock free (fun () -> while free.Count = 0 do () done;free.Dequeue())
            try 
                let result = f crawler
                free.Enqueue crawler
                result
            with |_ -> free.Enqueue crawler;reraise()
        let check a = if !depth < 0L then failwith "Done!"
        let rec inner todo = 
            if System.Threading.Interlocked.Decrement depth > 0L then
                if todo = "" then failwithf "Bad uri!"
                if lock d (fun () -> d.Add todo) then
                    let g = get_free (fun crawler -> crawler_get(todo,crawler))
                    //v.Descendants "a" 
                    //|> Array.ofSeq 
                    //|> Array.Parallel.choose (fun i -> i.TryGetAttribute("href")) 
                    System.Text.RegularExpressions.Regex("href=[\\\"\\']").Matches(g)
                    |> Seq.cast<System.Text.RegularExpressions.Match>
                    |> Array.ofSeq
                    |> Array.Parallel.map(fun i -> try let s' = g.[i.Index+6..] in s'.[..s'.IndexOfAny([|''';'\"'|])-1] with |_ -> "")//"
                    |> function
                        |[||] -> failwithf "Tree end @ %A" todo
                        |i    -> i
                    |> side check
                    |> Array.distinct
                    |> Array.Parallel.choose (fun i -> 
                        try
                            let v = 
                                i
                                |> fun s -> let i = s.IndexOf "?" in if i = -1 then s else s.[..i-1]
                                |> fun s -> let i = s.IndexOf "#" in if i = -1 then s elif i = 0 then "" else s.[..i-1]
                            if v.Length = 0 then None //Dodgy URL
                            elif v.StartsWith("http") then Some v //http(s):// will match this
                            elif v.[0] = '/' then //For urls relative to the root of the website
                                let b = System.UriBuilder(todo)
                                if b.Scheme <> "http" && b.Scheme <> "https" then b.Scheme <- "http"
                                let v = if v.StartsWith("/") then v else "/"+v
                                b.Path <- v
                                b.Uri.ToString() |> Some
                            elif v.StartsWith("javascript") then None //We do not care about javascript function calls
                            elif v.StartsWith("mailto:")    then None //We do not care about email
                            else //Urls relative to the current path
                                let b = System.UriBuilder(todo)
                                if b.Scheme <> "http" && b.Scheme <> "https" then b.Scheme <- "http"
                                b.Path <- b.Path.[..b.Path.LastIndexOf('/')]+v
                                b.Uri.ToString() |> Some
                        with |e -> failwithf "Broke at dealing wiht link %A\n%A" i e
                    ) 
                    |> Array.filter(d.Contains >> not)
                    |> fun i -> pages.Add(todo,(i,g));i
                    |> side check
                    |> Array.Parallel.map (fun i -> 
                        async{
                            check()
                            do! Async.SwitchToNewThread()
                            inner i
                        })//printfn "ERR:\n%A" e)
                    |> Async.Parallel
                    |> Async.Catch
                    |> Async.Ignore
                    |> Async.RunSynchronously
        inner start
module Front = 
    open System.Net
    open System.Net.Sockets
    open Agents
    let WebServer crawlers (localep:IPEndPoint) =
        let i = new TcpListener(localep)
        let requests = new System.Collections.Generic.Dictionary<string*string,unit->string option>()
        i.Start()
        let get_string b = System.Text.Encoding.UTF8.GetString b |> (fun s -> s.Replace("\r",""))
        let get_bytes (b:string) = System.Text.Encoding.UTF8.GetBytes b
        let read(r:TcpClient) =
            while r.Available <= 0 do ()
            let b = Array.zeroCreate<byte> r.Available
            r.Client.Receive(b)|>ignore
            get_string b
        let write(r:TcpClient) b = r.GetStream().Write(b,0,b.Length)
        let get_parts (s:string) = 
            let i = s.IndexOf("\n\n")
            s.[..i-1],s.[i+2..]
        let rec replace_all = function
            |s,[]       -> s:string
            |s,(a,b)::c -> s.Replace((a:string),b)
        let decode = System.Web.HttpUtility.UrlDecode
        let get_posts (s:string) = 
            s.Split('&')
            |> Array.map (fun s -> let s' = s.Split('=') in decode s'.[0], decode s'.[1])
            |> Map.ofArray
        let acc = 
            let p = 
                System.IO.File.ReadAllText("acc.nsv").Split('\x01') //Read from db and split at the bytes with value 1
                |> Array.skip 1
                |> Array.map (fun s -> let v = s.Split '\x00' in v.[0],(v.[1..v.Length-2],v.[v.Length-1]))//The first value is the key, the rest are elements of the value
            new System.Collections.Generic.Dictionary<string,string[]*string>(dict p)
        let pr =
            try
                System.IO.File.ReadAllText("pr.nsv").Split('\x01') //Read from db and split at the bytes with value 1
                |> Array.skip 1
                |> Array.map (fun s -> let v = s.Split '\x00' in v.[0],v.[1])//The first value is the key, the rest are elements of the value
                |> ref
            with |e -> printfn "WARNING!!!\nCORRUPTED pr.nsv!\n%A" e;ref [||]
        while true do
            let r = i.AcceptTcpClient()
            let s = r.GetStream()
            let v = ref null
            async{
                let str = 
                    try
                        let head,body = read r |> get_parts
                        v:=head
                        let verb,path = 
                            let init = head.[..head.IndexOf '\n']
                            let i = init.IndexOf ' '
                            init.[..i-1],init.[i+1..init.IndexOf(' ',i+1)-1]
                        match verb with
                        |"GET" ->
                            match path with
                            |"/"
                            |"/index.html" -> 
                                let body = System.IO.File.ReadAllText("index.html")
                                //let head,body = get_parts page
                                let body_replacements = ["%acc.Count%",string acc.Count]
                                let body' = replace_all(body,body_replacements)
                                //let head_replacements = ["%Content-Length%",string body'.Length]
                                //replace_all(head,head_replacements)+"\n\n"+body'
                                body'
                                |> get_bytes
                                |> Some
                            |"/db.html" ->
                                let body = System.IO.File.ReadAllText("db.html")
                                //let head,body = get_parts page
                                let body_replacements = ["%acc.Count%",string acc.Count]
                                let body' = replace_all(body,body_replacements)
                                //let head_replacements = ["%Content-Length%",string body'.Length]
                                //replace_all(head,head_replacements)+"\n\n"+body'
                                body'
                                |> get_bytes
                                |> Some
                            |"/grab/acc.nsv" -> System.IO.File.ReadAllBytes "acc.nsv"|>Some
                            |"/grab/pr.nsv" -> System.IO.File.ReadAllBytes "pr.nsv"|>Some
                            |s                   -> "Not a valid GET page: " + s |> get_bytes|>Some
                        |"POST" ->
                            match path with
                            |"/query"            -> 
                                let form = get_posts body
                                let q = form.["query"]
                                get_n (fun (uri,s:string) -> s.Contains(q.ToLower())) 10 !pr
                                |> Seq.fold (fun acc (url,page) -> acc + (sprintf "<a href='%s'>%s</a><br>" url url)) ""
                                |> fun i -> if i.Length = 0 then "<h1>No results</h1>!" else i
                                |> get_bytes
                                |> Some
                            |"/index"            ->
                                let form = get_posts body
                                let q = form.["query"]
                                let currentDepth = int64 form.["depth"]
                                crawlers_get_depth(q,crawlers,acc,currentDepth)|>say "Crawled"
                                "Done!<br>Use refresh to integrate it into the program"B
                                |> Some
                            |"/db/rank" ->
                                let form = get_posts body
                                let q = form.["query"].ToLower()
                                Seq.findIndex(fun (q':string,_) -> q'.ToLower() = q) !pr
                                |> sprintf "%s is at position %i/%i" q
                                <|pr.Value.Length
                                |> string
                                |> get_bytes
                                |> Some
                            |"/db/get" ->
                                let form = get_posts body
                                let q = form.["query"].ToLower()
                                match Array.tryPick(fun (q':string,s:string) -> if q'.ToLower() = q then Some s else None) !pr with
                                |None    -> "<h1>No page was found wth that url</h1>"
                                |Some(v) -> v

                                |> get_bytes
                                |> Some
                            |"/user/refresh" ->
                                let s = new System.Diagnostics.Stopwatch()
                                let elapsed() = s.Elapsed.ToString("dd\:hh\:mm\:ss\.FFF")
                                Array.append (System.IO.File.ReadAllBytes("header.txt")) "\nTransfer-Encoding: chunked\n\n"B |> write r
                                let say s a = 
                                    let b = s + " [" + elapsed() + "]" + "<br>"|> get_bytes
                                    System.Convert.ToString(b.Length,16)+"\r\n"|>get_bytes|>write r
                                    "\r\n"B |> Array.append b |> write r
                                    a
                                s.Start()
                                PageRank(Seq.zip acc.Keys acc.Values|>Map.ofSeq,10)|> say "Ranked"
                                |> Map.toArray |> say "Arrayed"
                                |> Array.Parallel.map (fun (i,(j,k)) -> i,j,System.Web.HttpUtility.HtmlDecode(k)) |> say "Decoded"
                                |> quicksort (fun(i,j,k) -> j) |> say "Sorted"
                                |> Array.Parallel.map (fun(i,j,k) -> i,k.ToLower()) |> say "Lowered"
                                |> fun i -> pr := i |> say "Updated"
                                //It gets REALLY annoying when I have to wait around for it to load ALL the pages again, so I am going to make it persist on disk.
                                (
                                    let data = 
                                        Array.Parallel.map(fun (kvp:System.Collections.Generic.KeyValuePair<_,_>) -> 
                                            let key,(a,b) = kvp.Key,kvp.Value
                                            "\x01" + key + "\x00" + String.concat "," a + "\x00" + b
                                        ) <| Array.ofSeq acc
                                        |> say "Disk 1 map"
                                        |> System.String.Concat |> say "Disk 1 concat"
                                    System.IO.File.WriteAllText("acc.nsv",data) |> say "Disk 1 write"
                                )
                                (
                                    let data = 
                                        Array.Parallel.map(fun (key,value) -> 
                                            "\x01" + key + "\x00" + value
                                        ) !pr
                                        |> say "Disk 2 map"
                                        |> System.String.Concat |> say "Disk 2 concat"
                                    System.IO.File.WriteAllText("pr.nsv",data) |> say "Disk 2 write"
                                )
                                write r "0\r\n\r\n"B
                                None
                                //sprintf "Done in %s" (s.Elapsed.ToString("dd\:hh\:mm\:ss\.FFF"))
                            |"/admin/authenticate" ->
                                let form = get_posts body
                                let g1 = form.["g1"]
                                let g2 = form.["g2"]
                                match requests.TryGetValue((g1,g2)) with
                                |false,_ -> Some "Bad code or key"
                                |true,v  -> v()

                                |> Option.map get_bytes
                            |"/admin/delete" ->
                                let g1 = System.Guid.NewGuid().ToString() |> side (printfn "Key: %A")
                                let g2 = System.Guid.NewGuid().ToString()
                                let q = get_posts(body).["query"].ToLower()
                                requests.Add((g1,g2),(fun() -> 
                                    if acc.Remove(q) then sprintf "Successfully removed %A from acc" q 
                                    else sprintf "Failed to remove %A from acc" q

                                    |>Some
                                    ))
                                "Code: " + g2 
                                |> get_bytes
                                |> Some
                            |s -> "Not a valid POST page: " + s |> get_bytes |> Some
                        |s                   -> "Not a valid verb: " + s |> get_bytes |> Some
                    with 
                    |e -> sprintf "There was an error processing your request: <br>%A<br><br>%A" !v e |> get_bytes |> Some
                try
                    if str.IsSome then
                        Array.append "\n\n"B str.Value
                        |> Array.append (System.IO.File.ReadAllBytes("header.txt"))
                        |> write r
                finally
                r.Close()
            }
            |>Async.Start

        (*
    let HTMLagent(uri:string,t:System.Threading.CancellationToken,crawler:Crawler) = 
        let e = new Event<string*string[]>()
        let d = System.Collections.Generic.HashSet<_>()
        let e' = e.Publish
        let w,r = 
            let a,b = crawler
            write a, fun() -> read b
        let asyncAdd(uri:string) = async{lock d (fun()-> d.Add uri)|>ignore}
        let rec agent(uri:string) = 
            async{
                try
                    t.ThrowIfCancellationRequested()
                    if not(d.Contains uri) then //&& not(t.IsCancellationRequested)
                        do! asyncAdd uri
                        do! Async.SwitchToNewThread()
                        let c = new System.Net.WebClient()
                        let r = uri|>c.OpenRead|>HtmlDocument.Load
                        //Get all links
                        let links = 
                            r.Descendants "a"
                            |> Array.ofSeq
                            |> Array.Parallel.choose(fun v -> 
                                v.TryGetAttribute("href")
                                |>Option.map(fun v ->
                                    t.ThrowIfCancellationRequested()
                                    let v = 
                                        v.Value()
                                        |> fun s -> let i = s.IndexOf('?') in if i < 1 then s else s.[..i-1]

                                    let v' = 
                                        if v.StartsWith("http") then v
                                        elif v.[0] = '/' then uri.[..uri.IndexOf('/',8)-1]+v
                                        elif v.[0] = '#' then uri
                                        else uri+"/"+v
                                    v'
                                    |> agent
                                    |> Async.Start
                                    v'
                                )
                            )
                            //|>Array.ofSeq
                        e.Trigger(uri,links)
                with |_ -> ()//printfn "!%A" uri
            }
        agent(uri),e'
    
        *)