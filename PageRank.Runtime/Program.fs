// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.
open PageRank
open PageRank.Agents
let side f a = f a;a
let say s a = printfn "%s" s; a
let clearline() = 
    System.Console.CursorLeft <- 0
    String.replicate System.Console.BufferWidth " " |> printf "%s"
    System.Console.CursorLeft <- 0
    System.Console.CursorTop <- System.Console.CursorTop-1
[<EntryPoint>]
let main argv = 
    let pages = System.Collections.Generic.Dictionary()
    let currentDepth = ref 200L
    let start = !currentDepth
    let stop = new System.Threading.CancellationTokenSource()
    let d = System.Collections.Generic.HashSet<_>()
    let crawler = IPC.existing_crawler(6996) //gen_crawler()
    //Async.Start(async{while true do snd(crawler)()|>ignore})
    System.Threading.Thread.Sleep 100
    Async.CancelDefaultToken()
    printfn "Ready"
    let start_page = System.Console.ReadLine()
    let a,e = crawler_getinf(start_page,crawler,stop.Token,d) //HTMLagent("http://wiki.osdev.org/",stop.Token)
    let v = e.Add(fun (i,j,k) -> pages.Add(i,(j,k)); System.Threading.Interlocked.Decrement(currentDepth)|>ignore)
    printfn "Go!"
    Async.Start(a,stop.Token)
    async{
        let last = ref !currentDepth
        while !currentDepth > 0L do 
            while !currentDepth = !last do ()
            last := !currentDepth
            System.Threading.Thread.Sleep 200
            clearline()
            let depth = start - !currentDepth
            printf "%f percent complete [%i of %i]" (float depth * 100. / float start) depth start
    }|>Async.Start
    while !currentDepth > 0L do ()
    clearline()
    let depth = start
    printfn "%f percent complete [%i of %i]" (float depth * 100. / float start) depth start
    stop.Cancel()
    Async.CancelDefaultToken()
    printfn "Got pages!"
    let v = 
        PageRank(pages|>Seq.map (fun i -> i.Key,i.Value)|>Map.ofSeq,5) |> say "Ranked"
        |> Map.toArray |> say "Arrayed"
        |> Array.Parallel.map (fun (i,(j,k)) -> i,j,System.Web.HttpUtility.HtmlDecode(k))
        |> quicksort (fun(i,j,k) -> j) |> say "Sorted"
        |> Array.map (fun(i,j,k) -> i,k.ToLower()) |> say "Done!"
    while true do
        printf "Query [CaseI]: "
        let r = System.Console.ReadLine()
        get_n (fun (uri,s:string) -> s.Contains(r.ToLower())) 10 v |> Seq.iter (fun (uri,_) -> printfn "\t%s" uri)
        //r.IsMatch
    0
