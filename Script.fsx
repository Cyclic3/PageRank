type Coupon = {id:string;length:float;guid:System.Guid}
type Token  = {id:string;L:float}
let side f a = f a;a
let e = 0.85
let rand = System.Random()
let randin (a:'a[]) = a.[rand.Next(a.Length-1)]
let pagerank(nodes:Map<string,string[]>,depth) =
    let n = float nodes.Count
    let l = n |> log |> sqrt
    //Phase 1
    let kept = Map.map (fun _ _ -> ResizeArray(n*depth|>int)) nodes
    let coupons =
        Map.map (fun i j -> 
            [|
                for k = 1 to depth*(log(n/e)|>( ** )<| 2.)|>int do
                    let g = System.Guid.NewGuid()
                    yield {id=i;length=l;guid=g};
            |]
        ) nodes
        |> ref
    for i = 1 to int l do
        Array.Parallel.map(fun (v,neighbors:string[]) ->
            let coupons = coupons.Value.[v]
            let kept = kept.[v]
            Array.Parallel.choose(fun coupon ->
                let r = rand.NextDouble()
                if neighbors.Length = 0 || r < e then 
                    lock kept (fun() -> kept.Add coupon) //Keep coupon
                    None //terminate coupon
                else 
                    {id=randin neighbors;length=coupon.length;guid=coupon.guid}
                    |> Some //pick some neighbor
            ) coupons
        ) <| Map.toArray nodes
        |> Array.concat
        |> Array.groupBy (function |{id=i} -> i) //Sort out the coupons into their owners
        |> Map.ofArray
        |> fun i -> coupons := i
    //Phase 2
    printfn "Phase 2"
    let f = e*(1.-e)
    let tokens = 
        Map.map (fun i j -> 
            [|
                for k = 1 to depth*(log(n/e)|>( ** )<| 2.)|>int do
                    let g = System.Guid.NewGuid()
                    let L = f**(rand.NextDouble()-1.)
                    yield {id=i;L=L};
            |]
        ) nodes
        |> ref
    let mem = Map.map (fun i j -> ResizeArray(depth*n|>int)) nodes
    for i = 1 to depth*(l/e)|>int do
        Array.Parallel.choose(fun (v,neighbors:string[]) ->
            let tokens = tokens.Value    //.[v]
            Option.map
            <|Array.Parallel.choose(fun (token:Token) ->
                if neighbors.Length = 0 || token.L < l then
                    //walk naively in parallel for another l steps
                    None
                else 
                    //(token,{owner=randin neighbors;L=token.L-l})
                    (randin neighbors,{id=v;length=token.L-l;guid=System.Guid.NewGuid()})
                    |> Some //pick some neighbor
                )
            <|tokens.TryFind v
        ) <| Map.toArray nodes
        |> Array.concat
        |> Array.groupBy fst //Sort out the coupons into their new owners
        |> Map.ofArray
        |> Map.map (fun id received -> let m = mem.[id] in Array.map(function|_,{id=v;length=L;guid=cid} -> m.Add(v,cid);{id=id;L=L}) received)
        |> fun i -> tokens := i
    //Phase 3
    printfn "Phase 3"
    let K = 0
    let counts = Map.map(fun _ _ -> ref 0) nodes
    let rec trace (id,couponid) =
        //iterate through showrt walks backward, and increment the counts atomicly
        System.Threading.Interlocked.Increment(counts.[id]) |> ignore
    Map.iter(fun u coupons -> coupons |> Array.ofSeq |> Array.Parallel.iter trace) mem
    //Return!
    Map.map (fun id count -> log(float(!count)/n)) counts//fix this plz!! (/(c n log n))
pagerank(Map(["a",[|"c"|];"b",[|"c"|];"c",[|"a"|];"d",[|"a"|]]),10000.)|>printfn "%A"