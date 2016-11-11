let read(s:System.IO.Stream) = 
    let rec inner (a:char list) = 
        let v = s.ReadByte()
        if v > 0 then inner (char v :: a) else a |> Seq.rev |> System.String.Concat
    inner []
let write(s:System.IO.Stream) (str:string) =
    let v = Array.append (System.Text.ASCIIEncoding.ASCII.GetBytes(str)) [|0uy|]
    s.Write(v,0,v.Length)
let extract (str:string) = 
    let v = str.IndexOf("\r\n\r\n")
    str.[v+4..]
let pipe1 = new System.IO.Pipes.NamedPipeServerStream("HAX")
pipe1.WaitForConnection()
let pipe2 = new System.IO.Pipes.NamedPipeServerStream("B")
pipe2.WaitForConnection()
write pipe1 "http://ss64.com/"
read pipe2 |> extract |> printfn "%A"

//---
let a = new System.IO.Pipes.NamedPipeServerStream("B")
a.WaitForConnection()
while true do a.ReadByte()
let v = new System.Collections.Hashtable()
v.
for i in v do i.
let wo i =
    let v' = (v.Clone():?>System.Collections.Hashtable)
    v'.Remove i
    v'