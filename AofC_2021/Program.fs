open System

[<EntryPoint>]
let main argv =
    let day = 11
    
    let assembly = AppDomain.CurrentDomain.GetAssemblies() |> Array.find (fun a -> a.GetName().Name.StartsWith("AofC"))
    let dayType = assembly.ExportedTypes |> Seq.find (fun t -> t.Name = $"D{day}")
    let input = 
        let filename = $"D{day}.txt"
        if System.IO.File.Exists(filename) then System.IO.File.ReadAllText(filename)
        else ""

    let methods = dayType.GetMethods() |> Seq.filter (fun m -> m.Name.StartsWith("part"))
    let results = methods |> Seq.map (fun f -> $"{f.Name}: {f.Invoke(null, [|input|])}") |> Seq.toArray

    printf "Result:\n%A\n" (results |> String.concat "\n")
    0
