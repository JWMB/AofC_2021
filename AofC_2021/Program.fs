﻿open System
open System.Text.RegularExpressions
open System.IO
open Tools

let getDayPartMethods (type_: Type) =
    type_.GetMethods() |> Seq.filter (fun m -> m.Name.StartsWith("part")) |> Seq.toArray

let findFileInParents leaf fGetFile =
    let rec loop (dir: DirectoryInfo) =
        let (foundFile: 'a option) = fGetFile dir
        if foundFile.IsNone then 
            if dir.Parent = null then None
            else loop dir.Parent
        else foundFile
    loop leaf

let findFileInParents2 filename =
    let currDir = new DirectoryInfo(Directory.GetCurrentDirectory())
    findFileInParents currDir (fun dir -> dir.GetFiles(filename) |> Array.tryHead)

let getTypeFilePath (type_: Type) extension =
    findFileInParents2 $"{type_.Name}.{extension}"

let getFileContent (type_: Type) extension = match getTypeFilePath type_ extension with
                                                | Some fi -> File.ReadAllText(fi.FullName)
                                                | None -> ""

let getDayTypes =
    let assembly = AppDomain.CurrentDomain.GetAssemblies() |> Array.find (fun a -> a.GetName().Name.StartsWith("AofC"))
    let found = assembly.ExportedTypes 
                |> Seq.map (fun t -> (t, Regex.Match(t.Name, @"^D(\d+)$"))) 
                |> Seq.filter (fun f -> (snd f).Success)
                |> Seq.map (fun f -> (int (snd f).Groups.[1].Value, fst f))
    Map(found |> Seq.toArray)

let generateReadMe (type_: Type) (aofcInfo: Tools.AofCSiteInfo.DayInfo) (basePath: DirectoryInfo) =
    let relativePath filepath = $"{basePath.Name}/{filepath}"
    
    let getSnippet (method: Reflection.MemberInfo) = 
        let fsContent = getFileContent method.DeclaringType "fs"
        let m = Regex.Match(fsContent + "\nlet ", $@"(?<=\r|\n)let {method.Name}.+?(?=(\r|\n)let\s)", RegexOptions.Singleline)
        if m.Success then m.Value.Trim()
        else ""
    let methodInfo input (method: Reflection.MethodInfo) = 
        let methodResult = method.Invoke(null, [|input|]).ToString()
        let methodResultString = if methodResult.Contains("\n") then $"\n```\n{methodResult}\n```" else $"`{methodResult}`"
        $"### {method.Name}\n```\n{getSnippet method}\n```\nResult: {methodResultString}"

    let input = getFileContent type_ "txt"
    $"""## [Day {aofcInfo.Day} - {aofcInfo.Title}]({aofcInfo.Url})
[Source]({relativePath type_.Name + ".fs"})  
[Input]({match getTypeFilePath type_ "txt" with | Some f -> relativePath f.Name | None -> ""})  
{getDayPartMethods type_ |> Array.map (fun f-> methodInfo input f) |> String.concat "\n"}
"""

let writeReadme readmeFilename =
    let readmeFile = findFileInParents2 readmeFilename
    if readmeFile.IsSome then
        let readmeContent = File.ReadAllText(readmeFile.Value.FullName)
        let startMatch = Regex.Match(readmeContent, "##Autogenerated##")
        if startMatch.Success then
            let dayTypes = getDayTypes
            let infos = dayTypes.Keys |> Seq.toArray |> Array.map (fun day -> AofCSiteInfo.main 2021 day) |> Async.Parallel |> Async.RunSynchronously
            let infoByDay = Map(infos |> Array.map(fun f -> (f.Day, f)))

            let dir = (findFileInParents2 "Program.fs").Value.Directory
            let autogenerated = dayTypes |> Map.toArray 
                                         |> Array.map (fun (day, type_) ->
                                                let info = infoByDay.[day]
                                                generateReadMe type_ info dir
                                            ) |> String.concat ""

            let newContent = readmeContent.Substring(0, (startMatch.Index + startMatch.Length)) + "\n" + autogenerated
            File.WriteAllText(readmeFile.Value.FullName, newContent)

[<EntryPoint>]
let main argv =
    let day = 13

    writeReadme "README.md"

    let dayType = getDayTypes.[day]
    let input = match getTypeFilePath dayType "txt" with
                | Some fi -> File.ReadAllText(fi.FullName)
                | None -> ""

    let methods = getDayPartMethods dayType
    let results = methods |> Seq.map (fun f -> $"{f.Name}: {f.Invoke(null, [|input|])}") |> Seq.toArray

    printf "Result:\n%A\n" (results |> String.concat "\n")
    0
