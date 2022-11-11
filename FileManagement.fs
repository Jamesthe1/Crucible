module FileManagement

open System
open System.IO
open System.IO.Compression
open System.Linq
open FSharp.Data
open Utils


let readZip filepath = ZipFile.OpenRead filepath
let readFile filepath = File.ReadAllLines filepath
let getPwd = Directory.GetCurrentDirectory
let replaceEnvs str = Environment.ExpandEnvironmentVariables str
let getFilename (filepath: string) = Path.GetFileName filepath
let deleteFile filepath = File.Delete filepath

let listFiles rawDir =
    (replaceEnvs rawDir
     |> Directory.EnumerateFiles).ToArray ()

let listDirs rawDir =
    (replaceEnvs rawDir
     |> Directory.EnumerateDirectories).ToArray ()
    |> Array.map (fun s -> DirectoryInfo(s).Name)

let moveFile filepath toDir =
    let filename = getFilename filepath
    File.Move(filepath, toDir + "/" + filename)

let moveFiles fromDir toDir =
    listFiles fromDir
    |> Array.iter (fun f -> replaceEnvs toDir
                            |> moveFile f)

let readFileKV filepath =
    readFile filepath
    |> Array.map (fun s -> splitToTuple s '=')
    |> Map.ofArray

let getSetting setting =
    readFileKV "settings.txt"
    |> Map.find setting

let readModArchive (filepath: string) (archive: ZipArchive) =
    try
        // Stream is automatically cleaned up by reader.Close()
        let stream = (archive.GetEntry "mcmod.info").Open()
        let reader = new StreamReader(stream)
        let result = Success (reader.ReadToEnd())
        reader.Close()
        result
    with
        | :? NullReferenceException -> Failure filepath

let readModMetadata filepath = using (readZip filepath) (readModArchive filepath)

let formatMetadata rawData filepath =
    let filename = getFilename filepath
    let root = JsonValue.Parse rawData
    match root with
    | JsonValue.Array infoArr -> extractModInfo infoArr.[0] filename
    | _ -> let infoArr = jsonPropertyArr root "modList"
           extractModInfo infoArr.[0] filename

let fetchMetadata filepath =
    match readModMetadata filepath with
    | Success s -> formatMetadata s filepath
    | Failure f -> { ModMetadata.empty with Filename = getFilename f }

let readAllMetadata filepaths =
    filepaths
    |> Array.map fetchMetadata

let filterModsRegex regex arr =
    Array.filter (fun (m: ModMetadata) -> regexMatches m.DisplayName regex) arr

let listAllMods enabled rawDir =
    listFiles rawDir
    |> Array.filter (fun (f: string) -> Path.GetExtension f = ".jar")
    |> readAllMetadata
    |> Array.map (fun m -> { m with Enabled = enabled })

let listAllModsRegex enabled rawDir regex =
    listAllMods enabled rawDir
    |> filterModsRegex regex

let printMod (m: ModMetadata) = printfn "%A" m
let printAllMods mods = Array.iter (fun m -> printMod m) mods

let confirmDelete list stringifyFunc deleteFunc =
    printf "Are you sure you want to delete the following? "
    Array.map stringifyFunc list
    |> printStrArray
    printf "They will be deleted forever!\ny/N> "
    let confirm = Console.ReadKey()
    printNewline()

    match confirm.Key with
    | ConsoleKey.Y -> Array.iter deleteFunc list
    | _ -> ()