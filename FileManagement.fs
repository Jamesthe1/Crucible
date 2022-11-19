module FileManagement

open System
open System.IO
open System.IO.Compression
open System.Linq
open FSharp.Data

open ConsoleUtils
open UtilTypes
open Utils
open TomlynWrapper

let readZip = ZipFile.OpenRead
let readFile = File.ReadAllLines
let getPwd = Directory.GetCurrentDirectory
let replaceEnvs = Environment.ExpandEnvironmentVariables
let getFilename (filepath: string) = Path.GetFileName filepath
let getFilenameNoExt (filepath: string) = Path.GetFileNameWithoutExtension filepath
let deleteFile = File.Delete
let fileExists = File.Exists
let dirExists = Directory.Exists
let createDir = Directory.CreateDirectory

let extractZip fromZip toDir =
    ZipFile.ExtractToDirectory(fromZip, toDir)
let zipFolder fromDir toZip =
    ZipFile.CreateFromDirectory(fromDir, toZip)

let listFiles rawDir =
    replaceEnvs rawDir
    |> Directory.EnumerateFiles
    |> Array.ofSeq

let listFilesWithExt rawDir ext =
    listFiles rawDir
    |> Array.filter (fun f -> Path.GetExtension f = ext)

let listJars rawDir =
    listFilesWithExt rawDir ".jar"

let listDirs rawDir =
    replaceEnvs rawDir
    |> Directory.EnumerateDirectories
    |> Array.ofSeq
    |> Array.map (fun s -> DirectoryInfo(s).Name)

let isFilepath path =
    match File.GetAttributes path with
    | a when a &&& FileAttributes.Directory = FileAttributes.Directory -> false
    | _ -> true

let moveFile filepath toDir =
    let filename = getFilename filepath
    File.Move(filepath, replaceEnvs toDir + "/" + filename)

let moveFileArr toDir =
    Array.iter (fun f -> moveFile f toDir)

let moveFiles fromDir toDir =
    listFiles fromDir
    |> moveFileArr toDir

let moveJarFiles fromDir toDir =
    listJars fromDir
    |> moveFileArr toDir

let moveAllInDir (fromDir: string) toDir =
    let dest = toDir + "/" + getFilename fromDir
    if dirExists dest |> not then
        createDir dest |> ignore

    listDirs fromDir
    |> Array.map (fun s -> fromDir + "/" + s)
    |> Array.iter (fun d -> Directory.Move(d, dest + "/" + getFilename d))
    listFiles fromDir
    |> moveFileArr dest

let moveFilesAndDirs toDir arr =
    let fullToDir = replaceEnvs toDir
    Array.iter (fun p -> let fullPath = replaceEnvs p
                         if fileExists fullPath || dirExists fullPath then
                             if isFilepath fullPath then
                                 moveFile fullPath fullToDir
                             else
                                 moveAllInDir fullPath fullToDir
                         else
                             printWarning $"{fullPath} does not exist, skipping"
               ) arr

let findEntry entries filename =
    try
        Array.find (fun (e: ZipArchiveEntry) -> e.Name = filename) entries
        |> Some
    with
    | :? Exception -> None

let readEntry (entry: ZipArchiveEntry) =
    // Stream is automatically cleaned up by reader.Close()
    let stream = entry.Open()
    let reader = new StreamReader(stream)
    let result = reader.ReadToEnd()
    reader.Close()
    result

let readFileKV filepath =
    readFile filepath
    |> Array.map (fun s -> splitToTuple s '=')
    |> Map.ofArray

let getSetting setting =
    readFileKV "settings.txt"
    |> Map.find setting

let readJsonFile path =
    using (File.Open(path, FileMode.Open)) JsonValue.Load

let readModArchive (filepath: string) (archive: ZipArchive) =
    let entries = archive.Entries.ToArray()
    let rootOpt = match findEntry entries "mcmod.info" with
                  | Some e -> Some (e, true)
                  | None -> match findEntry entries "mods.toml" with
                            | Some e -> Some (e, false)
                            | None -> None
    match rootOpt with
    | None -> Failure filepath
    | Some (entry, isJson) -> Success (readEntry entry, isJson)
    
    
/// <summary>
/// Gets metadata from a given mod
/// </summary>
/// <param name="filepath">The full path to a given mod</param>
/// <returns>
/// <p>Success: data and isJson</p>
/// <p>Failure: The filepath</p>
/// </returns>
let readModMetadata filepath = using (readZip filepath) (readModArchive filepath)

let tryFormatMetadata (rawData, isJson) filepath =
    let filename = getFilename filepath
    if isJson then
        let rootOpt = JsonValue.TryParse rawData
        match rootOpt with
        | None -> None
        | Some root -> match root with
                       | JsonValue.Array infoArr -> extractModInfo infoArr.[0] filename
                                                    |> Some
                       | _ -> let infoArr = jsonPropertyArr root "modList"
                              extractModInfo infoArr.[0] filename
                              |> Some
    else
        let rootRslt = tryParseTOML rawData
        match rootRslt with
        | Failure _ -> None
        | Success mdl -> extractModInfoTOML mdl filename
                         |> Some

let fetchMetadata filepath =
    let blank = { ModMetadata.empty with Filename = getFilename filepath }
    match readModMetadata filepath with
    | Success s -> match tryFormatMetadata s filepath with
                   | Some m -> m
                   | None -> blank
    | Failure _ -> blank

let readAllMetadata filepaths =
    filepaths
    |> Array.map fetchMetadata

let filterModsRegex regex arr =
    Array.filter (fun (m: ModMetadata) -> regexMatches m.DisplayName regex) arr

let listAllMods enabled rawDir =
    listJars rawDir
    |> readAllMetadata
    |> Array.map (fun m -> { m with Enabled = enabled })

let listAllModsRegex enabled rawDir regex =
    listAllMods enabled rawDir
    |> filterModsRegex regex

let printMod (m: ModMetadata) = printfn "%A" m
let printAllMods mods = Array.iter (fun m -> printMod m) mods

let printAllModNames mods = Array.iter (fun (m: ModMetadata) -> printfn "%s" m.DisplayName) mods

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