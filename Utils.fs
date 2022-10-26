module Utils

open System
open System.Text.RegularExpressions
open FSharp.Data
open FSharp.NativeInterop

let arrToString =
    String.concat ", "
let strToArr str =
    Array.ofSeq str
let strFromArr (arr: char[]) =
    String arr

[<StructuredFormatDisplay("{AsString}")>]
type ModMetadata =
    { Filename: string
      ModId: string
      ModName: string
      Description: string
      Version: string
      GameVersion: string
      Logo: string
      URL: string
      UpdateURL: string
      Authors: string[]
      Credits: string
      Parent: string
      Screenshots: string[]
      Dependencies: string[]
      Enabled: bool }
with
    static member empty = { Filename = ""
                            ModId = ""
                            ModName = ""
                            Description = ""
                            Version = ""
                            GameVersion = ""
                            Logo = ""
                            URL = ""
                            UpdateURL = ""
                            Authors = Array.empty
                            Credits = ""
                            Parent = ""
                            Screenshots = Array.empty
                            Dependencies = Array.empty
                            Enabled = true }
    member private this.EnabledAsText = if this.Enabled then "ENABLED "
                                        else "DISABLED"
    member this.DisplayName = match this.ModName with
                              | "" -> this.Filename
                              | name -> name
    override this.ToString() = this.EnabledAsText + " : " + this.DisplayName
    member this.AsString = this.ToString()

    member private this.RuntimeFix (arg: string) =
        if arg.StartsWith "${" then
            "Determined at runtime"
        else
            arg

    // Members are allowed to be recursive by default
    member private this.FindDependencyNames (modList: ModMetadata[]) start =
        if this.Dependencies.Length = 0 then
            "None"
        else
            let getI = let dependency = this.Dependencies.[start]
                       try
                           modList
                           |> Array.find (fun m -> m.ModId = dependency)
                           |> fun m -> m.ModName
                       with
                       | :? Exception -> dependency
            let iNext = start + 1
            if iNext = this.Dependencies.Length then
                getI
            else
                arrToString [| getI; this.FindDependencyNames modList iNext |]

    member private this.InfoLayout (args: string[]) =
        let separator = "\n\t"
        this.DisplayName + separator + String.Join (separator, args) + "\n"
    member private this.NoInfo = this.InfoLayout [| "No information available, mcmod.info not found" |]
    member private this.AllInfo modList = this.InfoLayout [| $"Filename: {this.Filename}"
                                                             $"ID: {this.ModId}"
                                                             $"Description: {this.Description}"
                                                             $"Version: {this.RuntimeFix this.Version}"
                                                             $"Minecraft Version: {this.RuntimeFix this.GameVersion}"
                                                             $"Authors: {arrToString this.Authors}"
                                                             $"Credits: {this.Credits}"
                                                             $"Dependencies: {this.FindDependencyNames modList 0}"
                                                          |]
    member this.FullInfo modList = if this.ModName.Equals "" then
                                       this.NoInfo
                                   else
                                       this.AllInfo modList

type Result<'TSuccess, 'TFailure> =
    | Success of 'TSuccess
    | Failure of 'TFailure

/// <summary>
/// Unmanaged string type for pointing
/// </summary>
type UmgdString = nativeptr<char>

let strIntoPtr (str: string) len (ptr: UmgdString) =
    if len < str.Length then
        raise (ArgumentException("Allocated length is smaller than string length, allocation not done"))
    String.iteri (fun i c -> NativePtr.set ptr i c) str
    (ptr, len)
let strFromPtr (ptr: UmgdString, len) =
    Array.zeroCreate<char> len
    |> Array.mapi (fun i _ -> NativePtr.get ptr i)
    |> Array.filter (fun c -> c <> '\x00')
    |> strFromArr

/// <summary>Prints an error in red text</summary>
let printError text =
    Console.ForegroundColor <- ConsoleColor.Red
    printfn "FAILED: %s" text
    Console.ResetColor ()
/// <summary>Prints the result value as a success or failure</summary>
let printResult result =
    match result with
    | Success(a) -> printfn "Success: %s" a
    | Failure(a) -> printError a

let succeedOnEmpty (successMsg: string) (str: string) =
    if str.Equals ""
    then Success(successMsg)
    else Failure(str)

let exceptionOnFailure result =
    match result with
    | Success(_) -> ()
    | Failure(e) -> failwith e

let jsonProperty (root: JsonValue) property =
    root.GetProperty property
let jsonPropertyStr root property =
    try
        (jsonProperty root property).AsString ()
    with
    | :? Exception -> ""
let jsonPropertyArr root property =
    try
        (jsonProperty root property).AsArray ()
    with
    | :? Exception -> Array.empty
let jsonArrToStrArr (arr: JsonValue[]) =
    arr
    |> Array.map (fun j -> j.AsString ())
let jsonPropertyStrArr root property =
    jsonArrToStrArr (jsonPropertyArr root property)

let extractModInfo info filename =
    { Filename = filename
      ModId = jsonPropertyStr info "modid"
      ModName = jsonPropertyStr info "name"
      Description = jsonPropertyStr info "description"
      Version = jsonPropertyStr info "version"
      GameVersion = jsonPropertyStr info "mcversion"
      Logo = jsonPropertyStr info "logoFile"
      URL = jsonPropertyStr info "url"
      UpdateURL = jsonPropertyStr info "updateUrl"
      Authors = jsonPropertyStrArr info "authorList"
      Credits = jsonPropertyStr info "credits"
      Parent = jsonPropertyStr info "parent"
      Screenshots = jsonPropertyStrArr info "screenshots"
      Dependencies = jsonPropertyStrArr info "dependencies"
      Enabled = true }

let splitToTuple (str: String) (c: char) =
    let arr = str.Split c
    (arr.[0], arr.[1])

let orRegexOptions<'a> arr =
    Array.map (fun e -> int e) arr
    |> Array.fold (fun s a -> s ||| a) 0
    |> enum<RegexOptions>

let regexMatches str expression =
    let rx = Regex(expression, orRegexOptions [|RegexOptions.Compiled; RegexOptions.IgnoreCase|])
    rx.IsMatch str

let enumValToString<'a, 'b when ^b : enum<'a>> (enumVal: 'b) =
    enumVal.ToString()
let printStrArray arr =
    arrToString arr
    |> printfn "%s"

let enumToArray<'a> =
    Enum.GetValues typeof<'a> :?> ('a[])

let enumToString<'a> =
    enumToArray<'a>
    |> Array.map (fun e -> e.ToString())
    |> arrToString

let mergeMaps map1 map2 =
    Map.fold (fun s k v -> Map.add k v s) map1 map2

let getLongestStrLen strArr =
    let mutable result = 0
    Array.iter (fun (s: string) -> if s.Length > result then result <- s.Length) strArr
    result

let stringTrueFalse (str: string) =
    match Boolean.TryParse str with
    | (true, b)  -> Success(b)
    | (false, _) -> Failure()