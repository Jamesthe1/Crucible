module Utils

open System
open System.Collections.Generic
open System.Text.RegularExpressions
open FSharp.Data
open FSharp.NativeInterop

open Tomlyn.Model

open StringHelper
open ConsoleUtils
open UtilTypes
open TomlynWrapper

/// <summary>
/// A function that returns a given value. Used primarily in if/else statements with functions that return the same type, in case you want to do nothing with the result
/// </summary>
/// <param name="a">The value to pass through</param>
let passthrough a =
    a

let writeToPtr ptr data =
    Array.iteri (fun i d -> NativePtr.set ptr i d) data

let strIntoPtr (str: string) len (ptr: UmgdString) =
    if len < str.Length then
        raise (ArgumentException "Allocated length is smaller than string length, allocation not done")
    str.ToCharArray()
    |> writeToPtr ptr
    (ptr, len)

let writeStrToPtr (ptr: UmgdString, len) (str: string) =
    Array.zeroCreate<char> len
    |> writeToPtr ptr
    str.ToCharArray()
    |> writeToPtr ptr

let strFromPtr (ptr: UmgdString, len) =
    Array.zeroCreate<char> len
    |> Array.mapi (fun i _ -> NativePtr.get ptr i)
    |> Array.filter (fun c -> c <> '\x00')
    |> strFromArr

/// <summary>Prints the result value as a success or failure</summary>
let printResult result =
    match result with
    | Success a -> printfn "Success: %s" a
    | Failure a -> printError a

let succeedOnEmpty (successMsg: string) (str: string) =
    if str.Equals ""
    then Success successMsg
    else Failure str

let exceptionOnFailure result =
    match result with
    | Success _ -> ()
    | Failure e -> failwith e

let someOrException failMsg opt =
    match opt with
    | Some v -> v
    | None -> failwith failMsg

let jsonProperty (root: JsonValue) property =
    root.GetProperty property

let jsonPropertyMap (root: JsonValue) =
    root.Properties()
    |> Map

let jsonPropertyOfMap map =
    Map.toArray map
    |> JsonValue.Record

let jsonSetProperty root property newData =
    jsonPropertyMap root
    |> Map.change property (fun vO -> match vO with
                                      | Some _ -> Some newData
                                      | None -> None
                           )
    |> jsonPropertyOfMap

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

let extractModInfoTOML (mdl: TomlTable) filename =
    let info = mdl.["mods"] :?> TomlTableArray
               |> fun mods -> mods.[0]
    let id = getTOMLValue info "modId"
               
    let dependencies = try
                           mdl.["dependencies"] :?> TomlTable
                           |> fun t -> t.[id] :?> TomlTableArray
                           |> Array.ofSeq
                       with
                       | :? KeyNotFoundException -> Array.empty

    let getDepsValues key =
        Array.map (fun d -> getTOMLValue d key)

    let mandatoryDeps = Array.filter (fun d -> getTOMLValue d "mandatory"
                                               |> tryParseBool
                                               |> someOrException "Malformed boolean/boolean not present"
                                     ) dependencies


    let mandatoryVersions =
        let ids = getDepsValues "modId" mandatoryDeps
        let ranges = getDepsValues "versionRange" mandatoryDeps
        combineStringValues ids ranges ':'
        |> arrToString

    { Filename = filename
      ModId = id
      ModName = getTOMLValue info "displayName"
      Description = getTOMLValue info "description"
      Version = getTOMLValue info "version"
      GameVersion = mandatoryVersions
      Logo = getTOMLValue info "logoFile"
      URL = getTOMLValue info "displayURL"
      UpdateURL = getTOMLValue info "updateJSONURL"
      Authors = getTOMLValue info "authors"
                |> Array.create 1
      Credits = getTOMLValue info "credits"
      Parent = ""
      Screenshots = Array.empty
      Dependencies = getDepsValues "modId" dependencies
      Enabled = true
    }

let getModByName name: ModMetadata[] -> ModMetadata =
    try
        Array.find (fun m -> m.DisplayName = name)
    with
    | :? Exception -> failwith $"Mod {name} not found"

let splitToTuple (str: String) (c: char) =
    let arr = str.Split c
    (arr.[0], arr.[1])

let regexMatches str expression =
    Regex.IsMatch(str, expression, RegexOptions.Compiled ||| RegexOptions.IgnoreCase)

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

let getBest comparison =
    Array.fold (fun s i -> if comparison s i then s else i) 0

let getLongestStrLen strArr =
    Array.map (fun (s: string) -> s.Length) strArr
    |> getBest (>)

let printNewline () =
    printfn ""

let expandArray func arr =
    Array.collect func arr

let useArrIfNotEmpty zeroFunc arrFunc arr =
    if Array.length arr = 0 then
        zeroFunc()
    else
        arrFunc arr