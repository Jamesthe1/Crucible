module StringHelper

open System

let arrToString: string[] -> string =
    String.concat ", "

let strToArr str =
    Array.ofSeq str

let strFromArr (arr: char[]) =
    String arr

let removeAny str forbiddenChars =
    String.filter (fun c -> String.exists (fun fc -> c = fc) forbiddenChars
                            <> true
                  ) str

let tryParseBool (str: string) =
    match Boolean.TryParse str with
    | (true, b)  -> Some b
    | _ -> None

let combineStringValues arr1 (arr2: string[]) (seperator: char) =
    Array.mapi (fun i s -> String.Join(seperator, s, arr2.[i])) arr1