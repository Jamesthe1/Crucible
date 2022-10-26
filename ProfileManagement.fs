module ProfileManagement

open System
open System.IO
open FSharp.NativeInterop
open FCMD.Command
open FileManagement
open Utils

let toProfilePath profile = "./profiles/" + profile + "/mods"

// Inref must be declared again because the variable coming in gets de-referenced
let getProfileMods profile =
    toProfilePath profile
    |> fun root -> root + "/" + getSetting "disabledFolder"
                   |> listAllMods false
                   |> Array.append (listAllMods true root)

let getProfileModByName profile name =
    try
        getProfileMods profile
        |> Array.find (fun m -> m.DisplayName = name)
    with
    | :? Exception -> failwith $"Mod {name} not found"

let getProfileModsRegex profile regex =
    getProfileMods profile
    |> Array.filter (fun m -> regexMatches m.DisplayName regex)
                    
let setModEnableStatus profile regex enabled =
    let profilePath = toProfilePath profile
    let disabledPath = profilePath + "/" + getSetting "disabledFolder"

    getProfileModsRegex profile regex
    |> Array.iter (fun m -> let fnameInDir = "/" + m.Filename
                            let disabledFile = (disabledPath + fnameInDir)
                            let enabledFile = (profilePath + fnameInDir)
                            try
                                if enabled then
                                    moveFile disabledFile profilePath
                                else
                                    moveFile enabledFile disabledPath
                            with
                            | :? IOException as e -> if (File.Exists disabledFile && enabled = false)
                                                         || (File.Exists enabledFile && enabled) then
                                                         printfn "Error: Mod \"%s\" already in status" m.DisplayName
                                                     else
                                                         printfn "Error: %s" e.Message
                  )

let deleteModsInProfile profile regex =
    let profilePath = toProfilePath profile
    let disabledPath = profilePath + "/" + getSetting "disabledFolder"
    getProfileModsRegex profile regex
    |> Array.iter (fun m -> let fnameInDir = "/" + m.Filename
                            if m.Enabled then profilePath + fnameInDir else disabledPath + fnameInDir
                            |> deleteFile
                  )

let listProfiles () =
    listDirs "./profiles"

let listProfilesRegex regex =
    listProfiles ()
    |> Array.filter (fun s -> regexMatches s regex)

let private getProfileModsRegexes profile args =
    Array.map (fun arg -> getProfileModsRegex profile arg) args
    |> Array.concat

let getModCommands ptrTuple: FuncCommands =
    FuncCommands [ ("list", { Function = fun aList -> let profile = strFromPtr ptrTuple
                                                      if aList.Length = 0 then
                                                          getProfileMods profile
                                                      else
                                                          getProfileModsRegexes profile aList
                                                      |> printAllMods
                              Description = "Lists all mods in the current profile\n=\t<args>: Gives a filtered mod list, uses regex"
                            }
                   )
                   ("disable", { Function = fun aList -> failOnNoArg aList
                                                         let profile = strFromPtr ptrTuple
                                                         for arg in aList do
                                                             setModEnableStatus profile arg false
                                 Description = "Disables mods by names, uses regex"
                               }
                   )
                   ("enable",  { Function = fun aList -> failOnNoArg aList
                                                         let profile = strFromPtr ptrTuple
                                                         for arg in aList do
                                                             setModEnableStatus profile arg true
                                 Description = "Enables mods by names, uses regex"
                               }
                   )
                   ("delete", { Function = fun aList -> failOnNoArg aList
                                                        let profile = strFromPtr ptrTuple
                                                        printf "Are you sure you want to delete the following? "
                                                        getProfileModsRegexes profile aList
                                                        |> Array.map (fun m -> m.DisplayName)
                                                        |> printStrArray
                                                        printf "They will be deleted forever!\ny/N> "
                                                        let confirm = Console.ReadKey()
                                                        printfn ""

                                                        match confirm.Key with
                                                        | ConsoleKey.Y -> for arg in aList do
                                                                              deleteModsInProfile profile arg
                                                        | _ -> ()
                                Description = "Deletes mods by names, uses regex"
                              }
                   )
                   ("detail", { Function = fun aList -> failOnNoArg aList
                                                        let profile = strFromPtr ptrTuple
                                                        let arg = aList.[0]
                                                        getProfileModByName profile arg
                                                        |> fun m -> m.FullInfo (getProfileMods profile)
                                                        |> printfn "%s"
                                Description = "Gets the details of a mod by name. Requires full name, only uses the first argument"
                              }
                   )
                 ]

let getProfileCommands (ptr, len): FuncCommands =
    FuncCommands [ ("list", { Function = fun aList -> if aList.Length = 0 then
                                                          listProfiles()
                                                      else
                                                          Array.map (fun (arg: string) -> listProfilesRegex arg) aList
                                                          |> Array.concat
                                                      |> printStrArray
                              Description = "Lists all profiles\n=\t<args>: Gives a filtered profile list, uses regex"
                            }
                   )
                   ("use", { Function = fun aList -> failOnNoArg aList
                                                     let arg = aList.[0]
                                                     let foundArg = listProfiles()
                                                                    |> Array.contains arg
                                                     if foundArg then
                                                         String.iteri (fun i c -> NativePtr.set ptr i c) arg
                                                         for i=arg.Length to (len-1) do
                                                            NativePtr.set ptr i '\x00'
                                                     else
                                                         failwith $"Profile {arg} not found"
                             Description = "Switches to an existing profile, only uses the first argument"
                           }
                   )
                   ("add", { Function = fun aList -> failOnNoArg aList
                                                     for arg in aList do
                                                        try
                                                            Directory.CreateDirectory ("./profiles/" + arg)
                                                            |> ignore
                                                        with
                                                        | :? Exception as e -> failwith ("Could not create profile: " + e.Message)
                             Description = "Creates new profiles by names"
                           }
                   )
                   #if DEBUG
                   ("pcp", { Function = fun _ -> strFromPtr (ptr, len)
                                                 |> printfn "%s"
                             Description = "Prints the current profile"
                           }
                   )
                   #endif
                 ]