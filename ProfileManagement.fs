module ProfileManagement

open System
open System.IO
open FSharp.NativeInterop
open FCMD.Command
open FileManagement
open Utils

let toProfilePath profile = "./profiles/" + profile
let toProfileModPath profile = toProfilePath profile + "/mods"

let getProfileMods profile =
    toProfileModPath profile
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
    |> filterModsRegex regex

let private getProfileModsRegexes profile args =
    expandArray (getProfileModsRegex profile) args
                    
let setModEnableStatus profile regex enabled =
    let profilePath = toProfileModPath profile
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

let deleteModInProfile profile modData =
    let modsPath = toProfileModPath profile
    let disabledPath = modsPath + "/" + getSetting "disabledFolder"
    let fnameInDir = "/" + modData.Filename
    if modData.Enabled then
        modsPath
    else
        disabledPath
    + fnameInDir
    |> deleteFile

let addProfile name =
    try
        Directory.CreateDirectory("./profiles/" + name)
        |> fun d -> d.CreateSubdirectory "mods"
        |> fun d -> d.CreateSubdirectory "disabled"
        |> ignore
    with
    | :? Exception as e -> failwith("Could not create profile: " + e.Message)

let deleteProfile name =
    Directory.Delete("./profiles/" + name)

let listProfiles () =
    listDirs "./profiles"

let listProfilesRegex regex =
    listProfiles()
    |> Array.filter(fun s -> regexMatches s regex)

let private listProfilesRegexes args =
    expandArray listProfilesRegex args

let getModCommands ptrTuple: FuncCommands =
    let setRegexModEnable status aList =
        failOnNoArg aList
        let profile = strFromPtr ptrTuple
        for arg in aList do
            setModEnableStatus profile arg status

    let mcDir = getSetting "minecraftDirectory"
    let modsPath = mcDir + "/mods"

    let listMCMods () = listAllMods true modsPath
    let listMCModsRegex regex = listMCMods()
                                |> filterModsRegex regex

    FuncCommands [ ("list", { Function = fun aList -> let profile = strFromPtr ptrTuple
                                                      if aList.Length = 0 then
                                                          getProfileMods profile
                                                      else
                                                          getProfileModsRegexes profile aList
                                                      |> printAllMods
                              Description = "Lists all mods in the current profile\n--\t\t<args>: Gives a filtered mod list, uses regex"
                            }
                   )
                   ("listApplied", { Function = fun aList -> if aList.Length = 0 then
                                                                 listMCMods()
                                                             else
                                                                 expandArray listMCModsRegex aList
                                                             |> printAllMods
                                     Description = "Lists all mods currently applied\n--\t\t<args>: Gives a filtered mod list, uses regex"
                                   }
                   )
                   ("disable", { Function = setRegexModEnable false
                                 Description = "Disables mods by names, uses regex"
                               }
                   )
                   ("enable",  { Function = setRegexModEnable true
                                 Description = "Enables mods by names, uses regex"
                               }
                   )
                   ("delete", { Function = fun aList -> failOnNoArg aList
                                                        let profile = strFromPtr ptrTuple
                                                        let mods = getProfileModsRegexes profile aList
                                                        confirmDelete mods (fun m -> m.DisplayName) (deleteModInProfile profile)
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
                   ("apply", { Function = fun _ -> let profile = strFromPtr ptrTuple
                                                   moveFiles (toProfileModPath profile) modsPath
                               Description = "Moves current profile's mods to the game's folder"
                             }
                   )
                   ("fetch", { Function = fun _ -> strFromPtr ptrTuple
                                                   |> toProfileModPath
                                                   |> moveFiles modsPath
                               Description = "Moves mods already in the game directory to the current profile"
                             }
                   )
                 ]

let getProfileCommands (ptr: UmgdString, len): FuncCommands =
    let mcDir = getSetting "minecraftDirectory"
    let modsPath = mcDir + "/mods"

    let getModCommand ptrTuple cmd =
        getModCommands ptrTuple
        |> fun cmds -> cmds.[cmd]

    FuncCommands [ ("list", { Function = fun aList -> if aList.Length = 0 then
                                                          listProfiles()
                                                      else
                                                          Array.map (fun (arg: string) -> listProfilesRegex arg) aList
                                                          |> Array.concat
                                                      |> printStrArray
                              Description = "Lists all profiles\n--\t\t<args>: Gives a filtered profile list, uses regex"
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
                                                        addProfile arg
                             Description = "Creates new profiles by names"
                           }
                   )
                   ("delete", { Function = fun aList -> failOnNoArg aList
                                                        let profiles = listProfilesRegexes aList
                                                        confirmDelete profiles (fun f -> f) deleteProfile
                                Description = "Deletes profiles by names, uses regex"
                              }
                   )
                   ("apply", { Function = fun aList -> let profile = if aList.Length = 0 then
                                                                         strFromPtr (ptr, len)
                                                                     else
                                                                         aList.[0]
                                                       moveFiles (toProfileModPath profile) modsPath
                               Description = getModCommand (ptr, len) "apply"
                                             |> fun fdef -> $"{fdef.Description}\n--\t\t<arg>: Uses the given profile, requires full name"
                             }
                   )
                   ("fetch", { Function = fun aList -> let profile = if aList.Length = 0 then
                                                                         strFromPtr (ptr, len)
                                                                     else
                                                                         aList.[0]
                                                       toProfileModPath profile
                                                       |> moveFiles modsPath
                               Description = getModCommand (ptr, len) "fetch"
                                             |> fun fdef -> $"{fdef.Description}\n--\t\t<arg>: Moves mods to the given profile, requires full name"
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