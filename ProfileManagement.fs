module ProfileManagement

open System
open System.IO
open System.IO.Compression
open FSharp.Data

open FCMD.Command
open FCMD.Types

open FileManagement
open Utils
open UtilTypes

let toProfilePath profile = "./profiles/" + profile
let toProfileModPath profile = toProfilePath profile + "/mods"

let getProfileMods profile () =
    toProfileModPath profile
    |> fun root -> root + "/disabled"
                   |> listAllMods false
                   |> Array.append (listAllMods true root)

let getProfileModByName profile name =
    getProfileMods profile ()
    |> getModByName name

let getProfileModsRegex profile regex =
    getProfileMods profile ()
    |> filterModsRegex regex

let private getProfileModsRegexes profile args =
    expandArray (getProfileModsRegex profile) args
                    
let setModEnableStatus profile regex enabled =
    let profilePath = toProfileModPath profile
    let disabledPath = profilePath + "/disabled"

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
    let disabledPath = modsPath + "/disabled"
    let fnameInDir = "/" + modData.Filename
    if modData.Enabled then
        modsPath
    else
        disabledPath
    + fnameInDir
    |> deleteFile

let private getMetaJsonPath (profile: string) =
    $"./profiles/{profile}/meta.json"

let getProfileMetaJson profile =
    getMetaJsonPath profile
    |> readJsonFile 

let private writeToMetaJson profile (data: JsonValue) =
    using (File.CreateText (getMetaJsonPath profile))
    <| fun fstream -> data.WriteTo(fstream, JsonSaveOptions.None)

let addProfile name =
    let profileDir = $"./profiles/{name}"
    let profileData = readJsonFile "./templates/profile_meta.json"
                      |> fun root -> JsonValue.String name
                                     |> jsonSetProperty root "name"
    try
        createDir profileDir
        |> fun d -> d.CreateSubdirectory "mods"
        |> fun d -> d.CreateSubdirectory "disabled"
        |> ignore
        writeToMetaJson name profileData
    with
    | :? Exception as e -> failwith $"Could not create profile: {e.Message}"

let getProfileMetaValues data =
    { Name = jsonPropertyStr data "name"
      Description = jsonPropertyStr data "desc"
      Movables = jsonPropertyStrArr data "movables"
    }

let getProfileMeta profile =
    getProfileMetaJson profile
    |> getProfileMetaValues

let setProfileProperty profile property newData =
    try
        getProfileMetaJson profile
        |> fun root -> jsonSetProperty root property newData
        |> writeToMetaJson profile
    with
    | :? Exception as e -> failwith $"Could not edit profile: {e.Message}"

let deleteProfile name =
    Directory.Delete($"./profiles/{name}", true)

let listProfiles () =
    listDirs "./profiles"

let listProfilesRegex regex =
    listProfiles()
    |> Array.filter(fun s -> regexMatches s regex)

let private listProfilesRegexes args =
    expandArray listProfilesRegex args

let archiveIsProfile (archive: ZipArchive) =
    let entries = archive.Entries |> Array.ofSeq
    match findEntry entries "meta.json" with
    | Some _ -> true
    | None -> false
    
let private getProfileMovables profile location =
    let profileMeta = getProfileMeta profile
    Array.map (fun s -> location + "/" + s) profileMeta.Movables

let private moveMovables profile fromDir toDir =
    getProfileMovables profile fromDir
    |> moveFilesAndDirs toDir

let fetchToProfile profile mcDir =
    printfn "Fetching data from Minecraft..."
    toProfileModPath profile
    |> moveJarFiles (mcDir + "/mods")
    toProfilePath profile
    |> moveMovables profile mcDir
    printfn "Fetched!"

let applyProfile profile mcDir =
    printfn "Applying data to Minecraft..."
    mcDir + "/mods"
    |> moveJarFiles (toProfileModPath profile)
    moveMovables profile (toProfilePath profile) mcDir
    printfn "Applied!"

let getModCommands profilePtr: FuncCommands =
    let setRegexModsEnable status aList =
        failOnNoArg aList
        let profile = strFromPtr profilePtr
        for arg in aList do
            setModEnableStatus profile arg status

    let mcDir = getSetting "minecraftDirectory"
                |> replaceEnvs
    let modsPath = mcDir + "/mods"
    let imports = getSetting "modImports"
                  |> replaceEnvs

    let listMCMods () = listAllMods true modsPath
    let listMCModsRegex = listAllModsRegex true modsPath
    let listMCModsRegexes = expandArray listMCModsRegex
    
    let listImportMods () = listAllMods true imports
    let listImportModsRegex = listAllModsRegex true imports
    let listImportModsRegexes = expandArray listImportModsRegex

    let printModDetails (modFetchFunc: string -> ModMetadata) modList aList =
        failOnNoArg aList
        let arg = aList.[0]
        modFetchFunc arg
        |> fun m -> m.PrintFullInfo modList

    let printModList modListFunc modListRegexFunc (aList: string[]) =
        useArrIfNotEmpty modListFunc modListRegexFunc aList
        |> printAllMods

    let printModNameList modListFunc modListRegexFunc (aList: string[]) =
        useArrIfNotEmpty modListFunc modListRegexFunc aList
        |> printAllModNames

    FuncCommands [ ("list", { Function = fun aList -> let profile = strFromPtr profilePtr
                                                      printModList (getProfileMods profile) (getProfileModsRegexes profile) aList
                              Description = "Lists all mods in the current profile"
                              ArgumentDescriptions = [| "filters", "Gives a filtered mod list, uses regex", false |]
                            }
                   )
                   ("listApplied", { Function = printModNameList listMCMods listMCModsRegexes
                                     Description = "Lists all mods currently applied"
                                     ArgumentDescriptions = [| "filters", "Gives a filtered mod list, uses regex", false |]
                                   }
                   )
                   ("listImports", { Function = printModNameList listImportMods listImportModsRegexes
                                     Description = "Lists all mods ready to be imported to the current profile"
                                     ArgumentDescriptions = [| "filters", "Gives a filtered mod list, uses regex", false |]
                                   }
                   )
                   ("disable", { Function = setRegexModsEnable false
                                 Description = "Disables mods by names, uses regex"
                                 ArgumentDescriptions = [| "regexes", "The regexes to use", true |]
                               }
                   )
                   ("enable",  { Function = setRegexModsEnable true
                                 Description = "Enables mods by names, uses regex"
                                 ArgumentDescriptions = [| "regexes", "The regexes to use", true |]
                               }
                   )
                   ("delete", { Function = fun aList -> failOnNoArg aList
                                                        let profile = strFromPtr profilePtr
                                                        let mods = getProfileModsRegexes profile aList
                                                        deleteModInProfile profile
                                                        |> confirmDelete mods (fun m -> m.DisplayName)
                                Description = "Deletes mods by names, uses regex"
                                ArgumentDescriptions = [| "regexes", "The regexes to use", true |]
                              }
                   )
                   ("detail", { Function = fun aList -> let profile = strFromPtr profilePtr
                                                        printModDetails (getProfileModByName profile) (getProfileMods profile ()) aList
                                Description = "Gets the details of a mod by name"
                                ArgumentDescriptions = [| "name", "The name of the mod. Requires full name", true |]
                              }
                   )
                   ("detailApplied", { Function = fun aList -> let modList = listMCMods()
                                                               printModDetails (fun name -> getModByName name modList) modList aList
                                       Description = "Gets the details of an applied mod"
                                       ArgumentDescriptions = [| "name", "The name of the mod. Requires full name", true |]
                                     }
                   )
                   ("detailImport", { Function = fun aList -> let modList = listImportMods()
                                                              printModDetails (fun name -> getModByName name modList) modList aList
                                      Description = "Details a mod in the imports folder"
                                      ArgumentDescriptions = [| "name", "The name of the mod. Requires full name", true |]
                                    }
                   )
                   ("apply", { Function = fun _ -> let profile = strFromPtr profilePtr
                                                   applyProfile profile mcDir
                               Description = "Moves the current profile's mods and movables to the game's folder"
                               ArgumentDescriptions = Array.empty
                             }
                   )
                   ("fetch", { Function = fun _ -> fetchToProfile (strFromPtr profilePtr) mcDir
                               Description = "Moves mods and movables already in the game directory to the current profile"
                               ArgumentDescriptions = Array.empty
                             }
                   )
                   ("import", { Function = fun aList -> let profile = strFromPtr profilePtr
                                                        useArrIfNotEmpty listImportMods listImportModsRegexes aList
                                                        |> Array.map (fun m -> imports + "/" + m.Filename)
                                                        |> moveFileArr (toProfileModPath profile)
                                Description = "Imports mods from the import folder to the current profile"
                                ArgumentDescriptions = [| "filters", "Imports a filtered mod list, uses regex", false |]
                              }
                   )
                 ]

let getProfileCommands profilePtr: FuncCommands =
    let mcDir = getSetting "minecraftDirectory"
                |> replaceEnvs
    let imports = getSetting "modImports"
                  |> replaceEnvs

    let getImportFiles () = listFilesWithExt imports ".zip"
                            |> Array.filter (fun z -> using (readZip z) archiveIsProfile)
    let getImportFilesRegex args = getImportFiles()
                                   |> Array.filter(fun s -> regexMatches s args)

    let getModCommand ptrTuple cmd =
        getModCommands ptrTuple
        |> fun cmds -> cmds.[cmd]

    let editProfilePropertyArgs property args =
        failOnNoArg args
        let profile = if args.Length < 2 then
                          strFromPtr profilePtr
                      else
                          args.[1]
        JsonValue.String args.[0]
        |> setProfileProperty profile property

    let editProfilePropertyArgsArr property argsEnum args =
        failOnNoArg args
        let profile = strFromPtr profilePtr
        let root = getProfileMetaJson profile
        match argsEnum with
        | Set -> args
        | Add -> jsonPropertyStrArr root "movables"
                 |> Array.append args
        | Remove -> jsonPropertyStrArr root "movables"
                    |> Array.filter(fun s -> Array.contains s args |> not)
        |> Array.map JsonValue.String
        |> JsonValue.Array
        |> setProfileProperty profile property

    FuncCommands [ ("list", { Function = fun aList -> useArrIfNotEmpty listProfiles (expandArray listProfilesRegex) aList
                                                      |> printStrArray
                              Description = "Lists all profiles"
                              ArgumentDescriptions = [| "filters", "Gives a filtered profile list, uses regex", false |]
                            }
                   )
                   ("listImports", { Function = fun aList -> useArrIfNotEmpty getImportFiles (expandArray getImportFilesRegex) aList
                                                             |> Array.map getFilenameNoExt
                                                             |> printStrArray
                                     Description = "Lists all .zip profiles ready to be imported to the current profile"
                                     ArgumentDescriptions = [| "filters", "Gives a filtered mod list, uses regex", false |]
                                   }
                   )
                   ("use", { Function = fun aList -> failOnNoArg aList
                                                     let arg = aList.[0]
                                                     let foundArg = listProfiles()
                                                                    |> Array.contains arg
                                                     if foundArg then
                                                         writeStrToPtr profilePtr arg
                                                     else
                                                         failwith $"Profile {arg} not found"
                             Description = "Switches to an existing profile"
                             ArgumentDescriptions = [| "profile", "The profile to switch to. Requires full name", true |]
                           }
                   )
                   ("add", { Function = fun aList -> failOnNoArg aList
                                                     for arg in aList do
                                                        addProfile arg
                             Description = "Creates new profiles by names"
                             ArgumentDescriptions = [| "names", "The names of the new profiles", true |]
                           }
                   )
                   ("delete", { Function = fun aList -> failOnNoArg aList
                                                        let profiles = listProfilesRegexes aList
                                                        confirmDelete profiles passthrough deleteProfile
                                Description = "Deletes profiles by names, uses regex"
                                ArgumentDescriptions = [| "regexes", "The regexes to use", true |]
                              }
                   )
                   ("detail", { Function = fun aList -> useArrIfNotEmpty (fun _ -> strFromPtr profilePtr) Array.head aList
                                                        |> getProfileMeta
                                                        |> fun p -> p.PrintFullInfo
                                Description = "Gets the details of the current profile"
                                ArgumentDescriptions = [| "profile", "Gets the details of the given profile", false |]
                              }
                   )
                   ("detailImport", { Function = fun aList -> failOnNoArg aList
                                                              let zip = getImportFiles() |> Array.find(fun z -> getFilename z = aList.[0] + ".zip")
                                                              use archive = readZip zip
                                                              let entries = archive.Entries |> Array.ofSeq
                                                              match findEntry entries "meta.json" with
                                                              | None -> failwith "Zip file does not have a meta.json"
                                                              | Some e -> readEntry e
                                                                          |> JsonValue.Parse
                                                                          |> getProfileMetaValues
                                                              |> fun p -> p.PrintFullInfo
                                      Description = "Prints the details of a profile to be imported"
                                      ArgumentDescriptions = [| "profile", "The name of the profile to examine. Requires full name", true |]
                                    }
                   )
                   ("rename", { Function = editProfilePropertyArgs "name" 
                                Description = "Change the current profile's display name to a new one"
                                ArgumentDescriptions = [| "name", "The new name of this profile", true
                                                          "profile", "Change the name of the given profile. Requires full name", false |]
                              }
                   )
                   ("describe", { Function = editProfilePropertyArgs "desc" 
                                  Description = "Change the current profile description to a new one"
                                  ArgumentDescriptions = [| "desc", "The new description of this profile", true
                                                            "profile", "Change the description of the given profile. Requires full name", false |]
                                }
                   )
                   ("addMovables", { Function = editProfilePropertyArgsArr "movables" Add
                                     Description = "Add movables to the current profile"
                                     ArgumentDescriptions = [| "args", "The files to be moved between Minecraft's root and the profile's", true |]
                                   }
                   )
                   ("setMovables", { Function = editProfilePropertyArgsArr "movables" Set
                                     Description = "Sets movables on the current profile"
                                     ArgumentDescriptions = [| "args", "The files to be moved between Minecraft's root and the profile's", true |]
                                   }
                   )
                   ("rmMovables", { Function = editProfilePropertyArgsArr "movables" Remove
                                    Description = "Removes movables from the current profile. DOES NOT MOVE FETCHED FILES BACK; APPLY BEFORE THIS"
                                    ArgumentDescriptions = [| "args", "The files to be removed from the movables list", true |]
                                  }
                   )
                   ("apply", { Function = fun aList -> let profile = useArrIfNotEmpty (fun _ -> strFromPtr profilePtr) Array.head aList
                                                       applyProfile profile mcDir
                               Description = (getModCommand profilePtr "apply").Description
                               ArgumentDescriptions = [| "profile", "Use the given profile. Requires full name", false |]
                             }
                   )
                   ("fetch", { Function = fun aList -> let profile = useArrIfNotEmpty (fun _ -> strFromPtr profilePtr) Array.head aList
                                                       fetchToProfile profile mcDir
                               Description = (getModCommand profilePtr "fetch").Description
                               ArgumentDescriptions = [| "profile", "Moves mods to the given profile. Requires full name", false |]
                            }
                   )
                   ("import", { Function = fun aList -> failOnNoArg aList
                                                        let path = imports + "/" + aList.[0] + ".zip"
                                                        let dest = toProfilePath aList.[0]
                                                        if fileExists path |> not then
                                                            failwith "Profile zip file does not exist in imports"

                                                        if using (readZip path) archiveIsProfile |> not then
                                                            failwith "Zip file is not a profile"

                                                        if dirExists dest then
                                                            failwith "Profile name already exists"

                                                        printfn "Importing profile..."
                                                        createDir dest |> ignore
                                                        extractZip path dest
                                                        printfn "Imported!"
                                Description = "Imports a zip archive as a profile"
                                ArgumentDescriptions = [| "name", "The name of the profile to import. Requires full name", true |]
                             }
                   )
                   ("export", { Function = fun aList -> let profile = useArrIfNotEmpty (fun _ -> strFromPtr profilePtr) Array.head aList
                                                        let path = toProfilePath profile
                                                        let dest = imports + "/" + profile + ".zip"
                                                        printfn "Exporting profile..."
                                                        if fileExists dest then
                                                            deleteFile dest // Make room or else zipFolder will throw an exception!
                                                        zipFolder path dest
                                                        printfn "Exported!"
                                Description = "Exports the current profile to the imports folder as a .zip"
                                ArgumentDescriptions = [| "profile", "Exports a specific profile. Requires full name", false |]
                              }
                   )
                   #if DEBUG
                   ("pcp", { Function = fun _ -> strFromPtr profilePtr
                                                 |> printfn "%s"
                             Description = "Prints the current profile"
                             ArgumentDescriptions = Array.empty
                           }
                   )
                   #endif
                 ]