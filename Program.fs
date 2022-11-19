open FSharp.NativeInterop

open FileManagement
open ProfileManagement
open Utils
open Menus
open Init

// Args do not count the executable name
[<EntryPoint>]
let main args =
    let mcDir = getSetting "minecraftDirectory"
    let buildVersion =
        readFileKV "buildinfo.txt"
        |> Map.find "version"
    let screenxy =
        (getSetting "dimensions").Split 'x'
        |> Array.map int
    let mutable profile = getSetting "defaultProfile"
    let profileNameLen = listProfiles()
                         |> getLongestStrLen

    // Cleans up after we leave scope
    let stack = NativePtr.stackalloc profileNameLen
    let profileTuple = strIntoPtr profile profileNameLen stack
    let mutable menu = MenuType.Mods

    let menuCmds = getMenuCommands &&menu
    let menuMap = Map [ MenuType.Mods, getModCommands profileTuple
                        MenuType.Profiles, getProfileCommands profileTuple
                      ]

    printfn "[Crucible Mod Manager %s] by Jamesthe1" buildVersion
    printfn "\tMinecraft directory is %s" mcDir
    printfn "\tImports are fetched from %s" (getSetting "modImports")
    
    if Array.contains "-terminal" args then
        initTerminal profileTuple menuCmds menuMap &&menu
    else
        initGraphics screenxy
    printfn "Shutting down!"
    0