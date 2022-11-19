module Menus

open System
open FSharp.NativeInterop
open FCMD.Command
open FCMD.Types
open Utils

type MenuType =
    | Mods = 0
    | Imports = 1
    | Profiles = 2
    | Configs = 3
    | Settings = 4

let getMenuCommands menuPtr: FuncCommands =
    FuncCommands [ ("listMenus", { Function = fun _ -> enumToArray<MenuType>
                                                       |> Array.map (fun e -> e.ToString())
                                                       |> printStrArray
                                   Description = "List all menus"
                                   ArgumentDescriptions = Array.empty
                                 }
                   )
                   ("useMenu", { Function = fun aList -> failOnNoArg aList
                                                         let arg = aList.[0]
                                                         try
                                                             let menuType = Enum.Parse (typeof<MenuType>, arg) :?> MenuType
                                                             NativePtr.write menuPtr (menuType)
                                                         with
                                                         | :? Exception -> failwith ("Menu " + arg + " does not exist")
                                 Description = "Changes the menu"
                                 ArgumentDescriptions = [| "menu", "The menu to change to", true |]
                               }
                   )
                   #if DEBUG
                   ("pcm", { Function = fun _ -> printfn "%s" (enumValToString (NativePtr.read menuPtr))
                             Description = "Prints the current menu"
                             ArgumentDescriptions = Array.empty
                           }
                   )
                   #endif
                 ]