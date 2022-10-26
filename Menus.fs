module Menus

open System
open FSharp.NativeInterop
open FCMD.Command
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
                               }
                   )
                   #if DEBUG
                   ("pcm", { Function = fun _ -> printfn "%s" (enumValToString (NativePtr.read menuPtr))
                             Description = "Prints the current menu"
                           }
                   )
                   #endif
                 ]