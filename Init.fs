module Init

open System
open FSharp.NativeInterop

open FCMD.Command
open FCMD.Types
open FGUI.RuntimeMgr
open FGUI.Elements
open FGUI.TextureMgr
open FGUI.FontMgr

open FileManagement
open InputManagement
open Menus
open Utils

let private getCommands (menuPtr: nativeptr<MenuType>) (menucmds: FuncCommands) (menuMap: Map<MenuType, FuncCommands>) (): FuncCommands =
    try
        mergeMaps menucmds menuMap.[NativePtr.read menuPtr]
    with
    | :? Exception -> (NativePtr.read menuPtr).ToString()
                      |> printfn "Error: Menu %s not implemented yet"
                      menucmds

let initTerminal profileTuple menuCmds menuMap (menuPtr: nativeptr<MenuType>) =
    printfn "Starting terminal mode..."
    
    // Use pointers for correct update
    let getPrefix menuPtr () =
        strFromPtr profileTuple + "/" + enumValToString (NativePtr.read menuPtr) + "> "
    
    inputLoop (getPrefix menuPtr) (getCommands menuPtr menuCmds menuMap)

let initGraphics (screenxy: int[]) =
    printfn "Loading graphics..."
    let uiRoot = "UI/" + getSetting "theme" + "/"
    // The file is automatically appended to the end of the function
    let themeData = readJsonFile (uiRoot + "meta.json")

    let elements: IElement[] = [| BaseQuad(-1.0f, 0.8f,  2.0f, 0.2f, 0.0f, 0.05f, 0.4f, 3, 0)
                                  BaseQuad(-0.2f, -0.9f, 1.1f, 1.6f, 0.0f, 0.1f,  0.1f, 2, 0)
                                  BaseQuad(-0.8f, -0.2f, 0.5f, 0.9f, 0.0f, 0.3f,  0.2f, 2, 0)
                                  (ButtonQuad(-0.79f, 0.505f, 0.48f, 0.18f, 0.1f, 0.2f, 0.3f, 0, 1, 2, 0) :> IInteractiveElement).SetOnClick (fun _ -> printfn "Hello world!")
                                  (ButtonQuad(-0.79f, 0.325f, 0.48f, 0.18f, 0.1f, 0.2f, 0.3f, 0, 1, 2, 0) :> IInteractiveElement).SetOnClick (fun input -> input.Button.ToString()
                                                                                                                                                           |> printfn "You pushed %s"
                                                                                                                                             )
                               |]
    let uiTexture = { path = uiRoot + "bars.png"; x = 0; y = 0; w = 128; h = 128; atlas = makeAtlasGrid 2 2 }
    let color = ColorRGBString(jsonPropertyStr themeData "bg")
    let font = loadFont "fonts/lucon.ttf" 24f
    
    loadGraphicsShortStyled "Crucible Mod Manager" screenxy.[0] screenxy.[1] [| "icons/icon.png"; "icons/icon16.png" |] color [| uiTexture |] font elements [| onInputEvent |]
    |> runWindow