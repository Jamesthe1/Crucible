open System
open System.IO
open FSharp.NativeInterop
open FSharp.Data

open FGUI.RuntimeMgr
open FGUI.Elements
open FGUI.TextureMgr
open FGUI.FontMgr
open FCMD.Command

open FileManagement
open ProfileManagement
open Utils
open Menus
open InputManagement

// Args do not count the executable name
[<EntryPoint>]
let main args =
    let mcDir = getSetting "minecraftDirectory"
    let buildVersion =
        readFileKV "buildinfo.txt"
        |> Map.find "version"
    let screenxy =
        (getSetting "dimensions").Split 'x'
        |> Array.map (fun d -> d |> int)
    let mutable profile = getSetting "defaultProfile"
    let profileNameLen = listProfiles ()
                         |> getLongestStrLen

    // Cleans up after we leave scope
    let stack = NativePtr.stackalloc profileNameLen
    let profileTuple = strIntoPtr profile profileNameLen stack
    let mutable menu = MenuType.Mods
    let menuPtr = &&menu

    let menucmds = getMenuCommands menuPtr
    let menuMap = Map [ MenuType.Mods, getModCommands profileTuple
                        MenuType.Profiles, getProfileCommands profileTuple
                      ]
    let getCommands menuPtr (): FuncCommands =
        try
            mergeMaps menucmds menuMap.[NativePtr.read menuPtr]
        with
        | :? Exception -> (NativePtr.read menuPtr).ToString()
                          |> printfn "Error: Menu %s not implemented yet"
                          menucmds

    printfn "[Crucible Mod Manager %s] by Jamesthe1" buildVersion
    printfn "\tMinecraft directory is %s" mcDir
    printfn "\tMod imports are fetched from %s" (getSetting "modImports")

    match stringTrueFalse (getSetting "moveModsToProfile") with
    | Success(b) -> if b then
                        try
                            toProfilePath profile
                            |> moveFiles (mcDir + "/mods")
                        with
                        | :? Exception -> printfn "Could not move one or more mods, stopping task"
    | Failure(_) -> ()
    
    if Array.contains "-terminal" args then
        printfn "Starting terminal mode..."

        // Use pointers for correct update
        let getPrefix menuPtr () =
            strFromPtr profileTuple + "/" + enumValToString (NativePtr.read menuPtr) + "> "

        inputLoop (getPrefix menuPtr) (getCommands menuPtr)
    else
        printfn "Loading graphics..."
        let uiRoot = "UI/" + getSetting "theme" + "/"
        // The file is automatically appended to the end of the function
        let themeData = using (File.Open (uiRoot + "meta.json", FileMode.Open)) JsonValue.Load

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
    printfn "Shutting down!"
    0