module UtilTypes

open System

open StringHelper
open ConsoleUtils

[<StructuredFormatDisplay("{AsString}")>]
type ModMetadata =
    { Filename: string
      ModId: string
      ModName: string
      Description: string
      Version: string
      GameVersion: string
      Logo: string
      URL: string
      UpdateURL: string
      Authors: string[]
      Credits: string
      Parent: string
      Screenshots: string[]
      Dependencies: string[]
      Enabled: bool }
with
    static member empty = { Filename = ""
                            ModId = ""
                            ModName = ""
                            Description = ""
                            Version = ""
                            GameVersion = ""
                            Logo = ""
                            URL = ""
                            UpdateURL = ""
                            Authors = Array.empty
                            Credits = ""
                            Parent = ""
                            Screenshots = Array.empty
                            Dependencies = Array.empty
                            Enabled = true }
    member private this.EnabledAsText = if this.Enabled then "ENABLED "
                                        else "DISABLED"
    member this.DisplayName = match this.ModName with
                              | "" -> this.Filename
                              | name -> name
    override this.ToString() = this.EnabledAsText + " : " + this.DisplayName
    member this.AsString = this.ToString()

    member private this.RuntimeFix (arg: string) =
        if arg.StartsWith "${" then
            "Determined at runtime"
        else
            arg

    // Members are allowed to be recursive by default
    member private this.FindDependencyNames (modList: ModMetadata[]) start =
        if this.Dependencies.Length = 0 then
            "None"
        else
            let getI = let dependency = this.Dependencies.[start]
                       try
                           modList
                           |> Array.find (fun m -> m.ModId = dependency)
                           |> fun m -> m.ModName
                       with
                       | :? Exception -> dependency
            let iNext = start + 1
            if iNext = this.Dependencies.Length then
                getI
            else
                arrToString [| getI; this.FindDependencyNames modList iNext |]

    member private this.PrintInfoLayout = printLayout this.DisplayName
    member private this.PrintNoInfo = this.PrintInfoLayout [| "NULL", "No information available, mod info not found or improperly formatted" |]
    member private this.PrintAllInfo modList = this.PrintInfoLayout [| "Filename", this.Filename
                                                                       "ID", this.ModId
                                                                       "Description", this.Description
                                                                       "Version", this.RuntimeFix this.Version
                                                                       "Minecraft Version", this.RuntimeFix this.GameVersion
                                                                       "Authors", arrToString this.Authors
                                                                       "Credits", this.Credits
                                                                       "Dependencies", this.FindDependencyNames modList 0
                                                                    |]
    member this.PrintFullInfo modList = if this.ModName.Equals "" then
                                            this.PrintNoInfo
                                        else
                                            this.PrintAllInfo modList
    
[<StructuredFormatDisplay("{AsString}")>]
type ProfileMetadata =
    { Name: string
      Description: string
      Movables: string[] }
with
    override this.ToString () = this.Name
    member this.AsString = this.ToString()
    
    member this.PrintFullInfo = printLayout this.Name [| "Description", this.Description
                                                         "Moves files", arrToString this.Movables
                                                      |]

type Result<'TSuccess, 'TFailure> =
    | Success of 'TSuccess
    | Failure of 'TFailure

/// <summary>
/// Unmanaged string type for pointing
/// </summary>
type UmgdString = nativeptr<char>

type ArrayActions =
    | Add
    | Remove
    | Set