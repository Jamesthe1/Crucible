module TomlynWrapper

open Tomlyn
open Tomlyn.Model

open UtilTypes

let tryParseTOML (toml: string) =
    let doc = Toml.Parse toml
    if doc.HasErrors then
        Failure doc.Diagnostics
    else
        Toml.ToModel doc
        |> Success

let getTOMLValue (tbl: TomlTable) key =
    if tbl.ContainsKey key then
        tbl.[key].ToString()
    else
        ""