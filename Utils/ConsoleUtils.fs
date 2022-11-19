module ConsoleUtils

open System
open Printf

let internal printColor (printFunc: TextWriterFormat<string -> unit> -> string -> unit) color text =
    Console.ForegroundColor <- color
    printFunc "%s" text
    Console.ResetColor()

let printfColor =
    printColor printf

let printfnColor =
    printColor printfn

/// <summary>Prints an error in red text</summary>
let printError text =
    printfnColor ConsoleColor.Red $"FAILED: {text}"
    
/// <summary>Prints a warning in yellow text</summary>
let printWarning text =
    printfnColor ConsoleColor.Yellow $"WARNING: {text}"

let printLayout title (args: (string * string)[]) =
    printfnColor ConsoleColor.Cyan title
    Array.iter (fun (name, desc) -> printfColor ConsoleColor.DarkCyan $"\t{name}"
                                    printfn ": %s" desc
               ) args