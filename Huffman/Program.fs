open Huffman.Encoding
open Huffman.Decoding

let printUsage () =
    printfn "%s" @"Usage: Huffman.exe [--help] [command <args>]
Commands:
    encode <file> <output>              encode a file, write result to output
    deocde <file> <output>              decode a file, write result to output
Options:
    --help                              help info
"

[<EntryPoint>]
let main argv =
    match argv with
    | [|"encode"; input; output|] -> encodeFile input output
    | [|"decode"; input; output|] -> decodeFile input output
    | _ -> printUsage ()
    0
