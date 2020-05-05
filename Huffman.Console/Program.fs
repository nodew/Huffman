open Huffman.Encoding
open Huffman.Decoding

let printUsage () =
    printfn "%s" @"Usage: hf.exe [--help] [command <args>]
Commands:
    encode <file> <output>              encode a file, write result to output
    deocde <file> <output>              decode a file, write result to output
Options:
    --help                              help info
"

[<EntryPoint>]
let main argv =
    match argv with
    | [|"encode"; input; output|] ->
        match encodeFile input output with
        | Ok _ -> printfn "Encode succeed"
        | Error err -> printfn "Encode failed: %s" err
    | [|"decode"; input; output|] ->
        match decodeFile input output with
        | Ok _ -> printfn "Decode succeed"
        | Error err -> printfn "Decode failed: %s" err

    | _ -> printUsage ()

    0
