namespace Huffman

open System.IO
open System.Text
open BitStream

module Decoding =
    let buildHuffmanTree (bs: BitReader ref) =
        let rec buildTree () =
            let bit = bs.Value.ReadBit()
            if not bit then
                HuffmanTree.merge (buildTree ()) (buildTree ())
            else
                let byte = bs.Value.ReadByte()
                HuffmanTree.from byte

        buildTree ()

    let private decodeStream' (inputStream: BitReader) (outputStream: Stream) =

        // Read version
        let _ = inputStream.ReadByte()

        // Read the original file length
        let originalLength = inputStream.ReadUInt64()

        // Read the encoded content length
        let encodedLength = inputStream.ReadUInt64()

        // Build huffman tree from input stream
        let huffmanTree = buildHuffmanTree (ref inputStream)

        // Decode content
        let mutable huffmanTree' = huffmanTree
        let mutable count = uint64 (0)
        let mutable contentLength = uint64 (0)
        while inputStream.Readable() && count <= encodedLength do
            match huffmanTree' with
            | HTLeaf a ->
                huffmanTree' <- huffmanTree
                contentLength <- contentLength + uint64 (8)
                outputStream.WriteByte(a)
            | HTNode (lt, rt) ->
                let bit = inputStream.ReadBit()
                count <- count + uint64 (1)
                huffmanTree' <-
                    match bit with
                    | false -> lt
                    | true -> rt

        if originalLength = contentLength then Result.Ok true else Result.Error "Content mismatch"

    let decodeStream (stream: 'a :> Stream) =
        use temp = new MemoryStream()
        stream.CopyTo(temp)
        stream.Seek(int64 (0), SeekOrigin.Begin) |> ignore
        temp.Seek(int64 (0), SeekOrigin.Begin) |> ignore
        use bitReader = new BitReader(temp)
        use output = new MemoryStream()
        match decodeStream' bitReader output with
        | Ok _ ->
            let bytes = output.GetBuffer()
            Encoding.UTF8.GetString(bytes) |> Ok
        | Error err -> Error err

    let decodeFile (inputFile: string) (outputFile: string) =
        if not (File.Exists inputFile) then
            sprintf "File %s doesn't exist" inputFile
            |> Result.Error
        else
            use inputStream = new BitReader(File.OpenRead(inputFile))
            use outputStream = File.Create(outputFile) :> Stream
            decodeStream' inputStream outputStream

    let decodeBytes (bytes: byte []) =
        let stream = new MemoryStream(bytes)
        decodeStream stream
