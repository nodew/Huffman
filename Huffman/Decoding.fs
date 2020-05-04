namespace Huffman

open System.IO
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
        buildTree()

    let decodeFile (inputFile: string) (outputFile: string) =
        use inputStream = new BitReader(File.OpenRead(inputFile))
        use outputStream = new BinaryWriter(File.Create(outputFile))
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
        let mutable count = uint64(0)
        let mutable contentLength = uint64(0)
        while inputStream.Readable () && count <= encodedLength do
            match huffmanTree' with
            | HTLeaf a ->
                huffmanTree' <- huffmanTree
                contentLength <- contentLength + uint64(8)
                outputStream.Write(a)
            | HTNode(lt, rt) ->
                let bit = inputStream.ReadBit()
                count <- count + uint64(1)
                huffmanTree' <- match bit with
                                | false -> lt
                                | true -> rt

        if originalLength = contentLength then
            printfn "Successfully decoded %s into %s" inputFile outputFile
        else
            printfn "Mismatch, can't decode original file!"

