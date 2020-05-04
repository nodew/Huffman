namespace Huffman

open System.IO
open System.Collections
open BitStream

module Encoding =
    let encodeHuffmanTree (tree: HuffmanTree<byte>) =
        let rec toBytes (tree: HuffmanTree<byte>) =
            match tree with
            | HTLeaf a ->
                let bitArr = new BitArray([| a |])
                let bits = Array.replicate 8 false
                bitArr.CopyTo(bits, 0)
                [true] @ (bits |> Array.toList)
            | HTNode(lt, rt) ->
                [false] @ toBytes lt @ toBytes rt
        toBytes tree

    let analysisFile input =
        use fs = new BinaryReader(File.OpenRead(input))
        let mutable frequence: FreqTable<byte> = Map.empty
        while (fs.BaseStream.Position < fs.BaseStream.Length) do
            let word = fs.ReadByte()
            frequence <- match Map.tryFind word frequence with
                         | None -> frequence |> Map.add word 1
                         | Some count -> frequence |> Map.add word (count + 1)
        match HuffmanTree.fromFreqTable frequence with
        | Some tree -> Some(tree, frequence)
        | _ -> None
    
    let encodeFile (inputFile: string) (outputFile: string) = 
        match analysisFile inputFile with
        | Some(tree, frequence)->
            let encodingTable = HuffmanCodeTable.fromHuffmanTree tree
            use inputStream = new BinaryReader(File.OpenRead(inputFile))
            use outputStream = new BitWriter(File.Create(outputFile))
            let version = byte(0);
            let orinalLength = uint64(inputStream.BaseStream.Length) * (uint64)8
            let encodedLength = HuffmanCodeTable.getSize frequence encodingTable

            // write version
            outputStream.WriteByte(version)

            // write orignal file length
            outputStream.WriteUInt64(orinalLength)

            // write encoded content length
            outputStream.WriteUInt64(encodedLength)

            // write huffman tree
            outputStream.WriteBits(encodeHuffmanTree tree)

            // write encoded content
            while (inputStream.BaseStream.Position < inputStream.BaseStream.Length) do
                let word = inputStream.ReadByte()
                match HuffmanCodeTable.lookup word encodingTable with
                | Some encoding ->
                    outputStream.WriteBits(encoding |> List.map (fun x -> x.ToBool()))
                | _ -> ()

            printfn "Successfully encoded %s into %s" inputFile outputFile
        | _ -> 
            printfn "Can't build huffman tree"

