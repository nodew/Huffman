namespace Huffman

open System.Collections
open System.IO
open System.Text

open BitStream

module Encoding =
    let encodeHuffmanTree (tree: HuffmanTree<byte>) =
        let rec toBytes (tree: HuffmanTree<byte>) =
            match tree with
            | HTLeaf a -> [ true ] @ (convertByteToBits a)
            | HTNode (lt, rt) -> [ false ] @ toBytes lt @ toBytes rt

        toBytes tree

    let analysisStream (stream: Stream) =
        let mutable frequence: FreqTable<byte> = Map.empty
        while stream.Position < stream.Length do
            let word = byte (stream.ReadByte())
            frequence <-
                match Map.tryFind word frequence with
                | None -> frequence |> Map.add word 1
                | Some count -> frequence |> Map.add word (count + 1)
        match HuffmanTree.fromFreqTable frequence with
        | Some tree -> Ok(tree, frequence)
        | None -> Error "Can't build huffman tree from input"

    let private encodeStream'
        (reader: Stream)
        (writer: BitWriter)
        ((tree: HuffmanTree<byte>), (frequence: FreqTable<byte>))
        =
        let encodingTable = HuffmanCodeTable.fromHuffmanTree tree
        let version = byte (0)
        let orinalLength = uint64 (reader.Length) * (uint64) 8

        let encodedLength =
            HuffmanCodeTable.getSize frequence encodingTable

        // write version
        writer.WriteByte(version)

        // write orignal file length
        writer.WriteUInt64(orinalLength)

        // write encoded content length
        writer.WriteUInt64(encodedLength)

        // write huffman tree
        writer.WriteBits(encodeHuffmanTree tree)

        let mutable Break = false

        // write encoded content
        while reader.Position < reader.Length && not Break do
            let word = byte (reader.ReadByte())

            match HuffmanCodeTable.lookup word encodingTable with
            | Some encoding -> writer.WriteBits(encoding |> List.map (fun x -> x.ToBool()))
            | _ -> Break <- true

        if not Break then
            writer.Complete()
            Ok()
        else
            Error "Unknown error"

    let encodeFile (inputFile: string) (outputFile: string) =
        if not (File.Exists(inputFile)) then
            sprintf "%s does't exist" inputFile |> Error
        else
            use input = File.OpenRead(inputFile)
            use writer = new BitWriter(File.Create(outputFile))

            analysisStream input
            |> Result.map (fun result ->
                input.Seek(int64 (0), SeekOrigin.Begin) |> ignore
                encodeStream' input writer result)

    let encodeStream (input: 'a :> Stream) =
        let output = new MemoryStream()
        use writer = new BitWriter(output)
        analysisStream input
        |> Result.map (fun result ->
            input.Seek(int64 (0), SeekOrigin.Begin) |> ignore
            encodeStream' input writer result)
        |> Result.map (fun _ -> output.ToArray())

    let encodeString (input: string) =
        use stream =
            new MemoryStream(Encoding.UTF8.GetBytes(input))

        encodeStream stream
