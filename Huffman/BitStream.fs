module Huffman.BitStream

open System
open System.IO
open System.Collections

let convertBitsToBytes (bits: bool list) =
    if bits.Length < 8 then
        invalidArg "bits" "The bits shouldn't be less than 8 bit"
    else
        let size = bits.Length / 8
        let (current, rest) = List.splitAt (8 * size) bits
        let bitArr = BitArray(current |> List.toArray)
        let _bytes: byte [] = Array.zeroCreate size
        bitArr.CopyTo(_bytes, 0)
        _bytes, rest

let convertBytesToBits (bytes: byte []) =
    let bitArr = BitArray(bytes)
    let bits = Array.replicate (8 * bytes.Length) false
    bitArr.CopyTo(bits, 0)
    bits |> Array.toList

let convertByteToBits (byte: byte) = convertBytesToBits ([| byte |])

type BitWriter(stream: Stream) =
    let bs = new BinaryWriter(stream)
    let mutable bits: bool list = []

    member _.BaseStream = bs.BaseStream

    member private _.WriteInternal() =
        if (bits.Length >= 8) then
            let (_bytes, rest) = convertBitsToBytes bits
            bs.Write(_bytes)
            bits <- rest

    member x.WriteBit flag =
        bits <- bits @ [ flag ]
        x.WriteInternal()

    member x.WriteBits flags =
        bits <- bits @ flags
        x.WriteInternal()

    member x.WriteByte(byte: byte) =
        bits <- bits @ (convertByteToBits byte)
        x.WriteInternal()

    member x.WriteUInt32(i: uint32) =
        let _bytes: byte [] = BitConverter.GetBytes(i)
        _bytes |> Seq.iter x.WriteByte

    member x.WriteUInt64(i: uint64) =
        let _bytes: byte [] = BitConverter.GetBytes(i)
        _bytes |> Seq.iter x.WriteByte

    member x.Complete() =
        if (bits.Length > 0) then
            bits <- (bits @ List.replicate 8 false) |> List.take 8
            x.WriteInternal()

    interface IDisposable with
        override x.Dispose() =
            x.Complete()
            bs.Dispose()

type BitReader(stream: Stream) =
    let bs = new BinaryReader(stream)
    let mutable bits: bool list = []

    let loadBits () =
        let _byte = bs.ReadByte()
        bits <- bits @ (convertByteToBits _byte)

    member _.BaseStream = bs.BaseStream

    member _.Readable() =
        not
            (bits.Length = 0
             && bs.BaseStream.Position = bs.BaseStream.Length)

    member x.ReadBit() =
        if bits.Length = 0 then
            loadBits ()
            x.ReadBit()
        else
            let bit = List.head bits
            bits <- List.tail bits
            bit

    member _.ReadByte() =
        if (bits.Length = 0) then
            bs.ReadByte()
        else
            while bits.Length < 8 do
                loadBits ()
            let (bytes, rest) = convertBitsToBytes (bits)
            bits <- rest
            bytes.[0]

    member _.ReadUInt32() =
        if (bits.Length = 0) then
            bs.ReadInt32()
        else
            while (bits.Length < 32) do
                loadBits ()
            let (bytes, rest) = convertBitsToBytes (bits)
            bits <- rest
            BitConverter.ToInt32(bytes, 0)

    member _.ReadUInt64() =
        if (bits.Length = 0) then
            bs.ReadUInt64()
        else
            while (bits.Length < 64) do
                loadBits ()
            let (bytes, rest) = convertBitsToBytes (bits)
            bits <- rest
            BitConverter.ToUInt64(bytes, 0)

    interface IDisposable with
        override _.Dispose() = bs.Dispose()
