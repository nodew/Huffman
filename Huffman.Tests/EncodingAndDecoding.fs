namespace Huffman.Tests

open Xunit
open FsUnit.Xunit
open Huffman.Encoding
open Huffman.Decoding

module EncodingAndDecodingTests =
    [<Fact>]
    let ``Encode and Decode stream`` () =
        let testStr = "hello world"
        let bytes = encodeString testStr

        let r =
            match bytes |> Result.bind decodeBytes with
            | Ok s -> Some s
            | Error _ -> None

        r |> should not' None
        r |> Option.get |> should equal testStr
