namespace Huffman.Tests

open Xunit
open FsUnit.Xunit
open Huffman

module FreqTableTests =
    [<Fact>]
    let ``FreqTable - fromList`` () =
        let freqTable = [1; 4; 5; 3; 2; 4; 5; 3; 5; 3] |> FreqTable.fromList
        freqTable.Count |> should equal 5
        Map.find 1 freqTable |> should equal 1
        Map.find 5 freqTable |> should equal 3

    [<Fact>]
    let ``FreqTable - add`` () =
        let freqTable = [1; 4; 5; 3; 2; 4; 5; 3; 5; 3] |> FreqTable.fromList |> FreqTable.add 1
        Map.find 1 freqTable |> should equal 2

module HuffmanTreeTests =
    [<Fact>]
    let ``HuffmanTree - fromList`` () =
        let tree = [1; 4; 5; 3; 2; 4; 5; 3; 5; 3] |> HuffmanTree.fromList
        let expected = "{{3 5} {4 {1 2}}}"
        tree |> should not' None
        let actual = tree.Value.ToString() 
        actual |> should equal expected