namespace Huffman.Tests

open Xunit
open FsUnit.Xunit
open Huffman

module FreqTableTests =
    [<Fact>]
    let ``FreqTable - fromList`` () =
        let freqTable =
            [ 1; 4; 5; 3; 2; 4; 5; 3; 5; 3 ]
            |> FreqTable.fromList

        freqTable.Count |> should equal 5
        Map.find 1 freqTable |> should equal 1
        Map.find 5 freqTable |> should equal 3

    [<Fact>]
    let ``FreqTable - add`` () =
        let freqTable =
            [ 1; 4; 5; 3; 2; 4; 5; 3; 5; 3 ]
            |> FreqTable.fromList
            |> FreqTable.add 1

        Map.find 1 freqTable |> should equal 2

module HuffmanTreeTests =
    [<Fact>]
    let ``HuffmanTree - fromList`` () =
        let tree =
            [ 1; 4; 5; 3; 2; 4; 5; 3; 5; 3 ]
            |> HuffmanTree.fromList

        let expected = "{{3 5} {4 {1 2}}}"
        tree |> should not' None
        let actual = tree.Value.ToString()
        actual |> should equal expected

module HuffmanCodeTableTests =
    [<Fact>]
    let ``HuffmanCodeTable - build & lookup`` () =
        let codeTable =
            [ 1; 4; 5; 3; 2; 4; 5; 3; 5; 3 ]
            |> HuffmanTree.fromList
            |> Option.map HuffmanCodeTable.fromHuffmanTree
            |> Option.get

        HuffmanCodeTable.lookup 3 codeTable
        |> Option.get
        |> should equal [ L; L ]
        HuffmanCodeTable.lookup 5 codeTable
        |> Option.get
        |> should equal [ L; R ]
        HuffmanCodeTable.lookup 4 codeTable
        |> Option.get
        |> should equal [ R; L ]
        HuffmanCodeTable.lookup 1 codeTable
        |> Option.get
        |> should equal [ R; R; L ]
        HuffmanCodeTable.lookup 2 codeTable
        |> Option.get
        |> should equal [ R; R; R ]

    [<Fact>]
    let ``HuffmanCodeTable - getSize`` () =
        let freqTable =
            [ 1; 4; 5; 3; 2; 4; 5; 3; 5; 3 ]
            |> FreqTable.fromList

        let tree =
            HuffmanTree.fromFreqTable freqTable |> Option.get

        let codeTable = tree |> HuffmanCodeTable.fromHuffmanTree

        let size =
            HuffmanCodeTable.getSize freqTable codeTable

        size
        |> should equal (uint64 (((3 + 3 + 2) * 2 + (1 + 1) * 3) * sizeof<int>))
