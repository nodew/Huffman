namespace Huffman.Tests

open Xunit
open FsUnit.Xunit
open Huffman

module PriorityQueueTests =
    [<Fact>]
    let ``PrirotyQueue - fromList & toList`` () =
        let expected = [ 1; 2; 3; 4; 5 ]

        [ 1; 2; 3; 4; 5 ]
        |> PriorityQueue.fromList
        |> PriorityQueue.toList
        |> should equal expected

        [ 5; 4; 3; 2; 1 ]
        |> PriorityQueue.fromList
        |> PriorityQueue.toList
        |> should equal expected

        [ 1; 4; 5; 3; 2 ]
        |> PriorityQueue.fromList
        |> PriorityQueue.toList
        |> should equal expected

    [<Fact>]
    let ``PrirotyQueue - duplicated elements`` () =
        [ 1; 4; 5; 3; 2; 4; 5; 2; 3; 5; 3 ]
        |> PriorityQueue.fromList
        |> PriorityQueue.toList
        |> should equal [ 1; 2; 2; 3; 3; 3; 4; 4; 5; 5; 5 ]
