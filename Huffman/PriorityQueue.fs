namespace Huffman

type PQueue<'a when 'a: comparison> =
    | PQEmpty
    | PQNode of Value: 'a * Left: PQueue<'a> * Right: PQueue<'a>

[<RequireQualifiedAccess>]
module PriorityQueue =
    let empty = PQEmpty
    let mkNode a = PQNode(a, PQEmpty, PQEmpty)

    let rec private merge pA pB =
        match (pA, pB) with
        | (PQEmpty, _) -> pB
        | (_, PQEmpty) -> pA
        | (PQNode (a, lA, rA), PQNode (b, lB, rB)) ->
            if a < b then PQNode(a, merge rA pB, lA) else PQNode(b, merge rB pA, lB)

    let insert value queue = merge queue (mkNode value)

    let pop queue =
        match queue with
        | PQEmpty -> (None, PQEmpty)
        | PQNode (a, left, right) -> (Some a, merge left right)

    let rec map f queue =
        match queue with
        | PQEmpty -> PQEmpty
        | PQNode (a, left, right) -> PQNode(f a, map f left, map f right)

    let fromList (items: 'a list) =
        Seq.fold (fun queue item -> insert item queue) empty items

    let rec toList (queue: PQueue<'a>) =
        let (value, rest) = pop queue
        match value with
        | None -> []
        | Some value -> value :: toList rest
