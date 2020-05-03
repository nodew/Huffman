namespace Huffman

open System
open System.Collections

type FreqTable<'a> when 'a: comparison 
    = Map<'a, int>

[<RequireQualifiedAccess>]
module FreqTable =
    let empty = Map.empty<'a, int>

    let add (x: 'a) (table: FreqTable<'a>) =
        match Map.tryFind x table with
        | None -> table |> Map.add x 1
        | Some w -> table |> Map.add x (w + 1)

    let fromList (items: 'a list) =
        Seq.fold (fun (table: FreqTable<'a>) x -> add x table) Map.empty items

[<CustomComparison;StructuralEquality>]
type Weighted<'a> = 
    | Weighted of Weight: int * Item: 'a
    interface IComparable<Weighted<'a>> with
        override x.CompareTo obj = (x.Weight()).CompareTo(obj.Weight())
    
    interface IComparable with
        override x.CompareTo obj = 
            match obj with
            | null -> 1
            | :? Weighted<'a> as weightedObj -> (x.Weight()).CompareTo(weightedObj.Weight())
            | _ -> invalidArg "obj" "Not an instance of Weighted"
    
    override x.ToString() =
        match x with Weighted(w, a) -> sprintf "(%d, %s)" w (a.ToString())

    member x.Weight() =
        match x with Weighted(w, _) -> w

type HuffmanTree<'a> when 'a : comparison = 
    | HTLeaf of 'a
    | HTNode of Left: HuffmanTree<'a> * Right: HuffmanTree<'a>
    override x.ToString() =
        match x with
        | HTLeaf a -> sprintf "%s" (a.ToString())
        | HTNode(lt, rt) -> sprintf "{%s %s}" (lt.ToString()) (rt.ToString())

type WeightedHuffumanTree<'a> when 'a : comparison 
    = Weighted<HuffmanTree<'a>>

[<RequireQualifiedAccess>]
module HuffmanTree =
    let from = HTLeaf
    
    let merge lt rt = HTNode(lt, rt)

    let rec map f tree = 
        match tree with
        | HTLeaf a -> f a
        | HTNode(lt, rt) -> HTNode(map f lt, map f rt)

    let mkWHT w a = Weighted(w, from a)
    
    let mergeWHT (Weighted(w1, htA)) (Weighted(w2, htB)) =
            Weighted(w1 + w2, merge htA htB)

    let private genPriorityQueue (freqTable: FreqTable<'a>) =
        let folder pQueue (x, w) = PriorityQueue.insert (Weighted(w, x)) pQueue
        Map.toSeq freqTable |> Seq.fold folder PriorityQueue.empty

    let rec private buildTree pQueue =
        match PriorityQueue.pop pQueue with
        | (None, _) -> None
        | (Some x1, pQueue') ->
            match PriorityQueue.pop pQueue' with
            | (None, _) -> Some x1
            | (Some x2, pQueue'') ->
                let combinedNode = mergeWHT x1 x2
                let newPQueue = PriorityQueue.insert combinedNode pQueue''
                buildTree newPQueue

    let fromPriorityQueue pQueue =
        let transform item = match item with Weighted(w, a) -> Weighted(w, from a)
        let pQueue' = pQueue |> PriorityQueue.map transform
        match buildTree pQueue' with
        | Some(Weighted(_, tree)) -> Some tree
        | _ -> None
    
    let fromFreqTable freqTable =
        freqTable |> genPriorityQueue |> fromPriorityQueue

    let fromList (items: 'a list when 'a: comparison) = 
        items |> FreqTable.fromList |> fromFreqTable

type Direction = 
    | L | R
    override x.ToString() =
        match x with
        | L -> "0"
        | R -> "1"
    member x.ToBool() =
        match x with L -> false | R -> true

type HuffmanCode = Direction list

[<RequireQualifiedAccess>]
module HuffmanCode =
    let empty: HuffmanCode = []

type HuffmanCodeTable<'a> when 'a: comparison = Map<'a, HuffmanCode>

module HuffmanCodeTable =
    let fromHuffmanTree (tree : HuffmanTree<'a>) =
        let rec go path = function
                   | HTLeaf a -> [(a, List.rev path)]
                   | HTNode (lt, rt) ->
                        let lt' = go (L :: path) lt
                        let rt' = go (R :: path) rt
                        in List.append lt' rt'
        go [] tree |> Map.ofSeq

    let lookup x (m: Map<'a, HuffmanCode>) =
        Map.tryFind x m

    let getSize (freqTable: FreqTable<'a>) (codeTable: HuffmanCodeTable<'a>) =
        codeTable |> Map.fold (fun sum key encoding -> uint64(Map.find key freqTable * sizeof<'a> * encoding.Length) + sum) (uint64(0))