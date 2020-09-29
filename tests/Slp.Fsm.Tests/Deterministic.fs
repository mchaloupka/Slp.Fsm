[<FsCheck.Xunit.Properties(Arbitrary=[| typeof<ByteBasedEdges.MachineGenerators> |], MaxTest = 2000, EndSize = 30)>]
module FiniteStateMachine.Deterministic

open Slp.Fsm
open FsCheck.Xunit

let acceptsFsm = FiniteStateMachine.accepts ByteBasedEdges.edgeEvaluator
let acceptsDfsm = DeterministicFiniteStateMachine.accepts ByteBasedEdges.edgeEvaluator
let acceptsDNfsm = DeterministicFiniteStateMachineNode.accepts ByteBasedEdges.edgeEvaluator

let disjunctEdges edges =
    edges
    |> List.distinct
    |> function
    | [] -> []
    | x::[] -> [x]
    | _ -> [AnyByte]

let isNotDisjunct edge1 edge2 =
    match edge1, edge2 with
    | AnyByte, _
    | _, AnyByte ->
        true
    | ExactByte x, ExactByte y when x = y ->
        true
    | _ ->
        false

let fromFiniteStateMachine =
    DeterministicFiniteStateMachine.fromFiniteStateMachine disjunctEdges isNotDisjunct

[<Property>]
let ``Deterministic machine accepts the same as original machine if the edges are disjunct`` (originalMachine: ByteBasedEdges.ByteBasedFsm) (input: byte list) =
    let deterministicMachine = originalMachine |> fromFiniteStateMachine

    let originalAccepts = originalMachine |> acceptsFsm input
    let deterministicAccepts = deterministicMachine |> acceptsDfsm input

    match originalAccepts, deterministicAccepts with
    | true, true
    | false, false ->
        true
    | true, false ->
        false
    | false, true ->
        originalMachine.Edges
        |> Map.toSeq
        |> Seq.collect snd
        |> Seq.map snd
        |> Seq.contains (AnyByte |> EdgeWithToken)

[<Property>]
let ``Deterministic machine accepts the same even when converted to node form if the edges are disjunct`` (originalMachine: ByteBasedEdges.ByteBasedFsm) (input: byte list) =
    let deterministicMachineStartNode =
        originalMachine
        |> fromFiniteStateMachine
        |> DeterministicFiniteStateMachineNode.buildFromDeterministicFiniteStateMachine

    let originalAccepts = originalMachine |> acceptsFsm input
    let deterministicAccepts = deterministicMachineStartNode |> acceptsDNfsm input

    match originalAccepts, deterministicAccepts with
    | true, true
    | false, false ->
        true
    | true, false ->
        false
    | false, true ->
        originalMachine.Edges
        |> Map.toSeq
        |> Seq.collect snd
        |> Seq.map snd
        |> Seq.contains (AnyByte |> EdgeWithToken)
