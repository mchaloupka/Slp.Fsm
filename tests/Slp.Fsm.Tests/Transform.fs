[<FsCheck.Xunit.Properties(Arbitrary=[| typeof<ByteBasedEdges.MachineGenerators> |], MaxTest = 2000, EndSize = 30)>]
module FiniteStateMachine.Transform

open Slp.Fsm
open FsCheck.Xunit

let accepts x = FiniteStateMachine.accepts ByteBasedEdges.edgeEvaluator x

[<Property>]
let ``RemoveLambdaEdges does not change accept`` (orMachine: ByteBasedEdges.ByteBasedFsm) (input: byte list) =
    let machine = orMachine |> FiniteStateMachine.removeLambdaEdges

    (machine |> accepts input) = (orMachine |> accepts input)

[<Property(EndSize=15)>]
let ``After RemoveLambdaEdges there is no lambda edge`` (orMachine: ByteBasedEdges.ByteBasedFsm) =
    let machine = orMachine |> FiniteStateMachine.removeLambdaEdges

    (true, machine.Edges)
    ||> Map.fold (
        fun current _ edges ->
            (current, edges)
            ||> List.fold (
                fun c (_,e) ->
                    match e with
                    | LambdaEdge -> false
                    | _ -> c
            )
    )

[<Property(EndSize=15)>]
let ``After RemoveNonReachable the accept is unchanged`` (orMachine: ByteBasedEdges.ByteBasedFsm) (input: byte list) =
    let machine = orMachine |> FiniteStateMachine.removeNonReachable

    (machine |> accepts input) = (orMachine |> accepts input)

[<Property(EndSize=15)>]
let ``After RemoveNonReachable the non-connected to end-states machine is empty`` (orMachine: ByteBasedEdges.ByteBasedFsm) =
    let machine = 
        { orMachine with
            EndStates = set [ GenericNode.create () ]
        } |> FiniteStateMachine.removeNonReachable

    machine = {
        StartStates = Set.empty
        EndStates = Set.empty
        Edges = Map.empty
    }

[<Property(EndSize=15)>]
let ``After RemoveNonReachable the non-connected to start-states machine is empty`` (orMachine: ByteBasedEdges.ByteBasedFsm) =
    let machine = 
        { orMachine with
            StartStates = set [ GenericNode.create () ]
        } |> FiniteStateMachine.removeNonReachable

    machine = {
        StartStates = Set.empty
        EndStates = Set.empty
        Edges = Map.empty
    }

[<Property>]
let ``After RemoveNonReachable the non-connected is empty`` (leftMachine: ByteBasedEdges.ByteBasedFsm) (rightMachine: ByteBasedEdges.ByteBasedFsm) =
    let machine =
        [
            { leftMachine with
                StartStates = set [ GenericNode.create () ]
            }
            { rightMachine with
                EndStates = set [ GenericNode.create () ]
            }
        ]
        |> GenericNode.choiceMachine
        |> FiniteStateMachine.removeNonReachable

    machine = {
        StartStates = Set.empty
        EndStates = Set.empty
        Edges = Map.empty
    }

[<Property>]
let ``If not accepting machine, then it does not accept any input`` (machine: ByteBasedEdges.ByteBasedFsm) (input: byte list) =
    if machine |> FiniteStateMachine.canAccept |> not then
        machine |> accepts input |> not
    else
        true

[<Property(EndSize=15)>]
let ``If cannot accept, then remove non-reachable results in empty machine`` (machine: ByteBasedEdges.ByteBasedFsm)  =
    let reducedMachine =
        machine
        |> FiniteStateMachine.removeNonReachable

    if machine |> FiniteStateMachine.canAccept then
        reducedMachine <> {
            StartStates = Set.empty
            EndStates = Set.empty
            Edges = Map.empty
        }
    else
        reducedMachine = {
            StartStates = Set.empty
            EndStates = Set.empty
            Edges = Map.empty
        }

[<Property>]
let ``Intersection of machines`` (leftMachine: ByteBasedEdges.ByteBasedFsm) (rightMachine: ByteBasedEdges.ByteBasedFsm) (input: byte list) =
    let machine =
        leftMachine
        |> FiniteStateMachine.intersect ByteBasedEdges.edgeIntersect rightMachine

    (machine |> accepts input) = ((leftMachine |> accepts input) && (rightMachine |> accepts input))
