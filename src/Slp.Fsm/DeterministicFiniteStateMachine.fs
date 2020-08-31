namespace Slp.Fsm

type DeterministicFiniteStateMachine<'N, 'C> when 'N : comparison =
    { StartState: 'N; Edges: Map<'N, ('N * 'C) list>; EndStates: Set<'N> }

module DeterministicFiniteStateMachine =
    let private addEdge edge fromNode toNode machine =
        let edgesFrom =
            machine.Edges
            |> Map.tryFind fromNode
            |> Option.defaultValue List.empty

        { machine with Edges = machine.Edges |> Map.add fromNode ((toNode, edge)::edgesFrom)}

    let fromFiniteStateMachine (disjunctEdges: 'C list -> 'C list) (isNotDisjunct: 'C -> 'C -> bool) (nonDeterministicMachine: FiniteStateMachine<'N, 'C>) =
        let machine =
            nonDeterministicMachine
            |> FiniteStateMachine.removeLambdaEdges

        let rec buildDeterministicFiniteStateMachine (toProcess: Set<'N> list) (processed: Set<Set<'N>>) (curMachine: DeterministicFiniteStateMachine<Set<'N>, 'C>) =
            match toProcess with
            | [] -> curMachine
            | processing :: xs ->
                if processed |> Set.contains processing then
                    buildDeterministicFiniteStateMachine xs processed curMachine
                else
                    let allEdgesMap =
                        processing
                        |> Set.toSeq
                        |> Seq.choose (fun s -> machine.Edges |> Map.tryFind s |> Option.map (fun x -> s, x))
                        |> Seq.collect (fun (s,x) -> x |> List.map (fun (t,e) -> e, (s, t)))
                        |> Seq.choose (
                            fun (e,x) ->
                                match e with
                                | EdgeWithToken c -> (c,x) |> Some
                                | LambdaEdge -> failwith "There should not be lambda edge present in this stage"
                        )
                        |> Seq.toList
                        |> List.groupBy fst
                        |> List.map (
                            fun (e, x) ->
                                e, x |> List.map (fun (_,(s,t)) -> s,t)
                        )

                    let allDisjunctEdges =
                        allEdgesMap
                        |> List.map fst
                        |> disjunctEdges

                    let (result, addToProcess) =
                        ((curMachine, List.empty), allDisjunctEdges)
                        ||> List.fold (
                            fun (cur,toProc) edgeToAdd ->
                                let nodesChanges =
                                    allEdgesMap
                                    |> List.filter (fst >> (isNotDisjunct edgeToAdd))
                                    |> List.collect snd

                                let nodesToRemove =
                                    nodesChanges
                                    |> List.map fst
                                    |> List.distinct
                                    |> Set.ofList

                                let nodesToAdd =
                                    nodesChanges
                                    |> List.map snd
                                    |> Set.ofList

                                let target =
                                    nodesToRemove
                                    |> Set.difference processing
                                    |> Set.union nodesToAdd

                                cur
                                |> addEdge edgeToAdd processing target, target::toProc
                        )

                    let newEndStates =
                        if processing |> Set.exists (fun x -> machine.EndStates |> Set.contains x) then
                            curMachine.EndStates |> Set.add processing
                        else
                            curMachine.EndStates

                    { result with EndStates = newEndStates }
                    |> buildDeterministicFiniteStateMachine (xs @ addToProcess) (processed |> Set.add processing)

        { StartState = machine.StartStates; Edges = Map.empty; EndStates = Set.empty }
        |> buildDeterministicFiniteStateMachine (machine.StartStates |> List.singleton) Set.empty

    let accepts (edgeEvaluator: ('C * 'X) -> bool) input machine =
        let rec acceptsFromState state restInput =
            match restInput with
            | [] ->
                machine.EndStates |> Set.contains state
            | x::xs ->
                let edgesFrom =
                    machine.Edges
                    |> Map.tryFind state
                    |> Option.defaultValue List.empty
                    |> List.filter (
                        fun (t, e) ->
                            edgeEvaluator(e, x)
                    )

                match edgesFrom with
                | [] ->
                    false
                | (t,_)::[] ->
                    acceptsFromState t xs
                | matchingEdges ->
                    matchingEdges
                    |> List.map snd
                    |> failwithf "Found non-disjunct edges in a machine: %A"

        acceptsFromState machine.StartState input

    let transformNodes transformNode (machine: DeterministicFiniteStateMachine<_,_>) =
        let nodeTransform =
            (machine.EndStates |> Set.add machine.StartState, machine.Edges)
            ||> Map.fold (
                fun current node edges ->
                    (current |> Set.add node, edges)
                    ||> List.fold (
                        fun cur (n, _) ->
                            cur |> Set.add n
                    )
            )
            |> Set.toSeq
            |> Seq.map (fun x -> x, transformNode x)
            |> Map.ofSeq

        let transformedEdges =
            (Map.empty, machine.Edges)
            ||> Map.fold (
                fun current orNode orEdges ->
                    let edges = orEdges |> List.map (fun (n, e) -> nodeTransform.[n], e)
                    let node = nodeTransform.[orNode]
                    current |> Map.add node edges
            )

        {
            StartState = nodeTransform.[machine.StartState]
            EndStates = machine.EndStates |> Set.map (fun n -> nodeTransform.[n])
            Edges = transformedEdges
        }
