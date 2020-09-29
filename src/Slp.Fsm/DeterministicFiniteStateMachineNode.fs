namespace Slp.Fsm

type IDeterministicFiniteStateMachineNode<'C> =
    abstract member Edges: (IDeterministicFiniteStateMachineNode<'C> * 'C) list with get
    abstract member IsEndState: bool with get

module DeterministicFiniteStateMachineNode =
    type private Node<'C>(isEndState: bool) =
        let mutable edges = List.empty

        member _.AddEdge(targetNode: IDeterministicFiniteStateMachineNode<'C>, edge: 'C) =
            edges <- (targetNode, edge) :: edges

        interface IDeterministicFiniteStateMachineNode<'C> with
            member _.Edges with get () = edges
            member _.IsEndState with get () = isEndState

    let buildFromDeterministicFiniteStateMachine (machine: DeterministicFiniteStateMachine<_,'C>) =
        let allStates =
            machine.Edges
            |> Map.toList
            |> List.collect (
                fun (from, edges) ->
                    from :: (
                        edges
                        |> List.map fst
                    )
            )
            |> Set.ofList
            |> Set.union machine.EndStates
            |> Set.add machine.StartState
            |> Set.toList

        let allNodes =
            allStates
            |> List.map (fun s -> s, Node<'C>(machine.EndStates |> Set.contains s))
            |> Map.ofList

        allStates
        |> List.iter (
            fun state ->
                let node = allNodes.[state]

                match machine.Edges.TryGetValue state with
                | true, edges ->
                    edges
                    |> List.iter (
                        fun (target, edge) ->
                            let targetNode = allNodes.[target]
                            node.AddEdge(targetNode, edge)
                    )
                | false, _ -> ()
        )

        allNodes.[machine.StartState] :> IDeterministicFiniteStateMachineNode<'C>

    let rec accepts (edgeEvaluator: ('C * 'X) -> bool) input (node: IDeterministicFiniteStateMachineNode<'C>) =
        match input with
        | [] -> node.IsEndState
        | x :: xs ->
            let edgesFrom =
                node.Edges
                |> List.filter (
                    fun (_, e) ->
                        edgeEvaluator(e, x)
                )

            match edgesFrom with
            | [] ->
                false
            | [(t,_)] ->
                t |> accepts edgeEvaluator xs
            | matchingEdges ->
                matchingEdges
                |> List.map snd
                |> failwithf "Found non-disjunct edges from a node: %A"