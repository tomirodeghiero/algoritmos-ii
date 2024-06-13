module directedGraph

sig Node {}

sig Graph {
    nodes: set Node,
    edges: Node -> Node
}

// Predicado para verificar si un grafo es acíclico
pred isAcyclic[g: Graph] {
    all n: g.nodes | not n in n.^(g.edges)
}

// Predicado para verificar si un grafo es completo
pred isComplete[g: Graph] {
    all n1, n2: g.nodes | n1 != n2 implies n1 -> n2 in g.edges
}

// Predicado para verificar si un grafo es conexo (considerando grafo no dirigido)
pred isConnected[g: Graph] {
    all n1, n2: g.nodes | n1 != n2 implies (n1 in n2.^(g.edges + ~(g.edges)))
}

// Predicado para verificar si un grafo es un árbol
pred isTree[g: Graph] {
    isAcyclic[g] and isConnected[g]
}

pred showAcyclic[] {
    some g: Graph | isAcyclic[g]
}

pred showComplete[] {
    some g: Graph | isComplete[g]
}

pred showConnected[] {
    some g: Graph | isConnected[g]
}

pred showTree[] {
    some g: Graph | isTree[g]
}

run showAcyclic for 3 but 3 Node, 1 Graph
run showComplete for 3 but 3 Node, 1 Graph
run showConnected for 3 but 3 Node, 1 Graph
run showTree for 3 but 3 Node, 1 Graph
