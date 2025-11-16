#lang typed/racket

(provide
  Node
  Edge
  Graph
  graph?
  graph-empty?
  make-graph
  digraph->graph
  graph->digraph
  empty-graph
  (struct-out edge)


  graph-add-node
  graph-add-nodes
  graph-node-count
  graph-nodes
  graph-has-node?

  graph-add-edge
  graph-add-edges
  graph-edge-count
  graph-edges
  graph-has-edge?

  graph-add-path
  graph-add-paths
  graph-has-path?

  graph-partitions
  graph-connected?

  graph-shortest-path)

(require
 spork/typed/digraph)

(: graph-equal-proc (-> Graph Graph (-> Any Any Boolean) Boolean))
(define (graph-equal-proc g1 g2 recur)
  (recur (graph-data g1) (graph-data g2)))

(: graph-hash-proc (-> Graph (-> Any Integer) Integer))
(define (graph-hash-proc g recur)
  (recur (graph-data g)))

(struct graph
  ([data : Digraph])
  #:type-name Graph
  #:property prop:equal+hash
  (list
   graph-equal-proc
   graph-hash-proc
   graph-hash-proc))

(: graph-empty? (-> Graph Boolean))
(define (graph-empty? g)
  (digraph-empty? (graph-data g)))

(: make-graph (-> (Listof Path) Graph))
(define (make-graph paths)
  (graph-add-paths empty-graph paths))

(: digraph->graph (-> Digraph Graph))
(define (digraph->graph dg)
  (graph (digraph-undirected dg)))

(: graph->digraph (-> Graph Digraph))
(define (graph->digraph g)
  (graph-data g))

(define empty-graph
  (graph empty-digraph))

(: graph-add-node (-> Graph Node Graph))
(define (graph-add-node g node)
  (graph (digraph-add-node (graph-data g) node)))

(: graph-add-nodes (-> Graph (Listof Node) Graph))
(define (graph-add-nodes g nodes)
  (match nodes
    [(list node nodes ...) (graph-add-nodes (graph-add-node g node) nodes)]
    [(list) g]))

(: graph-add-edge (-> Graph Node Node Graph))
(define (graph-add-edge g left right)
  (graph (digraph-add-edges (graph-data g) (list (cons left right) (cons right left)))))

(: graph-add-edges (-> Graph (Listof (Pairof Node Node)) Graph))
(define (graph-add-edges g edges)
  (match edges
    [(list (cons left right) edges ...) (graph-add-edges (graph-add-edge g left right) edges)]
    [(list) g]))

(: graph-add-path (-> Graph Path Graph))
(define (graph-add-path g path)
  (match path
    [(list node1 node2 nodes ...)
     (graph-add-path (graph-add-edge g node1 node2) (cons node2 nodes))]
    [(list node) (graph-add-node g node)]
    [(list) g]))

(: graph-add-paths (-> Graph (Listof Path) Graph))
(define (graph-add-paths g paths)
  (match paths
    [(list path paths ...)
     (graph-add-paths (graph-add-path g path) paths)]
    [(list) g]))

(: edge-equal-proc (-> Edge Edge (-> Any Any Boolean) Boolean))
(define (edge-equal-proc edge1 edge2 recur)
  (recur (edge->pair edge1)
         (edge->pair edge2)))

(: edge-hash-proc (-> Edge (-> Any Integer) Integer))
(define (edge-hash-proc edge recur)
  (recur (edge->pair edge)))

(struct edge
  ([left : Node]
   [right : Node])
  #:type-name Edge
  #:property prop:equal+hash
  (list edge-equal-proc
        edge-hash-proc
        edge-hash-proc))

(: edge->pair (-> Edge (Pairof Node Node)))
(define (edge->pair e)
  (match-let ([(edge left right) e])
    (cons (min left right) (max left right))))

(: pair->edge (-> (Pairof Node Node) Edge))
(define (pair->edge pair)
  (edge (car pair) (cdr pair)))

(: graph-nodes (-> Graph (Listof Node)))
(define (graph-nodes g)
  (digraph-nodes (graph-data g)))

(: graph-edges (-> Graph (Listof Edge)))
(define (graph-edges g)
  (set->list (list->set (map pair->edge (digraph-edges (graph-data g))))))

(: graph-loops (-> Graph (Listof Node)))
(define (graph-loops g)
  (digraph-loops (graph-data g)))

(: graph-has-node? (-> Graph Node Boolean))
(define (graph-has-node? g node)
  (digraph-has-node? (graph-data g) node))

(: graph-has-edge? (-> Graph Node Node Boolean))
(define (graph-has-edge? g left right)
  (digraph-has-edge? (graph-data g) left right))

(: graph-has-path? (-> Graph Path Boolean))
(define (graph-has-path? g path)
  (match path
    [(list node1 node2 nodes ...)
     (and (graph-has-edge? g node1 node2)
          (graph-has-path? g (cons node2 nodes)))]
    [(list node) (graph-has-node? g node)]
    [(list) #t]))

(: graph-node-count (-> Graph Natural))
(define (graph-node-count g)
  (length (graph-nodes g)))

(: graph-edge-count (-> Graph Natural))
(define (graph-edge-count g)
  (length (graph-edges g)))

(: graph-loop-count (-> Graph Natural))
(define (graph-loop-count g)
  (length (graph-loops g)))

(: graph-partitions (-> Graph (Listof Graph)))
(define (graph-partitions g)
  (map graph (digraph-partitions (graph-data g))))

(: graph-connected? (-> Graph Boolean))
(define (graph-connected? g)
  (= (length (graph-partitions g)) 1))

(: graph-shortest-path (-> Graph Node Node Path))
(define (graph-shortest-path g node1 node2)
  (digraph-shortest-path (graph-data g) node1 node2))

(: graph-union (-> Graph Graph Graph))
(define (graph-union g1 g2)
  (graph (digraph-union (graph-data g1) (graph-data g2))))

(: graph-intersect (-> Graph Graph Graph))
(define (graph-intersect g1 g2)
  (graph (digraph-intersect (graph-data g1) (graph-data g2))))

(: graph-difference (-> Graph Graph Graph))
(define (graph-difference g1 g2)
  (graph (digraph-difference (graph-data g1) (graph-data g2))))

(: graph-symmetric-difference (-> Graph Graph Graph))
(define (graph-symmetric-difference g1 g2)
  (graph (digraph-symmetric-difference (graph-data g1) (graph-data g2))))
