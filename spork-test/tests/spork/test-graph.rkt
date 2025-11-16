#lang racket

(module+ test
  (require
   rackunit rackunit/spec
   spork/typed/graph)

  (describe "edge"
    (it "constructs an unordered pair of nodes"
      (check-equal?
       (edge 0 1)
       (edge 1 0))))

  (describe "empty-graph"
    (it "is a graph"
      (check-true (graph? empty-graph)))
    (it "is empty?"
      (check-true (graph-empty? empty-graph))))

  (describe "graph-add-node"
    (define g (graph-add-node empty-graph 0))
    (it "returns a graph like the input graph with the addition of the input node"
      (check-equal? (graph-nodes g) '(0))
      (check-equal? (graph-edges g) '()))
    (it "returns the input graph when it already contains the node"
      (check-equal? (graph-add-node g 0) g)))

  (describe "graph-add-nodes"
    (define g (graph-add-nodes empty-graph '(0 1)))
    (it "is like graph-add-node but adds a list of nodes"
      (check-equal? (graph-nodes g) '(0 1))))

  (describe "graph-add-edge"
    (define g (graph-add-edge empty-graph 0 1))
    (it "adds the input edge to the input graph"
      (check-equal? (graph-edges g) (list (edge 0 1))))

    (it "doesn't matter what order the nodes for the input edge"
      (check-equal? (graph-add-edge empty-graph 1 0) g))

    (it "returns the input graph when it already has the input edge"
      (check-equal? (graph-add-edge g 0 1) g)
      (check-equal? (graph-add-edge g 1 0) g))))
