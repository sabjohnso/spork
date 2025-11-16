#lang racket

(module+ test
  (require
   rackunit rackunit/spec
   spork/typed/digraph)

  (describe "digraph?"
    (it "is a type predicate that recognizes digraphs"
      (check-true (digraph? empty-digraph)))

    (it "does not recognize other types of data"
      (check-false (digraph? 42))))

  (describe "digraph-empty?"
    (it "is a predicate for digraphs that recognizes digraphs that don't have any data"
      (check-true (digraph-empty? empty-digraph)))

    (it "does not recognize digraphs that have edges"
      (check-false (digraph-empty? (digraph-add-edge empty-digraph 0 1))))

    (it "does not recognize digraphs that have nodes"
      (check-false (digraph-empty? (digraph-add-node empty-digraph 0)))))

  (describe "digraph-node?"
    (it "is a predicate that recognizes digraph nodes (natural-numbers)."
      (check-true (digraph-node? 3)))

    (it "does not recognize values of other types"
      (check-false (digraph-node? 3.2))
      (check-false (digraph-node? -1))
      (check-false (digraph-node? "something completely different"))))

  (describe "digraph-path?"
    (it "is a predicate that recognizes digraph paths (Listof Node)"
      (check-true (digraph-path? (list 0 1 2))))
    (it "does not recognize values of other types"
      (check-false (digraph-path? (list "banana")))))

  (describe "digraph-has-node?"
    (it "is a binary relation for a digraph and a node that includes inputs where the digraph contains the node"
      (check-true (digraph-has-node? (digraph-add-node empty-digraph 0) 0)))

    (it "does not include inputs where the digraph does not contain the node"
      (check-false (digraph-has-node? empty-digraph 0))))

  (describe "digraph-has-edge?"
    (it "is a ternary relation for a digraph and two nodes that includes inputs where the digraph contains the edge from the first to second node"
      (check-true (digraph-has-edge? (digraph-add-edge empty-digraph 0 1) 0 1)))

    (it "does not include inputs where the digraph does not contain the edge"
      (check-false (digraph-has-edge? empty-digraph 0 1))))

  (describe "digraph-node-count"
    (it "is a digraph observer that returns the number of nodes in a digraph"
      (check-equal? (digraph-node-count (digraph-add-nodes empty-digraph '(0 1 2))) 3)))

  (describe "digraph-edge-count"
    (it "is a digraph observer that returns the number of edges in a digraph"
      (check-equal? (digraph-edge-count (digraph-add-edges empty-digraph '((0 . 1) (2 . 3)))) 2)))

  (describe "empty-digraph"
    (it "is a digraph"
      (check-true (digraph? empty-digraph)))

    (it "is empty"
      (check-true (digraph-empty? empty-digraph)))

    (it "has zero nodes"
      (check-equal? (digraph-node-count empty-digraph) 0))

    (it "has zero egdes"
      (check-equal? (digraph-edge-count empty-digraph) 0))

    (it "is equal to itself"
      (check-equal? empty-digraph empty-digraph)))

  (describe "digraph-add-node"
    (define node 0)
    (define dg (digraph-add-node empty-digraph node))

    (it "accepts a digraph and returns a digraph"
      (check-true (digraph? dg)))

    (it "returns a nonempty digraph"
      (check-false (digraph-empty? dg)))

    (it "returns a digraph that has an additional node"
      (check-equal? (digraph-node-count dg) 1))

    (it "returns a digraph without any additional edges"
      (check-equal? (digraph-edge-count dg) 0))

    (it "returns a digraph that contains the input node"
      (check-true (digraph-has-node? dg node)))

    (it "will return the input digraph if it already has the input node"
      (check-equal? (digraph-add-node dg node) dg)))

  (describe "digraph-add-nodes"
    (define node1 1)
    (define node2 2)
    (define dg (digraph-add-nodes empty-digraph (list node1 node2)))

    (it "accepts a digraph and a list of nodes, returning a digraph"
      (check-true (digraph? dg)))

    (it "returns a nonempty digraph"
      (check-false (digraph-empty? dg)))

    (it "returns a digraph with each of the nodes in the input list"
      (check-equal? (digraph-node-count dg) 2)
      (check-true (digraph-has-node? dg node1))
      (check-true (digraph-has-node? dg node2)))

    (it "does not add any edges"
      (check-equal? (digraph-edge-count dg) 0)))

  (describe "digraph-add-edge"
    (define src 1) ;; source
    (define tgt 2) ;; target
    (define dg (digraph-add-edge empty-digraph src tgt))

    (it "accepts a digraph and two nodes (source and target), and returns a digraph"
      (check-true (digraph? dg)))

    (it "returns a digraph that is not empty?"
      (check-false (digraph-empty? dg)))

    (it "returns a digraph that has two additional nodes"
      (check-equal? (digraph-node-count dg) 2))

    (it "returns a digraph that has one additional edge"
      (check-equal? (digraph-edge-count dg) 1))

    (it "returns a digraph that has the source and target nodes"
      (check-true (digraph-has-node? dg src))
      (check-true (digraph-has-node? dg tgt)))

    (it "returns a digraph that has and edge from source to target"
      (check-true (digraph-has-edge? dg src tgt)))

    (it "returns the input digraph if it already contains the edge"
      (check-equal? (digraph-add-edge dg src tgt) dg)))

  (describe "digraph-add-edges"
    (define node1 1)
    (define node2 2)
    (define node3 3)
    (define dg (digraph-add-edges empty-digraph (list (cons node1 node2) (cons node2 node3))))

    (it "accepts a digraph and a list of pairs of nodes and returns a digraph"
      (check-false (digraph-empty? dg)))

    (it "returns a digraph that has the additional nodes for every edge"
      (check-equal? (digraph-node-count dg) 3)
      (check-true (digraph-has-node? dg node1))
      (check-true (digraph-has-node? dg node2))
      (check-true (digraph-has-node? dg node3)))

    (it "returns a digraph that has the input edges"
      (check-equal? (digraph-edge-count dg) 2)
      (check-true (digraph-has-edge? dg node1 node2))
      (check-true (digraph-has-edge? dg node2 node3)))

    (it "returns the input digraph unaltered when the digraph already contains the input eges"
      (check-equal? (digraph-add-edges dg (list (cons node1 node2) (cons node2 node3))) dg)))

  (describe "digraph-add-path"
    (define node1 1)
    (define node2 2)
    (define node3 3)
    (define path (list node1 node2 node3))
    (define dg (digraph-add-path empty-digraph path))

    (it "accepts a digraph and a list of nodes defining a path and returns a digraph?"
      (check-true (digraph? dg)))

    (it "returns a digraph with each of the nodes in the input path"
      (check-equal? (digraph-node-count dg) (length path))
      (check-true (digraph-has-node? dg node1))
      (check-true (digraph-has-node? dg node2))
      (check-true (digraph-has-node? dg node3)))

    (it "returns a digraph with edges between each adjacent pair of nodes in the path"
      (check-equal? (digraph-edge-count dg) 2)
      (check-true (digraph-has-edge? dg node1 node2))
      (check-true (digraph-has-edge? dg node2 node3)))


    (it "does not add anything if the input path is empty"
      (check-equal? empty-digraph (digraph-add-path empty-digraph '())))

    (it "will only add a node if the input path only has one element"
      (check-equal? (digraph-node-count (digraph-add-path empty-digraph '(1))) 1)))

  (describe "digraph-add-paths"
    (define node1 1)
    (define node2 2)
    (define node3 3)
    (define path1 (list node1 node2 node3))
    (define path2 (list node3 node1))
    (define dg (digraph-add-paths
                empty-digraph
                (list path1 path2)))
    (it "accepts a digraph and a list of paths and it returns a digraph"
      (check-true (digraph? dg)))

    (it "returns a digraph with each of the nodes in each of the input paths"
      (check-equal? (digraph-node-count dg) 3)
      (check-true (digraph-has-node? dg node1))
      (check-true (digraph-has-node? dg node2))
      (check-true (digraph-has-node? dg node3)))

    (it "returns a digraph with edges between each adjacent pair of nodes in the path"
      (check-equal? (digraph-edge-count dg) 3)
      (check-true (digraph-has-edge? dg node1 node2))
      (check-true (digraph-has-edge? dg node2 node3))
      (check-true (digraph-has-edge? dg node3 node1))))


  (describe "digraph-remove-node"
    (define node1 1)
    (define node2 2)
    (define node3 3)
    (define input-digraph (digraph-add-path empty-digraph (list node1 node2 node3)))
    (define output-digraph (digraph-remove-node input-digraph node2))

    (it "accepts a digraph and a node and returns a digraph"
      (check-true (digraph? output-digraph)))

    (it "returns a digraph that does not contain the removed node"
      (check-false (digraph-has-node? output-digraph node2)))

    (it "returns a digraph with all edges associated with the node removed"
      (check-equal? (digraph-edge-count input-digraph) 2)
      (check-equal? (digraph-edge-count output-digraph) 0))

    (it "returns the input digraph when it does not contain the node"
      (check-equal? (digraph-remove-node empty-digraph node1) empty-digraph)))

  (describe "digraph-remove-nodes"
    (define node1 1)
    (define node2 2)
    (define node3 3)
    (define input-digraph (digraph-add-path empty-digraph (list node1 node2 node3)))
    (define output-digraph (digraph-remove-nodes input-digraph (list node1 node2)))

    (it "removes the input nodes from the digraph"
      (check-false (digraph-has-node? output-digraph node1))
      (check-false (digraph-has-node? output-digraph node2))))

  (describe "digraph-remove-edge"
    (define node1 1)
    (define node2 2)
    (define node3 3)
    (define input-digraph (digraph-add-path empty-digraph (list node1 node2 node3)))
    (define output-digraph (digraph-remove-edge input-digraph node1 node2))

    (it "accepts a digraph and two nodes defining a directed edge and returns a digraph"
      (check-true (digraph? output-digraph)))

    (it "returns a digraph that is equivalent to the input digraph without the indicated edge"
      (check-true (digraph-has-edge? input-digraph node1 node2))
      (check-false (digraph-has-edge? output-digraph node1 node2))))

  (describe "digraph-targets"
    (define node1 1)
    (define node2 2)
    (define node3 3)
    (define node4 4)
    (define dg (digraph-add-paths
                empty-digraph
                (list (list node1 node2 node3)
                      (list node2 node4))))

    (define targets (digraph-targets dg node2))
    (check-equal? (length targets) 2))

  (describe "digraph-sources"
    (define node1 1)
    (define node2 2)
    (define node3 3)
    (define node4 4)

    (define dg (make-digraph
                (list (list node1 node2 node3)
                      (list node4 node2))))
    (define sources (list->set (digraph-sources dg node2)))
    (check-equal? sources (set node1 node4)))

  (describe "digraph-shortest-path"
    (define dg (make-digraph
                (list (list 0 1 2 3)
                      (list 4 5 6 7)
                      (list 8 5 2))))

    (check-equal? (digraph-shortest-path dg 0 3) '(0 1 2 3))
    (check-equal? (digraph-shortest-path dg 4 7) '(4 5 6 7))
    (check-equal? (digraph-shortest-path dg 8 3) '(8 5 2 3))
    (check-equal? (digraph-shortest-path dg 8 7) '(8 5 6 7))
    (check-equal? (digraph-shortest-path dg 8 0) '())
    (check-equal? (digraph-shortest-path dg 11 12) '()))

  (describe "digraph-find-cycle"
    (it "returns a path for cyclic digraphs"
      (define dg
        (make-digraph
         (list (list 0 1 2 3)
               (list 3 5 6 0)
               (list 8 5 2))))
      (define cycle (digraph-find-cycle dg))
      (check-true (not (null? cycle)))
      (check-true (digraph-has-path? dg cycle))
      (check-true (digraph-cyclic? dg))
      (check-false (digraph-acyclic? dg)))

    (it "returns an empty path for acyclic digraphs"
      (define dg
        (make-digraph
         (list (list 0 1 2 3)
               (list 4 5 6 7)
               (list 8 5 2))))
      (define cycle (digraph-find-cycle dg))
      (check-true (null? cycle))
      (check-true (digraph-acyclic? dg))
      (check-false (digraph-cyclic? dg))))

  (context "with a defined digraph"
    (define dg
      (make-digraph
       (list (list 0 1 2 3)
             (list 4 5 6 7)
             (list 8 5 2))))
    (describe "digraph-degree-in"
      (it "returns the inward degree of a node in a digraph"
        (check-equal? (digraph-degree-in dg 0) 0)
        (check-equal? (digraph-degree-in dg 5) 2)
        (check-equal? (digraph-degree-in dg 3) 1)))

    (describe "digraph-degree-out"
      (it "returns the outward degree of a node in a digraph"
        (check-equal? (digraph-degree-out dg 0) 1)
        (check-equal? (digraph-degree-out dg 5) 2)
        (check-equal? (digraph-degree-out dg 3) 0)))

    (describe "digraph-degree"
      (it "returns the total degree (inward and outward) for a node in a digraph"
        (check-equal? (digraph-degree dg 0) 1)
        (check-equal? (digraph-degree dg 5) 4)
        (check-equal? (digraph-degree dg 3) 1)))

    (describe "digraph-inputs"
      (it "returns a list of input nodes for a digraph"
        (check-equal? (list->set (digraph-inputs dg)) (list->set '(0 4 8)))))

    (describe "digraph-outputs"
      (it "returns a list of output nodes for a digraph"
        (check-equal? (list->set (digraph-outputs dg)) (list->set '(3 7))))))

  (context "with two digraphs defined"
    (define dg1 (make-digraph '((0 1 2 3))))
    (define dg2 (make-digraph '((4 1 2 5))))

    (describe "digraph-union"
      (it "returns a digraph with all nodes and edges in either input digraph"
        (check-equal?
         (digraph-union dg1 dg2)
         (make-digraph
          '((0 1 2 3)
            (4 1 2 5))))))

    (describe "digraph-intersect"
      (it "returns a digraph with all nodes and edges in shared by both input digraphs"
        (check-equal?
         (digraph-intersect dg1 dg2)
         (make-digraph
          '((1 2))))))

    (describe "digraph-difference"
      (it "returns a digraph like the first input digraph with all nodes shared with the second digraph removed"
        (check-equal?
         (digraph-difference dg1 dg2)
         (make-digraph
          '((0) (3))))))

    (describe "digraph-edge-difference"
      (it "returns a digraph like the first input digraph with all the edges of the second digraph removed"
        (check-equal?
         (digraph-edge-difference dg1 dg2)
         (make-digraph
          '((0 1) (2 3))))))

    (describe "digraph-symmetric-difference"
      (it "return a digraph with all nodes and edges that are not common between the input digraphs"
        (check-equal?
         (digraph-symmetric-difference dg1 dg2)
         (make-digraph
          '((0) (3) (4) (5)))))))


  (describe "digraph-dual"
    (it "returns the dual of the input digraph"
      (check-equal?
       (digraph-dual
        (make-digraph
         '((0 1 2 3)
           (4 5 6 7)
           (8 5 2))))
       (make-digraph
         '((3 2 1 0)
           (7 6 5 4)
           (2 5 8))))))

  (describe "digraph-undirected"
    (it "returns a directed graph with all edges having a reverse "
      (check-equal?
       (digraph-undirected
        (make-digraph
         '((0 1 2 3)
           (4 5 6 7)
           (8 5 2))))
       (make-digraph
        '((0 1 2 3)
          (4 5 6 7)
          (8 5 2)
          (2 5 8)
          (7 6 5 4)
          (3 2 1 0))))))

  (describe "digraph-partitions"
    (it "splits digraphs into weakly connected partitions"
      (let ([parts (digraph-partitions
                    (make-digraph
                     '((0 1 2 3)
                       (4 5 6 7))))])
        (check-true (set-member? parts (make-digraph '((0 1 2 3)))))
        (check-true (set-member? parts (make-digraph '((4 5 6 7)))))))))
