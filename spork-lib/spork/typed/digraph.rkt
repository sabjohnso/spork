#lang typed/racket

(provide
 Digraph
 Node
 Path
 make-digraph
 digraph?
 digraph-empty?
 digraph-node?
 digraph-edge?
 digraph-path?
 digraph-has-node?
 digraph-has-edge?
 digraph-has-path?
 digraph-node-count
 digraph-nodes
 digraph-edge-count
 digraph-edges
 digraph-loop-count
 digraph-loops
 empty-digraph
 digraph-add-node
 digraph-add-nodes
 digraph-remove-node
 digraph-remove-nodes
 digraph-add-edge
 digraph-add-edges
 digraph-remove-edge
 digraph-remove-edges
 digraph-add-path
 digraph-add-paths
 digraph-remove-path
 digraph-remove-paths
 digraph-sources
 digraph-targets
 digraph-shortest-path
 digraph-find-cycle
 digraph-cyclic?
 digraph-acyclic?
 digraph-degree-in
 digraph-degree-out
 digraph-degree
 digraph-inputs
 digraph-outputs
 digraph-union
 digraph-intersect
 digraph-difference
 digraph-edge-difference
 digraph-symmetric-difference
 digraph-dual
 digraph-undirected
 digraph-weakly-connected
 digraph-partitions)

(require (prefix-in list: spork/typed/list-extras))

(define-type Node Natural)
(define-predicate digraph-node? Node)

(define-type Edge (Pairof Node Node))
(define-predicate digraph-edge? Edge)

(define-type Path (Listof Node))
(define-predicate digraph-path? Path)


(: digraph-equal-proc (-> Digraph Digraph (-> Any Any Boolean) Boolean))
(define (digraph-equal-proc this other recur)
  (and (recur (digraph-edge-table this)
              (digraph-edge-table other))))

(: digraph-hash-proc (-> Digraph (-> Any Integer)  Integer))
(define (digraph-hash-proc dg recur)
  (recur (digraph-edge-table dg)))

(struct digraph
  ([edge-table : (Immutable-HashTable Node (Setof Node))])
  #:type-name Digraph
  #:property prop:equal+hash
  (list
   digraph-equal-proc
   digraph-hash-proc
   digraph-hash-proc))

(: make-digraph (-> (Listof Path) Digraph))
(define (make-digraph paths)
  (digraph-add-paths empty-digraph paths))

(define empty-digraph
  (digraph (make-immutable-hash)))

(: digraph-empty?  (-> Digraph Boolean))
(define (digraph-empty? dg)
  (hash-empty? (digraph-edge-table dg)))

(: digraph-node-count (-> Digraph Natural))
(define  (digraph-node-count dg)
  (hash-count (digraph-edge-table dg)))

(: digraph-edge-count (-> Digraph Natural))
(define (digraph-edge-count dg)
  (let loop ([target-sets (hash-values (digraph-edge-table dg))]
             [accum 0])
    (match target-sets
      [(list targets target-sets ...)
       (loop target-sets (+ accum (set-count targets)))]
      [(list) (assert accum exact-nonnegative-integer?)])))

(: digraph-loop-count (-> Digraph Natural))
(define (digraph-loop-count dg)
  (length (digraph-loops dg)))

(: digraph-nodes (-> Digraph (Listof Node)))
(define (digraph-nodes dg)
  (hash-keys (digraph-edge-table dg)))


(: digraph-edges (-> Digraph (Listof (Pairof Node Node))))
(define (digraph-edges dg)
  (list:bind (digraph-nodes dg)
        (λ ([src : Node])
          (list:bind (digraph-targets dg src)
                (λ ([tgt : Node])
                  (list (cons src tgt)))))))

(: digraph-targets (-> Digraph Node (Listof Node)))
(define (digraph-targets dg src)
  (let ([edges (digraph-edge-table dg)])
    (if (hash-has-key? edges src)
        (set->list (hash-ref edges src))
      '())))

(: digraph-sources (-> Digraph Node (Listof Node)))
(define (digraph-sources dg tgt)
  (let ([edges (digraph-edge-table dg)])
    (list:bind (digraph-nodes dg)
               (λ ([src : Node])
                 (if (set-member? (hash-ref edges src) tgt)
                     (list src)
                   '())))))

(: digraph-loops (-> Digraph (Listof Node)))
(define (digraph-loops dg)
  (filter (λ ([node : Node]) (digraph-has-edge? dg node node))
          (digraph-nodes dg)))



(: digraph-has-node? (-> Digraph Node Boolean))
(define (digraph-has-node? dg node)
  (hash-has-key? (digraph-edge-table dg) node))


(: digraph-has-edge? (-> Digraph Node Node Boolean))
(define (digraph-has-edge? dg src tgt)
  (and (digraph-has-node? dg src)
       (digraph-has-node? dg tgt)
       (set-member? (hash-ref (digraph-edge-table dg) src) tgt)))

(: digraph-has-path? (-> Digraph Path Boolean))
(define (digraph-has-path? dg path)
  (match path
    [(list src tgt nodes ...)
     (and (digraph-has-edge? dg src tgt)
          (digraph-has-path? dg (cons tgt nodes)))]
    [(list node) (digraph-has-node? dg node)]
    [(list) #t]))

(: digraph-add-node (-> Digraph Node Digraph))
(define (digraph-add-node dg node)
  (let ([edges (digraph-edge-table dg)])
    (if (hash-has-key? edges node) dg
      (digraph (hash-set edges node (ann (set) (Setof Node)))))))

(: digraph-add-nodes (-> Digraph (Listof Node) Digraph))
(define (digraph-add-nodes dg nodes)
  (let loop ([dg dg]
             [nodes nodes])
    (match nodes
      [(list node nodes ...) (loop (digraph-add-node dg node) nodes)]
      [(list) dg])))

(: digraph-add-edge (-> Digraph Node Node Digraph))
(define (digraph-add-edge dg src tgt)
  (let* ([dg (digraph-add-nodes dg (list src tgt))]
         [edges (digraph-edge-table dg)])
    (digraph (hash-set edges src (set-add (hash-ref edges src) tgt)))))


(: digraph-add-edges (-> Digraph (Listof (Pairof Node Node)) Digraph))
(define (digraph-add-edges dg edges)
  (let loop ([dg dg]
             [edges edges])
    (match edges
      [(list (cons src tgt) edges ...) (loop (digraph-add-edge dg src tgt) edges)]
      [(list) dg])))

(: digraph-add-path (-> Digraph Path Digraph))
(define (digraph-add-path dg path)
  (let loop ([dg dg]
             [path path])
    (match path
      [(list src node nodes ...) (loop (digraph-add-edge dg src node) (cons node nodes))]
      [(list node) (digraph-add-node dg node)]
      [(list) dg])))


(: digraph-add-paths (-> Digraph (Listof Path) Digraph))
(define (digraph-add-paths dg paths)
  (let loop ([dg dg]
             [paths paths])
    (match paths
      [(list path paths ...) (loop (digraph-add-path dg path) paths)]
      [(list) dg])))

(: digraph-remove-node (-> Digraph Node Digraph))
(define (digraph-remove-node dg node)
  (let ([edges (hash-remove (digraph-edge-table dg) node)])
    (let loop ([edges : (Immutable-HashTable Node (Setof Node)) edges]
               [srcs (hash-keys edges)])
      (match srcs
        [(list src srcs ...)
         (loop (hash-set edges src (set-remove (hash-ref edges src) node)) srcs)]
        [(list) (digraph edges)]))))

(: digraph-remove-nodes (-> Digraph (Listof Node) Digraph))
(define (digraph-remove-nodes dg nodes)
  (match nodes
    [(list node nodes ...)
     (digraph-remove-nodes (digraph-remove-node dg node) nodes)]
    [(list) dg]))

(: digraph-remove-edge (-> Digraph Node Node Digraph))
(define (digraph-remove-edge dg src tgt)
  (if (digraph-has-edge? dg src tgt)
      (let ([edges (digraph-edge-table dg)])
        (digraph (hash-set edges src (set-remove (hash-ref edges src) tgt))))
    dg))

(: digraph-remove-edges (-> Digraph (Listof (Pairof Node Node)) Digraph))
(define (digraph-remove-edges dg edges)
  (match edges
    [(list (cons src tgt) edges ...)
     (digraph-remove-edges (digraph-remove-edge dg src tgt) edges)]
    [(list) dg]))

(: digraph-remove-path (-> Digraph Path Digraph))
(define (digraph-remove-path dg path)
  (match path
    [(list src tgt nodes ...)
     (digraph-remove-path (digraph-remove-edge dg src tgt) (cons tgt nodes))]
    [(list node) dg]
    [(list) dg]))

(: digraph-remove-paths (-> Digraph (Listof Path) Digraph))
(define (digraph-remove-paths dg paths)
  (match paths
    [(list path paths ...)
     (digraph-remove-paths (digraph-remove-path dg path) paths)]
    [(list) dg]))


(: digraph-shortest-path (-> Digraph Node Node Path))
(define (digraph-shortest-path dg src* tgt*)

  (: target-path? (-> Path Boolean))
  (define (target-path? path)
    (= (car path) tgt*))

  (: extend-path (-> Path (Listof Path)))
  (define (extend-path path)
    (let loop ([tgts (digraph-targets dg (car path))]
               [accum : (Listof Path) '()])
      (match tgts
        [(list tgt tgts ...)
         (loop tgts (cons (cons tgt path) accum))]
        [(list) accum])))

  (: select-paths (-> (Setof Node) (Listof Path) (values (Setof Node) (Listof Path))))
  (define (select-paths used-nodes paths)
    (let loop ([used-nodes used-nodes]
               [paths paths]
               [accum : (Listof Path) '()])
      (match paths
        [(list (list node nodes ...) paths ...)
         (if (set-member? used-nodes node)
             (loop used-nodes paths accum)
           (loop (set-add used-nodes node) paths (cons (cons node nodes) accum)))]
        [(list) (values used-nodes accum)])))

  (let loop ([paths : (Listof Path) (list (list src*))]
             [used-nodes (set src*)])
    (match paths
      [(list path paths ...)
       (let ([tgts (digraph-targets dg (car path))])
         (if (set-member? tgts tgt*) (reverse (cons tgt* path))
           (let-values ([(used-nodes new-paths) (select-paths used-nodes (extend-path path))])
             (loop (append paths new-paths) used-nodes))))]
      [(list) '()])))

(: digraph-find-cycle (-> Digraph Path))
(define (digraph-find-cycle dg)
  (let loop ([nodes (digraph-nodes dg)])
    (match nodes
      [(list node nodes ...)
       (let ([path (digraph-shortest-path dg node node)])
         (if (null? path) (loop nodes)
           path))]
      [(list) '()])))


(: digraph-acyclic? (-> Digraph Boolean))
(define (digraph-acyclic? dg)
  (null? (digraph-find-cycle dg)))

(: digraph-cyclic? (-> Digraph Boolean))
(define (digraph-cyclic? dg)
  (not (digraph-acyclic? dg)))

(: digraph-degree-in (-> Digraph Node Natural))
(define (digraph-degree-in dg tgt)
  (length (digraph-sources dg tgt)))

(: digraph-degree-out (-> Digraph Node Natural))
(define (digraph-degree-out dg src)
  (length (digraph-targets dg src)))

(: digraph-degree (-> Digraph Node Natural))
(define (digraph-degree dg node)
  (+ (digraph-degree-in dg node)
     (digraph-degree-out dg node)))

(: digraph-inputs (-> Digraph (Listof Node)))
(define (digraph-inputs dg)
  (filter (λ ([node : Node]) (zero? (digraph-degree-in dg node)))
          (digraph-nodes dg)))

(: digraph-outputs (-> Digraph (Listof Node)))
(define (digraph-outputs dg)
  (filter (λ ([node : Node]) (zero? (digraph-degree-out dg node)))
          (digraph-nodes dg)))

(: digraph-union (-> Digraph Digraph Digraph))
(define (digraph-union dg1 dg2)
  (let ([nodes (set-union (digraph-nodes dg1) (digraph-nodes dg2))]
        [edges (set-union (digraph-edges dg1) (digraph-edges dg2))])
    (digraph-add-edges (digraph-add-nodes empty-digraph nodes) edges)))

(: digraph-intersect (-> Digraph Digraph Digraph))
(define (digraph-intersect dg1 dg2)
  (let ([nodes (set-intersect (digraph-nodes dg1) (digraph-nodes dg2))]
        [edges (set-intersect (digraph-edges dg1) (digraph-edges dg2))])
    (digraph-add-edges (digraph-add-nodes empty-digraph nodes) edges)))

(: digraph-difference (-> Digraph Digraph Digraph))
(define (digraph-difference dg1 dg2)
  (digraph-remove-nodes dg1 (digraph-nodes dg2)))

(: digraph-edge-difference (-> Digraph Digraph Digraph))
(define (digraph-edge-difference dg1 dg2)
  (digraph-remove-edges dg1 (digraph-edges dg2)))

(: digraph-symmetric-difference (-> Digraph Digraph Digraph))
(define (digraph-symmetric-difference dg1 dg2)
  (digraph-difference (digraph-union dg1 dg2)
                      (digraph-intersect dg1 dg2)))

(: digraph-dual (-> Digraph Digraph))
(define (digraph-dual dg)
  (digraph-add-edges
   (digraph-add-nodes empty-digraph (digraph-nodes dg))
   (map (λ ([edge : (Pairof Node Node)])
          (match-let ([(cons src tgt) edge])
            (cons tgt src)))
        (digraph-edges dg))))

(: digraph-undirected (-> Digraph Digraph))
(define (digraph-undirected dg)
  (digraph-union (digraph-dual dg) dg))

(: digraph-first-node (-> Digraph Node))
(define (digraph-first-node dg)
  (car (digraph-nodes dg)))


(: digraph-strongly-connected (-> Digraph Node Digraph))
(define (digraph-strongly-connected dg node)
  (let loop ([frontier : (Listof Path) (list (list node))]
             [connected-nodes : (Listof Node) (list node)])
    (match frontier
      [(list (list src nodes ...) paths ...)
       (let ([new-nodes
              (filter (λ ([tgt : Node]) (not (set-member? connected-nodes tgt)))
                      (digraph-targets dg src))])
         (loop (append paths (map (λ ([tgt : Node]) (list* tgt src nodes)) new-nodes))
               (append connected-nodes new-nodes)))]
      [(list) (digraph-remove-nodes dg (set-subtract (digraph-nodes dg) connected-nodes))])))

(: digraph-weakly-connected (-> Digraph Node Digraph))
(define (digraph-weakly-connected dg node)
  (let ([g (digraph-undirected dg)])
    (digraph-intersect dg (digraph-strongly-connected g node))))


(: digraph-partitions (-> Digraph (Listof Digraph)))
(define (digraph-partitions dg)
  (let loop ([dg dg]
             [accum : (Listof Digraph) (list)])
    (if (digraph-empty? dg) accum
      (let ([partition (digraph-weakly-connected dg (digraph-first-node dg))])
        (loop (digraph-difference dg partition)
              (cons (digraph-intersect dg partition) accum))))))
