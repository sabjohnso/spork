#lang racket



(module+ test
  (require
   rackunit rackunit/spec
   spork/parst spork/functor)

  (describe "primitive parsers"

    (describe "parser-return"
      (it "accepts a value and returns a parser"
        (define value 'x)
        (define parser (parser-return value))
        (check-true (parser? parser))
        (check-true (parser-return? parser))
        (it "always succeeds"
          (context "with arbitrary input"
            (define input '(a b))
            (define results (parse parser input))
            (it "succeeds"
              (check-true (parse-success? results)))
            (it "returns the value it holds"
              (check-equal? (parse-result-value (stream-first results)) value))
            (it "does not consume any input"
              (check-equal? (parse-result-remaining-input (stream-first results)) input))))))

    (describe "parse-fail"
      (it "does not accept any inputs constructs a primitive parser "
        (define parser (parser-fail))
        (check-true (parser-fail? parser))
        (check-true (parser? parser))
        (it "never succeeds"
          (context "with arbitrary input and results from parsing the input"
            (define input '(a b))
            (define results (parse parser input))
            (it "failed"
              (check-true (parse-failure? results)))))))

    (describe "parser-item"
      (it "does not accept any inputs and constructs a primitive parser"
        (define parser (parser-item))
        (check-true (parser-item? parser))
        (check-true (parser? parser))
        (context "with arbitrary nonempty input"
          (define input '(a b))
          (define results (parse parser input))
          (it "succeeds"
            (check-true (parse-success? results)))
          (it "returns one result on success"
            (check-equal? (stream-length results) 1))
          (it "consumes the first value in the input"
            (check-equal?
             (parse-result-remaining-input (stream-first results))
             '(b))))
        (context "with empty input"
          (define input '())
          (define results (parse parser input))
          (it "fails"
            (check-true
             (parse-failure? results))))))

    (describe "parser-empty"
      (it "accepts an input value and constructs a primitive parser"
        (define parser (parser-empty 'x))
        (check-true (parser-empty? parser))
        (check-true (parser? parser))
        (context "with empty input"
          (define input '())
          (define results (parse parser input))
          (it "it succeeds"
            (check-true (parse-success? results))))
        (context "with arbitrary nonempty input"
          (define input '(a b))
          (define results (parse parser input))
          (it "fails"
            (check-true (parse-failure? results))))))

    (describe "parser-disj"
      (it "is a primitive parser combinator accepting two parsers"
        (context "a parser-disj constructed with two parsers that will always succeed"
          (define parser (parser-disj (parser-return 'x) (parser-return 'y)))
          (check-true (parser-disj? parser))
          (check-true (parser? parser))
          (context "with arbitrary input"
            (define input '(a b))
            (define results (parse parser input))
            (it "will succeed"
              (check-true (parse-success? results)))
            (it "will return results for each of the parsers it holds"
              (check-equal? (stream-length results) 2)
              (check-equal? (parse-result-value (stream-first results)) 'x)
              (check-equal? (parse-result-value (stream-first (stream-rest results))) 'y))))

        (context "a parser-disj constructed with parser-item and parser-empty parsers"
          (define parser (parser-disj (parser-item) (parser-empty 'y)))
          (context "with arbitrary nonempty input"
            (define input '(x))
            (define results (parse parser input))
            (it "will still succeed, even tough the second parser failed"
              (check-true (parse-success? results))
              (check-equal? (stream-length results) 1))
            (it "will return the value from the successful parser"
              (check-equal? (parse-result-value (stream-first results)) 'x)))

          (context "with empty input"
            (define input '())
            (define results (parse parser input))
            (it "will still succeed even though the first parser failed"
              (check-true (parse-success? results))
              (check-equal? (stream-length results) 1))
            (it "will return the value returned by the second parser"
              (check-equal? (parse-result-value (stream-first results)) 'y))))

        (context "a parser-disj constructed with parser-fail parsers"
          (define parser (parser-disj (parser-fail) (parser-fail)))
          (context "with arbitrary input"
            (define input '(a b))
            (define results (parse parser input))
            (it "will fail because bother parsers it holds fail"
              (check-true (parse-failure? results))))))

      (it "will only accept parsers for the first input"
        (check-exn exn:fail? (thunk (parser-disj 'thunder-cat (parser-return 'x)))))
      (it "will only accept parsers for the second input"
        (check-exn exn:fail? (thunk (parser-disj (parser-item) "a pink elefant")))))

    (describe "parser-alt"
      (it "is a primitive parser combinator accepting two parsers"
        (context "a parser-alt constructed from two parsers that always succeed"
          (define parser (parser-alt (parser-return 'x) (parser-return 'y)))
          (context "with arbitrary input"
            (define input '(a b))
            (define results (parse parser input))
            (it "will succed"
              (check-true (parse-success? results)))
            (it "will only have the results from the first parser"
              (check-equal? (stream-length results) 1)
              (check-equal? (parse-result-value (stream-first results)) 'x))))
        (context "a parser-alt constructed from parser-fail and parser-return parsers"
          (define parser (parser-alt (parser-fail) (parser-return 'x)))
          (context "with arbitary input"
            (define input '(a b))
            (define results (parse parser input))
            (it "will succeed"
              (check-true (parse-success? results)))
            (it "will have the results from the second parser"
              (check-equal? (stream-length results) 1)
              (check-equal? (parse-result-value (stream-first results)) 'x))))
        (context "a parser-alt constructed from from two parser-fail parsers"
          (define parser (parser-alt (parser-fail) (parser-fail)))
          (context "with arbitrary input"
            (define input '(a b))
            (define results (parse parser input))
            (it "will fail because both parsers faild"
              (check-true (parse-failure? results))))))

      (it "will only accept parsers for the first input"
        (check-exn exn:fail? (thunk (parser-disj 'thunder-cat (parser-return 'x)))))
      (it "will only accept parsers for the second input"
        (check-exn exn:fail? (thunk (parser-disj (parser-item) "a pink elefant")))))

    (describe "parser-bind"
      (it "is a parser combinator constructing a parser from a parser and a parser constructor"
        (context "a parser-bind constructred from a parser-item parser and the parser-empty constructor"
          (define parser (parser-bind (parser-item) parser-empty))
          (context "given input with one item"
            (define input '(x))
            (define results (parse parser input))
            (it "will successfully parser the item"
              (check-true (parse-success? results)))
            (it "will return the item consumed from the input"
              (check-equal? (stream-length results) 1)
              (check-equal? (parse-result-value (stream-first results)) 'x)
              (check-true (stream-empty? (parse-result-remaining-input (stream-first results))))))))
      (it "only accepts parsers as the first argument"
        (check-exn exn:fail? (thunk (parser-bind "I'm not a parser!" parser-return))))
      (it "only accepts unary procedures as the second argument"
        (check-exn exn:fail? (thunk (parser-bind (parser-item) (λ (x y) (parser-item))))))
      (it "only accepts unary procedures returning a parser as the second argument at parse time"
        (check-exn exn:fail? (thunk (parse (parser-bind (parser-item) (λ (x) 'bannanas))
                                           '(a b))))))

    (describe "parser-push"
      (it "accepts a value and returns a parser"
        (define parser (parser-push 'x))
        (context "given arbitrary input"
          (define input '(a b))
          (define results (parse parser input))
          (it "always succeeds"
            (check-true (parse-success? results))
            (check-equal? (stream-length results) 1))
          (it "returns the value it holds"
            (check-equal? (parse-result-value (stream-first results)) 'x))
          (it "also pushes the value onto the input"
            (check-equal? (stream->list (parse-result-remaining-input (stream-first results)))
                          '(x a b))))))

    (describe "parser-undo"
      (it "accepts a parser and returns a parser"
        (define parser (parser-peek (parser-item)))
        (context "with arbitrary nonempty input"
          (define input '(a b))
          (define results (parse parser input))
          (it "will succeed with one result"
            (check-true (parse-success? results))
            (check-equal? (stream-length results) 1))
          (it "will return the first value in th input"
            (check-equal? (parse-result-value (stream-first results)) 'a))
          (it "will not consume any input"
            (check-equal? (parse-result-remaining-input (stream-first results)) input))))))

  (describe "compound parsers"
    (describe "parser-disj*"
      (it "is a variadic version of parser-disj"
        (define parser
          (parser-disj*
           (parser-return 'x)
           (parser-return 'y)
           (parser-return 'z)))
        (define input '(a b))
        (define results (parse parser input))
        (check-equal? (stream-length results) 3)))

    (describe "parser-alt*"
      (it "is a variadic version of parser-alt"
        (define parser
          (parser-alt*
           (parser-return 'x)
           (parser-return 'y)
           (parser-return 'z)))
        (define input '(a b))
        (define results (parse parser input))
        (check-equal? (stream-length results) 1)))

    (describe "parser-sat"
      (it "accepts a predicate and constructs a parser"
        (context "a pareser-sat parser constructed with symbol?"
          (define parser (parser-sat symbol?))
          (context "input with a leading symbol"
            (define input '(a b))
            (define results (parse parser input))
            (it "succeeds"
              (check-true (parse-success? results)))
            (it "returns the symbol it consumed from the input"
              (check-true (parse-success? results))
              (check-equal? (stream-length results) 1)
              (check-equal? (parse-result-value (stream-first results)) 'a)))
          (context "given input with something else as the first item"
            (define input '(3 b))
            (define results (parse parser input))
            (it "fails because the predicate was not satisfied"
              (check-true (parse-failure? results))))
          (context "given empty input"
            (define input '())
            (define results (parse parser input))
            (it "fails because the input was empty"
              (check-true (parse-failure? results)))))))

    (describe "parser-optional-nongreedy"
      (context "with a parser-optional-nongreedy for a symbol"
        (define parser (parser-optional-nongreedy (parser-sat symbol?) 'x))
        (context "with input with a leading symbol"
          (define input '(a 2))
          (define results (parse parser input))
          (it "will succeed with two results"
            (check-true (parse-success? results))
            (check-equal? (stream-length results) 2))
          (context "with the set of value returned"
            (define result-values (parse-results-value-set results))
            (it "will have the leading symbol from the input"
              (check-true (set-member? result-values 'a)))
            (it "will have the default value from the parser"
              (check-true (set-member? result-values 'x)))))))

    (describe "parser-optional-greedy"
      (context "with aparser-optional-greedy for a symbol"
        (define parser (parser-optional-greedy (parser-sat symbol?) 'x))
        (context "with input with a leading symbol"
          (define input '(a 2))
          (define results (parse parser input))
          (it "will succeed with one result"
            (check-true (parse-success? results))
            (check-equal? (stream-length results) 1))
          (it "will have the leading symbol as the value"
            (check-equal? (parse-result-value (stream-first results)) 'a)))
        (context "with input that does not have a leading symbol"
          (define input '(1 2))
          (define results (parse parser input))
          (it "succeed with one result"
            (check-true (parse-success? results))
            (check-equal? (stream-length results) 1))
          (it "will have the default value from the parser"
            (check-equal? (parse-result-value (stream-first results)) 'x)))))

    (describe "parser-zero-or-more-nongreedy"
      (context "with a nongreedy parser matching zero or more symbols"
        (define parser (parser-zero-or-more-nongreedy (parser-sat symbol?)))
        (context "with arbitrary input"
          (define input '(a b c))
          (define results (parse parser input))
          (it "will succeed with three results"
            (check-true (parse-success? results))
            (check-equal? (stream-length results) (add1 (length input))))
          (it "will include results of each length upto the length of the input"
            (check-equal? (parse-results-value-set results)
                          (set '()
                               '(a)
                               '(a b)
                               '(a b c)))))))

    (describe "parser-zero-or-more-greedy"
      (context "with a nongreedy parser matching zero or more symbols"
        (define parser (parser-zero-or-more-greedy (parser-sat symbol?)))
        (context "with arbitrary input"
          (define input '(a b c))
          (define results (parse parser input))
          (it "will succeed with 1 result"
            (check-true (parse-success? results))
            (check-equal? (stream-length results) 1))
          (it "will include the full input in the result value"
            (check-equal? (parse-result-value (stream-first results))
                          '(a b c))))))


    (describe "parser-one-or-more-nongreedy"
      (context "with a nongreedy parser matching one or more symbols"
        (define parser (parser-one-or-more-nongreedy (parser-sat symbol?)))
        (context "with arbitrary input"
          (define input '(a b c))
          (define results (parse parser input))
          (it "will succeed with three results"
            (check-true (parse-success? results))
            (check-equal? (stream-length results) (length input)))
          (it "will include results of each length starting from 1 upto the length of the input"
            (check-equal? (parse-results-value-set results)
                          (set '(a)
                               '(a b)
                               '(a b c)))))
        (context "without a leading symbol in the input"
            (define input '(1 b c))
            (define results (parse parser input))
            (it "will fail"
              (check-true (parse-failure? results))))))

    (describe "parser-one-or-more-greedy"
      (context "with a nongreedy parser matching one or more symbols"
        (define parser (parser-one-or-more-greedy (parser-sat symbol?)))
        (context "with arbitrary input"
          (define input '(a b c))
          (define results (parse parser input))
          (it "will succeed with 1 result"
            (check-true (parse-success? results))
            (check-equal? (stream-length results) 1))
          (it "will include the full input in the result value"
            (check-equal? (parse-result-value (stream-first results))
                          '(a b c))))
        (context "without a leading symbol in the input"
          (define input '(1 b c))
          (define results (parse parser input))
          (it "will fail"
            (check-true (parse-failure? results))))))

    (describe "parser-sequence"
      (context "with a parser for a symbol string number sequence"
        (define parser
          (parser-sequence
           (parser-sat symbol?)
           (parser-sat string?)
           (parser-sat number?)))
        (context "with input that matches the sequence"
          (define input '(x "y" 3))
          (define results (parse parser input))
          (it "will succeed with one output"
            (check-true (parse-success? results))
            (check-equal? (stream-length results) 1))
          (it "will have the input sequence in the results"
            (check-equal? (parse-result-value (stream-first results))
                          input)))
        (context "with input that does not the sequence"
          (define input '(a b c))
          (define results (parse parser input))
          (it "will fail"
            (check-true (parse-failure? results))))))

    (describe "parser-descend"
      (context "with a parser that descends into a sequence of one or more symbols"
        (define parser
          (let/monad ([(parse-result value remaining-input)
                         (parser-descend
                          (parser-zero-or-more-greedy
                           (parser-sat symbol?)))])
            (if (stream-empty? remaining-input)
                (parser-return (stream->list value))
              (parser-fail))))
        (check-true (parser? parser))
        (context "with input with a nested stream (list) of symbols"
          (define input '((a b c) "x"))
          (define results (parse parser input))
          (it "succeeds with one result"
            (check-true (parse-success? results))
            (check-equal? (stream-length results) 1))
          (it "will have the nested list from the input as the result value"
            (check-equal? (parse-result-value (stream-first results)) (car input))))))

    (describe "parser-unordered-sequence-greedy"
      (context "with an unordered sequence of parsers"
        (define parser (parser-unordered-sequence-greedy
                        (parser-sat symbol?)
                        (parser-sat string?)
                        (parser-sat number?)))
        (context "with a input matching the sequence"
          (define input '(a "b" 3))
          (define results (parse parser input))
          (it "succeeds"
            (check-true (parse-success? results)))
          (context "with permutations of the input"
            (define input-permutations (permutations input))
            (it "succeeds for each permutation"
              (for ([input input-permutations])
                (check-true (parse-success? (parse parser input))))))))))

  (describe "binding syntax"
    (describe "functor binding syntax"
      (it "can be used to build parsers"
        (define parser
          (let/functor ([x (parser-return 'x)])
            (symbol->string x)))
        (check-true (parser? parser))
        (context "with arbitrary input"
          (define input '(a b))
          (define results (parse parser input))
          (it "will successfully parse the input"
            (check-true (parse-success? results)))
          (it "will return the transformed result of the first parser"
            (check-equal? (parse-result-value (stream-first results)) "x")))))

    (describe "applicative binding syntax"
      (it "can be used to build parsers"
        (define parser
          (let/applicative ([x (parser-return 'x)]
                            [y (parser-return 'y)])
            (string-append (symbol->string x) (symbol->string y))))
        (check-true (parser? parser))
        (context "with arbitrary input"
          (define input '(a b))
          (define results (parse parser input))
          (it "will successfully parse the input"
            (check-true (parse-success? results)))
          (it "will return the transformed result of the first parser"
            (check-equal? (parse-result-value (stream-first results)) "xy")))))

    (describe "monad binding syntax"
      (it "can be used to build parsers"
        (define parser
          (let/monad ([x (parser-return 'x)])
            (return (symbol->string x))))
        (check-true (parser? parser))

        (context "with arbitrary input"
          (define input '(a b))
          (define results (parse parser input))
          (it "will successfully parse the input"
            (check-true (parse-success? results)))
          (it "will return the transformed result of the first parser"
            (check-equal? (parse-result-value (stream-first results)) "x")))))))
