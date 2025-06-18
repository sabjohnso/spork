#lang racket



(module+ test
  (require spork/enum rackunit rackunit/spec)
  (describe "enum, a macro for defining enumerated value types"
    (context "with a type for enumerated values a b and c"
      (enum abc
	([a]
	 [b]
	 [c]))
      (it "binds the names to the corresponding symbols"
	(check-equal? a 'a)
	(check-equal? b 'b)
	(check-equal? c 'c))
      (describe "abc?"
	(it "is a predicate that recognizes 'a 'b and 'c"
	  (check-true (abc? 'a))
	  (check-true (abc? 'b))
	  (check-true (abc? 'c)))
	(it "does not recognize other values"
	  (check-false (abc? 'x))
	  (check-false (abc? 7))
	  (check-false (abc? "a purple cow"))))
      (describe "abc-value?"
	(it "is a predicate that recognizes the values associated with 'a 'b and 'c"
	  (check-true (abc-value? 0))
	  (check-true (abc-value? 1))
	  (check-true (abc-value? 2)))
	(it "does NOT recognize the symbols or other values"
	  (check-false (abc-value? 'a))
	  (check-false (abc-value? 'b))
	  (check-false (abc-value? 'c))
	  (check-false (abc-value? 3))))
      (describe "abc-name->value"
	(it "converts an abc name to an abc value"
	  (check-equal? (abc-name->value 'a) 0)
	  (check-equal? (abc-name->value 'b) 1)
	  (check-equal? (abc-name->value 'c) 2))
	(it "fails on other inputs"
	  (check-exn exn:fail? (thunk (abc-name->value 'x)))))
      (describe "abc-value->name"
	(it "converts an abc value to an abc name"
	  (check-equal? (abc-value->name 0) 'a)
	  (check-equal? (abc-value->name 1) 'b)
	  (check-equal? (abc-value->name 2) 'c))
	(it "fails on other inputs"
	  (check-exn exn:fail? (thunk (abc-value->name 3))))))
    (context "with a type for enumerated values one and two"
      (enum thing
	([one 1]
	 [two]))
      (describe "thunk-name->value"
	(it "was expicitly defined for one"
	  (check-equal? (thing-name->value 'one) 1))
	(it "was implicitly define for two"
	  (check-equal? (thing-name->value 'two) 2))))))
