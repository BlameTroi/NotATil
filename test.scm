;; originally these were test cases for the exercism
;; scheme track forth exercise. i'm using them as a
;; base for testing notatil.

(load "test-util.ss")

(define test-cases
  `(
    (test-success "numbers just get pushed onto the stack"
      equal? notatil-test-clear-dict '("1 2 3 4 5") '(5 4 3 2 1))
    (test-success "can add two numbers" equal? notatil-test-clear-dict
      '("1 2 +") '(3))
    (test-error
      "errors if there is nothing on the stack"
      notatil-test-clear-dict
      '("+"))
    (test-error
      "errors if there is only one value on the stack"
      notatil-test-clear-dict
      '("1 +"))
    (test-success "can subtract two numbers" equal? notatil-test-clear-dict
      '("3 4 -") '(-1))
    (test-error
      "errors if there is nothing on the stack"
      notatil-test-clear-dict
      '("-"))
    (test-error
      "errors if there is only one value on the stack"
      notatil-test-clear-dict
      '("1 -"))
    (test-success "can multiply two numbers" equal? notatil-test-clear-dict
      '("2 4 *") '(8))
    (test-error
      "errors if there is nothing on the stack"
      notatil-test-clear-dict
      '("*"))
    (test-error
      "errors if there is only one value on the stack"
      notatil-test-clear-dict
      '("1 *"))
    (test-success "can divide two numbers" equal? notatil-test-clear-dict
      '("12 3 /") '(4))
    (test-success "performs integer division" equal? notatil-test-clear-dict
      '("8 3 /") '(2))
    (test-error "errors if dividing by zero" notatil-test-clear-dict '(("4 0 /")))
    (test-error
      "errors if there is nothing on the stack"
      notatil-test-clear-dict
      '("/"))
    (test-error
      "errors if there is only one value on the stack"
      notatil-test-clear-dict
      '("1 /"))
    (test-success "addition and subtraction" equal? notatil-test-clear-dict
      '("1 2 + 4 -") '(-1))
    (test-success "multiplication and division" equal? notatil-test-clear-dict
      '("2 4 * 3 /") '(2))
    (test-success "copies a value on the stack" equal? notatil-test-clear-dict
      '("1 dup") '(1 1))
    (test-success "copies the top value on the stack" equal?
      notatil-test-clear-dict '("1 2 dup") '(2 2 1))
    (test-error
      "errors if there is nothing on the stack"
      notatil-test-clear-dict
      '("dup"))
    (test-success
      "removes the top value on the stack if it is the only one"
      equal? notatil-test-clear-dict '("1 drop") '())
    (test-success
      "removes the top value on the stack if it is not the only one"
      equal? notatil-test-clear-dict '("1 2 drop") '(1))
    (test-error
      "errors if there is nothing on the stack"
      notatil-test-clear-dict
      '("drop"))
    (test-success
      "swaps the top two values on the stack if they are the only ones"
      equal? notatil-test-clear-dict '("1 2 swap") '(1 2))
    (test-success
      "swaps the top two values on the stack if they are not the only ones"
      equal? notatil-test-clear-dict '("1 2 3 swap") '(2 3 1))
    (test-error
      "errors if there is nothing on the stack"
      notatil-test-clear-dict
      '("swap"))
    (test-error
      "errors if there is only one value on the stack"
      notatil-test-clear-dict
      '("1 swap"))
    (test-success
      "copies the second element if there are only two" equal?
      notatil-test-clear-dict '("1 2 over") '(1 2 1))
    (test-success
      "copies the second element if there are more than two"
      equal? notatil-test-clear-dict '("1 2 3 over") '(2 3 2 1))
    (test-error
      "errors if there is nothing on the stack"
      notatil-test-clear-dict
      '("over"))
    (test-error
      "errors if there is only one value on the stack"
      notatil-test-clear-dict
      '("1 over"))
    (test-success "can consist of built-in words" equal? notatil-test-clear-dict
      '(": dup-twice dup dup ; 1 dup-twice") '(1 1 1))
    (test-success "execute in the right order" equal? notatil-test-clear-dict
      '(": countup 1 2 3 ; countup") '(3 2 1))
    (test-success "can override other user-defined words" equal? notatil-test-clear-dict
      '(": foo dup ; : foo dup dup ; 1 foo") '(1 1 1))
    (test-success "can override built-in words" equal? notatil-test-clear-dict
      '(": swap dup ; 1 swap") '(1 1))
    (test-success "can override built-in operators" equal? notatil-test-clear-dict
      '(": + * ; 3 4 +") '(12))
    (test-success "can use different words with the same name" equal? notatil-test-clear-dict
      '(": foo 5 ; : bar foo ; : foo 6 ; bar foo") '(6 5))
    (test-success "can define word that uses word with the same name" equal?
      notatil-test-clear-dict '(": foo 10 ; : foo foo 1 + ; foo") '(11))
    (test-error "cannot redefine numbers" notatil-test-clear-dict '(": 1 2 ;"))
    (test-error
      "errors if executing a non-existent word"
      notatil-test-clear-dict
      '("foo"))
    (test-success "DUP is case-insensitive" equal? notatil-test-clear-dict
      '("1 DUP Dup dup") '(1 1 1 1))
    (test-success "DROP is case-insensitive" equal? notatil-test-clear-dict
      '("1 2 3 4 DROP Drop drop") '(1))
    (test-success "SWAP is case-insensitive" equal? notatil-test-clear-dict
      '("1 2 SWAP 3 Swap 4 swap") '(1 4 3 2))
    (test-success "OVER is case-insensitive" equal? notatil-test-clear-dict
      '("1 2 OVER Over over") '(1 2 1 2 1))
    (test-success "user-defined words are case-insensitive" equal? notatil-test-clear-dict
      '(": foo dup ; 1 FOO Foo foo") '(1 1 1 1))
    (test-success "definitions are case-insensitive" equal?
      notatil-test-clear-dict '(": SWAP DUP Dup dup ; 1 swap") '(1 1 1 1))
    ))

(run-with-cli "notatil.scm" (list test-cases))
