;; This is based on srfi 78's examples.scm file for lightweight
;; testing.

;; Setup For Guile:
;; (use-modules (srfi srfi-23))  ;; error, this is always there in Guile
(use-modules (srfi srfi-42)) ;; comprehensions, not loaded by default
;; Guile does not include built in srfi 78 support as a module so load the
;; raw source.
(load "srfi-78.scm")

;;
;; Testing notatil
;;
;; srfi 78 doesn't provide a check-error yet, so we can only test things
;; that don't throw errors. So, typos and mismanaging the stack can't be
;; tested well. We'll test words and behavior for correctness then, not
;; error handling.
;;
(load "notatil.scm")
(define nat notatil-test-clear-dictionary)

;; quick test is it up and somewhat functional
(check (nat "1 2 3") => '(3 2 1)) ;; stack works
(check (nat "1 1 +") => '(2)) ;; simple success

;; test basic integer math
(check (nat "1 2 +") => '(3))     ;; +	( n1 n2 — sum )	Adds
(check (nat "1 2 -") => '(-1))    ;; –	( n1 n2 — diff )	Subtracts (n1-n2)
(check (nat "2 1 -") => '(1))     ;; ordering confirmed
(check (nat "10 5 *") => '(50))   ;; *	( n1 n2 — prod )	Multiplies
(check (nat "10 5 /") => '(2))    ;;	( n1 n2 — quot )	Divides (n1/n2)
(check (nat "5 10 /") => '(0))    ;; watch for fractions
(check (nat "2 3 /") => '(0))     ;; modulo stuff
(check (nat "2 3 mod") => '(2))   ;; remainder of 2 / 3
(check (nat "2 3 /mod") => '(2 0)) ;; remainder and quotient

;; test simple stack manipulation
(check (nat "0 1 swap") => '(0 1))   ;; swap top two elements
(check (nat "3 dup") => '(3 3))      ;; duplicate top entry
(check (nat "1 2 3 drop") => '(2 1)) ;; drop top entry
(check (nat "0 1 over") => '(0 1 0)) ;; copy second element to top
(check (nat "1 2 3 4 rot") => '(2 4 3 1)) ;; rotate top 3 items

;; return stack manipulation
(check (nat "1 >r") => '()) ;; kinda hard to test since return stack is not returned
(check (nat "1 >r r>") => '(1)) ;; but this shows to and from r
(check (nat "1 >r r@ r>") => '(1 1)) ;; fetch r is a copy, so this should copy then move

;; i don't think we'll really use the double
;; precision integer words, but testing anyway
;; msw under lsw on stack is how i envision it
(check (nat "1 2 3 2drop") => '(1))
(check (nat "11 12 21 22 2swap") => '(12 11 22 21))
(check (nat "11 12 21 22 2over") => '(12 11 22 21 12 11))
(check (nat "11 12 21 22 2drop") => '(12 11))

;; test base support
(check (nat "hex 10") => '(16))
(check (nat "dec 10") => '(10))
(check (nat "oct 10") => '(8))
(check (nat "bin 10") => '(2))
(check (nat "hex f0") => '(240))
(check (nat "oct 77") => '(63))
(check (nat "bin 1101") => '(13))

;; test simple booleans
(check (nat "-1 not") => '(0))    ;; -1 is true, 0 is false
(check (nat "0 not")  => '(-1))   ;; actually anything not false
(check (nat "14 not") => '(0))    ;; is true

(check (nat "-1 0 and") => '(0))   ;; true and false should be false
(check (nat "0 -1 and") => '(0))   ;; regardless of order
(check (nat "-1 -1 and") => '(-1)) ;; true and true should be true
(check (nat "0 0 and") => '(0))    ;; and of course true and false is false

(check (nat "-1 0 or") => '(-1))   ;; same sequence as and, but we get
(check (nat "0 -1 or") => '(-1))   ;; more trues
(check (nat "-1 -1 or") => '(-1))  ;;
(check (nat "0 0 or") => '(0))     ;;

;; test simple relationals
(check (nat "1 2 <") => '(-1))
(check (nat "1 2 =") => '(0))
(check (nat "1 2 >") => '(0))
(check (nat "1 0<") => '(0))
(check (nat "1 0=") => '(0))
(check (nat "1 0>") => '(-1))
(check (nat "0 ?dup") => '(0))
(check (nat "1 ?dup") => '(1 1))



(check-report)

;; ; -- different equality predicate --

;; (check (vector 1) => (vector 1))
;; (check (vector 1) (=> eq?) (vector 1)) ; fails

;; ; -- parametric tests --

;; (check-ec (+ 1 1) => 2)

;; (check-ec (: x 10) (+ x 1) => (+ x 1) (x))

;; (check-ec (: e 100) (positive? (expt 2 e)) => #t (e)) ; fails on fixnums

;; (check-ec (: e 100) (:let x (expt 2.0 e)) (= (+ x 1) x) => #f (x)) ; fails

;; (check-ec (: e 100) (:let x (expt 2.0 e)) (= (+ x 1) x) => #f)

;; (check-ec (: x 10) (: y 10) (: z 10)
;;           (* x (+ y z)) => (+ (* x y) (* x z))
;;           (x y z)) ; passes with 10^3 cases checked

;; ; -- toy examples --

;; (define (fib n)
;;   (if (<= n 2) 1 (+ (fib (- n 1)) (fib (- n 2)))))

;; (check (fib 1) => 1)
;; (check (fib 2) => 1)
;; (check-ec (: n 1 31) (even? (fib n)) => (= (modulo n 3) 0) (n))

;; ; -- reporting --

;; (check-report)
