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
(load "notatil.scm")
(define nat notatil-test-clear-dictionary)

;; quick test is it up and somewhat functional
(check (nat "1 1 +") => '(2)) ;; simple success
(check (nat "1 1 +") => '(3)) ;; simple failure

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
