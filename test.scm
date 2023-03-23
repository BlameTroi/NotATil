(load "srfi-78.scm")

; Lightweight testing (examples)
; ==============================
;
; Sebastian.Egner@philips.com
; in R5RS + SRFI 23 (error) + SRFI 42 (comprehensions)
;
; history of this file:
;   SE, 25-Oct-2004: first version

; -- portability --

; PLT:
; (require (lib "23.ss" "srfi") (lib "42.ss" "srfi")) (load "check.scm")
; (load "examples.scm")

; Scheme48:
; ,open srfi-23 srfi-42
; ,load check.scm examples.scm

; -- simple test --

(check (+ 1 1) => 2)
(check (+ 1 1) => 3) ; fails

; -- different equality predicate --

(check (vector 1) => (vector 1))
(check (vector 1) (=> eq?) (vector 1)) ; fails

; -- parametric tests --

(check-ec (+ 1 1) => 2)

(check-ec (: x 10) (+ x 1) => (+ x 1) (x))

(check-ec (: e 100) (positive? (expt 2 e)) => #t (e)) ; fails on fixnums

(check-ec (: e 100) (:let x (expt 2.0 e)) (= (+ x 1) x) => #f (x)) ; fails

(check-ec (: e 100) (:let x (expt 2.0 e)) (= (+ x 1) x) => #f)

(check-ec (: x 10) (: y 10) (: z 10)
          (* x (+ y z)) => (+ (* x y) (* x z))
          (x y z)) ; passes with 10^3 cases checked

; -- toy examples --

(define (fib n)
  (if (<= n 2) 1 (+ (fib (- n 1)) (fib (- n 2)))))

(check (fib 1) => 1)
(check (fib 2) => 1)
(check-ec (: n 1 31) (even? (fib n)) => (= (modulo n 3) 0) (n))

; -- reporting --

(check-report)
