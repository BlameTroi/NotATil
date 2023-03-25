;; This is based on srfi 78's examples.scm file for lightweight
;; testing.

;; Setup For Guile:
;; (use-modules (srfi srfi-23))  ;; error, this is always there in Guile
(use-modules (srfi srfi-42)) ;; comprehensions, not loaded by default
;; Guile does not include built in srfi 78 support as a module so load the
;; raw source.
(load "srfi-78.scm")

;;
;; Testing new notatil tokenization
;;
;; srfi 78 doesn't provide a check-error yet, so we can only test things
;; that don't throw errors. So, typos and mismanaging the stack can't be
;; tested well. We'll test words and behavior for correctness then, not
;; error handling.
;;
(load "notatil.scm")
(define nat nat-test-clear-dictionary)

;; quick test is it up and somewhat functional
(check (nat "1 2 3") => '(3 2 1)) ;; stack works
(check (nat "1 1 +") => '(2)) ;; simple success

;;
;; develop and test in one file :)
;;
(define nat-compiling #f)
(define nat-in-def #f)
(define nat-in-comment #f)
(define nat-in-string #f)
(define nat-question #f)

(define nat-tok-word)
(define nat-tok-integer)
(define nat-tok-real)
(define nat-tok-string)
(define nat-tok-comment)
(define nat-tok-real)
(define nat-tok-definition)
(define nat-tok-unknown)


;; USE xxx
;; USING xxx	( –)	Use file xxx as Forth “disk”
;; ( xxx)	( — )	Ignores text up to “)” delimeter
;; INCLUDE xxx	( — )	Load the text file xxx
;; FORGET xxx	( — )	Forget definitions back through xxx
;; MARKER xxx	( — )	Defines marker xxx to roll back dictionary
;; : ;
;; variable xxx
;; constant xxx
;; cells
;; allot
(define nat-definition-markers '( " : " " ; " " .\" " "\" " " marker "
                                  " variable " "constant " " cells "
                                  "allot" ))
(define (nat-has-definitions s)
  "Does string S contain definining words? If so, are they complete?
Returns a list of start and end locations of:

: ; variable constant cell alot and marker."

  '()
  )

(define (nat-scrub s)
  "Normalize string S for tokenization. Change all whitespace to
blanks, reduce runs of blanks to a single blank unless they are in
a Forth string, and add blanks to either end of the incoming string."
  (let ((accum '(#\space))
        (lastc #\space)
        (currc #\space)
        (instr #f)
        (t (string->list s)))
    (while (not (null? t))
      (set! currc (car t))
      (set! t (cdr t))
      (if (or (char=? currc #\nl) (char=? currc #\tab)) (set! currc #\space))
      (if (or (not (char=? currc #\space)) (not (char=? lastc currc)))
          (begin
            (set! accum (cons currc accum))))
      (set! lastc currc))
    (if (not (char=? (car accum) #\space))
        (set! accum (cons #\space accum)))
    (list->string (reverse accum))))

(define (nat-tokenizer s)
  "Return tokens from string S as a list of (type . text) pairs."
  (set! s (nat-scrub s))
  (if (not (null? (nat-has-definitions s)))
      ;; has definitions, not doing anything yet
      (begin '())
      ;; has no definitions, straight break on whitespace
      (begin
        (map (lambda (t)
               (let ((num (number? (token-is-numeric-literal t radix))))
                 (cond ((and num (integer? (token-is-numeric-literal t radix))) (cons 'nat-tok-integer t))
                       ((and num (inexact? (token-is-numeric-literal t radix))) (cons 'nat-tok-real t))
                       ((not (equal? 'word-not-found (lookup t))) (cons 'nat-tok-word t))
                       (else (cons 'nat-tok-unknown t)))))
             (filter (lambda (t) (string<> t "")) (string-split s (char-set #\space)))
             ;;
             ))))


;; scrub line that needs no scrub
(check (nat-scrub "1 2 +") => " 1 2 + ")

;; deblank and denewline
(check (nat-scrub "     17 3 4     x  zz
  w") => " 17 3 4 x zz w ")

;; check all three special characters
(check (nat-scrub (list->string '(#\tab #\1 #\space #\tab #\tab #\2 #\nl #\tab #\+ #\nl))) => " 1 2 + ")

;; clean text in
(check (nat-tokenizer "1 2 +") => '((nat-tok-integer . "1") (nat-tok-integer . "2") (nat-tok-word . "+")))

;; extra white space
(check (nat-tokenizer "1    2 .     ") => '((nat-tok-integer . "1") (nat-tok-integer . "2") (nat-tok-word . ".")))

;; just a comment
(check (nat-tokenizer "( n1 n2 -- n1 * n2 ) ") => '((nat-comment . "( n1 n2 -- n1 * n2 )")))

;;;; be sure to test : foo 5; as opposed to : foo 5 ;


;; final report



(check-report)
