;; This is based on srfi 78's examples.scm file for
;; lightweight testing.

;; Setup For Guile:
;;
;; (use-modules (srfi srfi-23))  ;; error (always there)
(use-modules (srfi srfi-42))     ;; comprehensions
;; Guile does not include srfi 78 in its library modules
;; so we load it as raw source.
;;
(load "srfi-78.scm")

;;
;; Testing new notatil tokenization
;;
;; srfi 78 doesn't provide a check-error yet, so we can
;; only test things that don't throw errors. This isn't
;; too limiting for the type of work I'm doing. We'll
;; test words and behavior for correctness then, not
;; error handling.
;;

(load "notatil.scm")
(define nat nat-test-clear-dictionary)


;;
;; develop and test in one file :)
;;

;; moved to notatil.scm
;;
;; (define nat-buffer "")
;; (define nat-tokenized '())
;; (define nat-compiling #f)
;; (define nat-in-def #f)
;; (define nat-in-comment #f)
;; (define nat-in-string #f)
;; (define nat-buffer-empty #t)



(define (tok-tester s)
  "A unit test entry point to set up the tokenizer state
to treat string S as if it were an incoming buffer to
NotaTil."
  ;; reset notatil state, primarily dictionary
  (nat-full-reset)
  ;; reset tokenizer state
  (set! nat-buffer (nat-scrub s))
  (set! nat-tokenized '())
  (set! nat-compiling #f)
  ;; (set! nat-in-comment #f)
  ;; (set! nat-in-string #f)
  (set! nat-buffer-empty #f)
  ;; call nat-tokenizer until such time as nat-buffer
  ;; is empty
  (let* ((tok-pair '()) (tok-extra-pair '()) (tok-closing-pair '())
         (tok-type 'nat-tok-none) (tok-string "")
         (dict-entry 'nat-word-not-found))
    (set! tok-pair (nat-next-token))
    (while (not nat-buffer-empty)
      (set! tok-type (car tok-pair))
      (set! tok-string (cdr tok-pair))
      (cond
       ((equal? tok-type 'nat-tok-word)
        (set! dict-entry (nat-lookup tok-string))
        (if (equal? dict-entry 'nat-word-not-found)
            (let ((n (token-is-integer-literal tok-string radix))
                  (f (token-is-real-literal tok-string radix)))
              (cond (n (set! tok-type 'nat-tok-integer))
                    (f (set! tok-type 'nat-tok-real))
                    (else (set! tok-type 'nat-tok-word-unknown)))
              (set! tok-pair (cons tok-type tok-string)))))
       ((equal? tok-type 'nat-tok-begin-comment)
        (set! tok-extra-pair (cons 'nat-tok-comment (nat-buffer-to-char #\))))
        (set! tok-closing-pair (nat-next-token)))
       ((or (equal? tok-type 'nat-tok-print-string) (equal? tok-type 'nat-tok-begin-string))
        (set! tok-extra-pair (cons 'nat-tok-string (nat-buffer-to-char #\")))
        (set! tok-closing-pair (nat-next-token))
        (if (equal? (car tok-closing-pair) 'nat-tok-begin-string)
            (set! tok-closing-pair (cons 'nat-tok-end-string (cdr tok-closing-pair)))))
       ) ;; add processed token to result
      (set! nat-tokenized (cons tok-pair nat-tokenized))
      (if (not (null? tok-extra-pair))
          (set! nat-tokenized (cons tok-extra-pair nat-tokenized)))
      (if (and (not (null? tok-closing-pair))
               (not (equal? (car tok-closing-pair) 'nat-tok-none)))
          (set! nat-tokenized (cons tok-closing-pair nat-tokenized)))
      (set! tok-pair (nat-next-token))
      (set! tok-extra-pair '())
      (set! tok-closing-pair '())))
  ;; put in proper order for caller
  (reverse nat-tokenized))


;;;
;;; Parse and tokenize testing.
;;;

;; clear reporting
(check-reset!)

;;
;; Recognition of single tokens in isolation
;;
(check (tok-tester " ") =>
       (list ))
(check (tok-tester "blah") =>
       (list (cons 'nat-tok-word-unknown "blah")))
(check (tok-tester "if") =>
       (list (cons 'nat-tok-if "if")))
(check (tok-tester "then") =>
       (list (cons 'nat-tok-then "then")))
(check (tok-tester "do") =>
       (list (cons 'nat-tok-do "do")))
(check (tok-tester "loop") =>
       (list (cons 'nat-tok-loop "loop")))
(check (tok-tester "(") =>
       (list (cons 'nat-tok-begin-comment "(")
             (cons 'nat-tok-comment "ERROR COULD NOT FIND EXPECTED ')'")))
(check (tok-tester ".\"") =>
       (list (cons 'nat-tok-print-string ".\"")
             (cons 'nat-tok-string "ERROR COULD NOT FIND EXPECTED '\"'")))
(check (tok-tester "s\"") =>
       (list (cons 'nat-tok-begin-string "s\"")
             (cons 'nat-tok-string "ERROR COULD NOT FIND EXPECTED '\"'")))
(check (tok-tester "\"") =>
       (list (cons 'nat-tok-begin-string "\"")
             (cons 'nat-tok-string "ERROR COULD NOT FIND EXPECTED '\"'")))
(check (tok-tester "constant") =>
       (list (cons 'nat-tok-constant "constant")))
(check (tok-tester "variable") =>
       (list (cons 'nat-tok-variable "variable")))
(check (tok-tester "marker") =>
       (list (cons 'nat-tok-marker "marker")))
(check (tok-tester "bye") =>
       (list (cons 'nat-tok-bye "bye")))
(check (tok-tester "help") =>
       (list (cons 'nat-tok-help "help")))
(check (tok-tester ":") =>
       (list (cons 'nat-tok-begin-definition ":")))

;; does not change case
(check (tok-tester "DelTA") =>
       (list (cons 'nat-tok-word-unknown "DelTA")))
(check (tok-tester "ELSE") =>
       (list (cons 'nat-tok-else "ELSE")))

;;
;; numeric literal recognition
;;
(check (tok-tester "-2") =>
       (list (cons 'nat-tok-integer "-2")))
(check (tok-tester "-1") =>
       (list (cons 'nat-tok-integer "-1")))
(check (tok-tester "0") =>
       (list (cons 'nat-tok-integer "0")))
(check (tok-tester "1") =>
       (list (cons 'nat-tok-integer "1")))
(check (tok-tester "2") =>
       (list (cons 'nat-tok-integer "2")))
(check (tok-tester "1.25") =>
       (list (cons 'nat-tok-real "1.25")))
(check (tok-tester "1.0E-8") =>
       (list (cons 'nat-tok-real "1.0E-8")))

;; This could be a hex value depending on radix but for now testing
;; with radix 10
(check (tok-tester "1EEF") =>
       (list (cons 'nat-tok-word-unknown "1EEF")))

;; display number format should be seen as an unknown word
;; but is not an automatic error.
(check (tok-tester "10,357.62") =>
       (list (cons 'nat-tok-word-unknown "10,357.62")))

;;
;; simple multi token expressions
;;
(check (tok-tester ": foo 5 ; ") =>
       (list (cons 'nat-tok-begin-definition ":")
             (cons 'nat-tok-word-unknown "foo")
             (cons 'nat-tok-integer "5")
             (cons 'nat-tok-end-definition ";")))

(check (tok-tester "10 0 DO foo LOOP .s") =>
       (list (cons 'nat-tok-integer "10")
             (cons 'nat-tok-integer "0")
             (cons 'nat-tok-do "DO")
             (cons 'nat-tok-word-unknown "foo")
             (cons 'nat-tok-loop "LOOP")
             (cons 'nat-tok-word ".s")))

;; various strings
(check (tok-tester ".\" this is a test\" cr") =>
       (list (cons 'nat-tok-print-string ".\"")
             (cons 'nat-tok-string "this is a test")
             (cons 'nat-tok-end-string "\"")
             (cons 'nat-tok-word "cr")))

(check (tok-tester "\" This is a string\"") =>
       (list (cons 'nat-tok-begin-string "\"")
             (cons 'nat-tok-string "This is a string")
             (cons 'nat-tok-end-string "\"")))

;; Words before and after a string, s" variety
(check (tok-tester "constant string s\" asdf\" string c@ ") =>
       (list (cons 'nat-tok-constant "constant")
             (cons 'nat-tok-word-unknown "string")
             (cons 'nat-tok-begin-string "s\"")
             (cons 'nat-tok-string "asdf")
             (cons 'nat-tok-end-string "\"")
             (cons 'nat-tok-word-unknown "string")
             (cons 'nat-tok-word-unknown "c@"))) ;; will need to change to known word at some point

;; comments
(check (tok-tester ": plus ( n1 n2 -- sum ) + ; ") =>
       (list (cons 'nat-tok-begin-definition ":")
             (cons 'nat-tok-word-unknown "plus")
             (cons 'nat-tok-begin-comment "(")
             (cons 'nat-tok-comment "n1 n2 -- sum ")
             (cons 'nat-tok-end-comment ")")
             (cons 'nat-tok-word "+")
             (cons 'nat-tok-end-definition ";")))


;;;
;;; Scrubbing of inbound strings
;;;

;; test embeded newline
(check (nat-scrub "     17 3 4     x  zz
  w") => " 17 3 4 x zz w ")

;; test that tabs, spaces, and newlines handled properly
(check (nat-scrub
        (list->string
         '(#\tab #\1 #\space #\tab #\tab #\2 #\nl #\tab #\+ #\nl)))
       => " 1 2 + ")

;; tests embeded comment
(check   (nat-scrub "Test  ( comment   handling)    in  scrub.") =>
         " Test ( comment   handling) in scrub. ")

;; test from comment testing
(check (nat-scrub "Test with           nothing inside  bracketing   characters ") =>
       " Test with nothing inside bracketing characters ")

;; test embeded string
(check (nat-scrub "Test    with stuff outside s\" quotes   and also inside\"  quotes") =>
       " Test with stuff outside s\" quotes   and also inside\" quotes ")

;;;
;;; final report
;;;

(check-report)
