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

;; quick test is it up and somewhat functional
(check (nat "1 2 3") => '(3 2 1)) ;; stack works
(check (nat "1 1 +") => '(2)) ;; simple success

;;
;; develop and test in one file :)
;;

(define nat-buffer "")
(define nat-tokenized '())
(define nat-compiling #f)
(define nat-in-def #f)
(define nat-in-comment #f)
(define nat-in-string #f)
(define nat-buffer-empty #t)

;; some of the forms we'll have to deal with
;;
;; USE xxx
;; USING xxx Use file xxx as Forth “disk”
;; ( xxx)	Ignores text up to “)” delimeter
;; INCLUDE xxx Load the text file xxx
;; FORGET xxx	Forget definitions back through xxx
;; MARKER xxx	Defines marker xxx to roll back dictionary
;;            where xxx is a word that forgets itself
;; : ;
;; variable xxx
;; constant xxx
;; cells
;; allot

(define (nat-scrub s)
  "Normalize string S for tokenization. Change all white-
space to blanks, reduce runs of blanks to a single blank
unless they are in a Forth string, and add blanks to both
ends of the incoming string to simplify boundary handling."
  (let ((accum '(#\space))
        (lastc #\space)
        (currc #\space)
        (instr #f)
        (t (string->list s)))
    (while (not (null? t))
      (set! currc (car t))
      (set! t (cdr t))
      (if (or (char=? currc #\nl) (char=? currc #\tab))
          (set! currc #\space))
      (if (or (not (char=? currc #\space)) (not (char=? lastc currc)))
          (begin
            (set! accum (cons currc accum))))
      (set! lastc currc))
    (if (not (char=? (car accum) #\space))
        (set! accum (cons #\space accum)))
    (list->string (reverse accum))))


(define (tok-tester s)
  "A unit test entry point to set up the tokenizer state
to treat string S as if it were an incoming buffer to
NotaTil."
  (set! nat-buffer (nat-scrub s))
  (set! nat-tokenized '())
  (set! nat-compiling #f)
  (set! nat-in-comment #f)
  (set! nat-in-string #f)
  (set! nat-buffer-empty #f)
  ;; call net-tokenizer until such time as nat-buffer
  ;; is empty
  (let* ((i 0)
         (tok-pair '())
         (tok-type 'nat-tok-none)
         (tok-string ""))
    (set! tok-pair (nat-next-token))
    (while (not nat-buffer-empty)
      (set! tok-type (car tok-pair))
      (set! tok-string (cdr tok-pair))
      ;; could do more here but for now ...
      (set! nat-tokenized (cons tok-pair nat-tokenized))
      (set! tok-pair (nat-next-token))
      )
    ;; post processing should scan for numbers and
    ;; mark them here. check everything for existence
    ;; in dictionary, if not, it must either be a
    ;; literal, new definition, or ?
    (reverse nat-tokenized)))


(define (nat-next-token)
  "Get the next token out of nat-buffer and return it as
a (type . text) pair. Updates tokenizer state and buffer
but does not consume past the token.

Anything not recognized is assumed to be a word and will
be addressed later in notatil."
  (nat-trimleft-buffer)
  (cond ((string= "" nat-buffer)
         (set! nat-buffer-empty #t)
         (cons 'nat-tok-none ""))
        ((nat-buffer-begins-definition?)
         (cons 'nat-tok-definition (nat-buffer-to-blank)))
        ((nat-buffer-begins-string?)
         (cons 'nat-tok-string (nat-buffer-to-blank)))
        ((nat-buffer-begins-do?)
         (cons 'nat-tok-do (nat-buffer-to-blank)))
        ((nat-buffer-begins-loop?)
         (cons 'nat-tok-loop (nat-buffer-to-blank)))
        ((nat-buffer-begins-if?)
         (cons 'nat-tok-if (nat-buffer-to-blank)))
        ((nat-buffer-begins-then?)
         (cons 'nat-tok-then (nat-buffer-to-blank)))
        ((nat-buffer-begins-else?)
         (cons 'nat-tok-else (nat-buffer-to-blank)))
        ((nat-buffer-begins-comment?)
         (cons 'nat-tok-comment (nat-buffer-to-blank)))
        ((nat-buffer-begins-variable?)
         (cons 'nat-tok-variable (nat-buffer-to-blank)))
        ((nat-buffer-begins-constant?)
         (cons 'nat-tok-constant (nat-buffer-to-blank)))
        ((nat-buffer-begins-marker?)
         (cons 'nat-tok-marker (nat-buffer-to-blank)))
        ((nat-buffer-begins-bye?)
         (cons 'nat-tok-bye (nat-buffer-to-blank)))
        ((nat-buffer-begins-help?)
         (cons 'nat-tok-help (nat-buffer-to-blank)))
        ((nat-buffer-begins-see?)
         (cons 'nat-tok-see (nat-buffer-to-blank)))
        ((nat-buffer-begins-list?)
         (cons 'nat-tok-list (nat-buffer-to-blank)))
        (else
         (cons 'nat-tok-word (nat-buffer-to-blank)))))

;;;
;;; Buffer parse and carve
;;;

(define (nat-trimleft-buffer)
  "Remove leading blanks from nat-buffer. Recursive."
  (cond ((or nat-buffer-empty (= 0 (string-length nat-buffer)))
         (set! nat-buffer-empty #t)     ; emptied
         (set! nat-buffer ""))
        ((not (char=? #\space (string-ref nat-buffer 0) )))
        (else
         (set! nat-buffer (substring nat-buffer 1)) ; trim and check again
         (nat-trimleft-buffer))))

(define (nat-buffer-to-blank)
  "Take the next token from nat-buffer up to a blank
or the end of the buffer. The scrubbing process when
starting tokenize is expected to wrap the buffer with
a blank on either end.

The terminating blank is consumed but not returned."
  (let* ((i (string-index nat-buffer #\space))
         (t (substring nat-buffer 0 i)))
    (set! nat-buffer (substring nat-buffer (1+ i)))
    t))

(define (nat-buffer-to-char c)
  "Return the contents of the buffer up to but not
including character C. This would be useful when
finding end of string, etc."
  )

(define (nat-buffer-through-char c)
  "Return the contents of the buffer up to and
including character C."
  )

(define (nat-buffer-to-token tk)
  "Return the contents of the buffer up to but not
including the string token TK."
  )

(define (nat-buffer-through-token tk)
  "Return the contents of the buffer up to and
including the string token TK."
  )

(define (nat-buffer-prefix? s)
  "DRY buffer checks."
  (= (string-length s) (string-prefix-length-ci nat-buffer s)))

(define (nat-buffer-begins-string?)
  "Is this the start of a string definition, either a print
or literal?"
  (or (nat-buffer-prefix? ".\" ")
      (nat-buffer-prefix? "s\" ")
      (nat-buffer-prefix? "\" ")))

;;
;; These are pretty routine syntax wise. We could have some
;; sort of table controlling this, but since I don't know
;; what all I need in that table beyond the list of special
;; tokens, we'll do things this way for now.
;;
(define (nat-buffer-begins-do?)
  (nat-buffer-prefix? "do "))

(define (nat-buffer-begins-loop?)
  (nat-buffer-prefix? "loop "))

(define (nat-buffer-begins-if?)
  (nat-buffer-prefix? "if "))

(define (nat-buffer-begins-then?)
  (nat-buffer-prefix? "then "))

(define (nat-buffer-begins-else?)
  (nat-buffer-prefix? "else "))

(define (nat-buffer-begins-definition?)
  (nat-buffer-prefix? ": "))

(define (nat-buffer-begins-variable?)
  (nat-buffer-prefix? "variable "))

(define (nat-buffer-begins-constant?)
  (nat-buffer-prefix? "constant "))

(define (nat-buffer-begins-marker?)
  (nat-buffer-prefix? "marker "))

(define (nat-buffer-begins-see?)
  (nat-buffer-prefix? "see "))

(define (nat-buffer-begins-list?)
  (nat-buffer-prefix? "list "))

(define (nat-buffer-begins-bye?)
  (nat-buffer-prefix? "bye "))

(define (nat-buffer-begins-help?)
  (nat-buffer-prefix? "help "))

(define (nat-buffer-begins-comment?)
  (nat-buffer-prefix? "( "))

;;;
;;; Parse and tokenize testing.
;;;

;; clear reporting
(check-reset!)

;;
;; Recognition of single tokens in isolation
;;
(check (tok-tester " ") => (list ))
(check (tok-tester "blah") => (list (cons 'nat-tok-word "blah")))
;; does not change case
(check (tok-tester "DelTA") => (list (cons 'nat-tok-word "DelTA")))
(check (tok-tester "if") => (list (cons 'nat-tok-if "if")))
(check (tok-tester "ELSE") => (list (cons 'nat-tok-else "ELSE")))
(check (tok-tester "then") => (list (cons 'nat-tok-then "then")))
(check (tok-tester "do") => (list (cons 'nat-tok-do "do")))
(check (tok-tester "loop") => (list (cons 'nat-tok-loop "loop")))
(check (tok-tester "(") => (list (cons 'nat-tok-comment "(")))
(check (tok-tester "constant") => (list (cons 'nat-tok-constant "constant")))
(check (tok-tester "variable") => (list (cons 'nat-tok-variable "variable")))
(check (tok-tester "marker") => (list (cons 'nat-tok-marker "marker")))
(check (tok-tester "bye") => (list (cons 'nat-tok-bye "bye")))
(check (tok-tester "help") => (list (cons 'nat-tok-help "help")))
(check (tok-tester ":") => (list (cons 'nat-tok-definition ":")))

;;
;; numeric literal recognition
;;
(check (tok-tester "1") => (list (cons 'nat-tok-integer "1")))
(check (tok-tester "1.25") => (list (cons 'nat-tok-real "1.25")))
(check (tok-tester "1.0E-8") => (list (cons 'nat-tok-real "1.0E-8")))
;; This could be a hex value depending on radix but for now testing
;; with radix 10
(check (tok-tester "1EEF") => (list (cons 'nat-tok-word "1EEF")))

;;
;; simple multi token expressions
;;
(check (tok-tester ": foo 5 ; ") => (list (cons 'nat-tok-definition ":")
                                          (cons 'nat-tok-word "foo")
                                          (cons 'nat-tok-word "5")
                                          (cons 'nat-tok-word ";")))

(check (tok-tester "10 0 do foo loop .s") => (list (cons 'nat-tok-word "10")
                                                   (cons 'nat-tok-word "0")
                                                   (cons 'nat-tok-do "do")
                                                   (cons 'nat-tok-word "foo")
                                                   (cons 'nat-tok-loop "loop")
                                                   (cons 'nat-tok-word ".s")))

(check (tok-tester ".\" this is a test\" cr") => (list (cons 'nat-tok-begin-string ".\"")
                                                       (cons 'nat-tok-string "this is a test")
                                                       (cons 'nat-tok-end-string "\"")
                                                       (cons 'nat-tok-word "cr")))


;;;
;;; Scrubbing of inbound strings
;;;

;; scrub line that needs no scrub
(check (nat-scrub "1 2 +") => " 1 2 + ")

;; deblank and denewline
(check (nat-scrub "     17 3 4     x  zz
  w") => " 17 3 4 x zz w ")

;; check all three special characters
(check (nat-scrub (list->string '(#\tab #\1 #\space #\tab #\tab #\2 #\nl #\tab #\+ #\nl))) => " 1 2 + ")


;;;
;;; final report
;;;

(check-report)
