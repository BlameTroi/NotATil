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


;; these gymnastics are needed because I haven't figured
;; out how to enter (char-set #\" #\( #\) ) with lispy.
;; I'd rather not switch it off and on.
(define dlm-lsc (string->list "\"()"))
(define dlm-quote (car dlm-lsc))
(define dlm-lparen (cadr dlm-lsc))
(define dlm-rparen (caddr dlm-lsc))
(define dlm-openers (char-set dlm-quote dlm-lparen))
(define dlm-closers (char-set dlm-quote dlm-rparen))


;; original simple scrub
(define (nat-scrub s)
  "Normalize string S for tokenization. Change all white-
space to blanks, reduce runs of blanks to a single blank
unless they are in a Forth string or comment, and add
blanks to both ends of the incoming string to simplify
boundary handling."
  (let ((accum '(#\space))
        (lastc #\space) (currc #\space)
        (inside #f) (seeking #\nul)
        (t (string->list s)))
    (while (not (null? t))
      ;; consume next char
      (set! lastc currc) (set! currc (car t)) (set! t (cdr t))
      ;; if inside pass straight thru but watch for end
      (if inside
          (begin      ; just pass inside through
            (set! accum (cons currc accum))
            (if (char=? seeking currc)
                (begin                  ; transition to outside
                  (set! seeking #\nul)
                  (set! inside #f)))
            (continue)))
      ;;
      (if (or (char=? currc dlm-lparen) (char=? currc dlm-quote))
          (begin                ; entered a string or comment
            (set! accum (cons currc accum))
            (set! inside #t)
            (set! seeking (if (char=? currc dlm-lparen) dlm-rparen dlm-quote))
            (continue)))
      ;;
      (if (or (char=? currc #\nl) (char=? currc #\tab))
          (set! currc #\space))
      ;;
      (if (or (not (char=? currc #\space)) (not (char=? lastc currc)))
          (set! accum (cons currc accum))))
    ;; end of while, watch for needing one more space
    (if (not (char=? (car accum) #\space))
        (set! accum (cons #\space accum)))
    (list->string (reverse accum))))


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
  (set! nat-in-comment #f)
  (set! nat-in-string #f)
  (set! nat-buffer-empty #f)
  ;; call net-tokenizer until such time as nat-buffer
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
        (set! dict-entry (lookup tok-string))
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

(define (nat-next-token)
  "Get the next token out of nat-buffer and return it as
a (type . text) pair. Updates tokenizer state and buffer
but does not consume past the token.

Anything not recognized is assumed to be a word and will
be addressed later in notatil."
  (nat-trimleft-buffer)
  (cond (nat-buffer-empty
         (set! nat-buffer "")
         (cons 'nat-tok-none ""))
        ((string= "" nat-buffer)
         (set! nat-buffer-empty #t)
         (cons 'nat-tok-none ""))
        (else
         (cons (nat-buffer-token-code) (nat-buffer-to-blank)))))


(define (nat-buffer-token-code)
  "Does the first token in the buffer exist in the code
mapping table? If so, return the code or unknown."
  (letrec*
      ((f (lambda (xs)
            (cond ((null? xs) 'nat-tok-word)
                  ((nat-buffer-prefix? (car (car xs))) (cdr (car xs)))
                  (else (f (cdr xs))))
            )))
    (f nat-token-code-map)))

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
finding end of string, etc. The terminating character
is not returned or consumed."
  (let ((i (string-index nat-buffer c)) (t ""))
    (cond (i (set! t (substring nat-buffer 0 i))
             (set! nat-buffer (substring nat-buffer i)))
          (else (set! t (string-join (list  "ERROR COULD NOT FIND EXPECTED '" (list->string (list c)) "'") ""))))
    t))

(define (nat-buffer-through-char c)
  "Return the contents of the buffer up to and
including character C."
  (error "not implemented"))

(define (nat-buffer-to-token tk)
  "Return the contents of the buffer up to but not
including the string token TK."
  (error "not implemented"))

(define (nat-buffer-through-token tk)
  "Return the contents of the buffer up to and
including the string token TK."
  (error "not implemented"))

(define (nat-buffer-prefix? s)
  "DRY buffer checks."
  (= (string-length s) (string-prefix-length-ci nat-buffer s)))

;;
;; Token and type mapping table
;;
;; Not every possible word is included. Control structure
;; and definition words are, plus some repl operations.
;;
(define nat-token-code-map
  (list (cons "do " 'nat-tok-do)
        (cons "loop " 'nat-tok-loop)
        (cons "if " 'nat-tok-if)
        (cons "then " 'nat-tok-then)
        (cons "else " 'nat-tok-else)
        (cons ": " 'nat-tok-definition)
        (cons "; " 'nat-tok-end-definition)
        (cons "variable " 'nat-tok-variable)
        (cons "constant " 'nat-tok-constant)
        (cons "marker " 'nat-tok-marker)
        (cons "see " 'nat-tok-see)
        (cons "list " 'nat-tok-list)
        (cons "bye " 'nat-tok-bye)
        (cons "help " 'nat-tok-help)
        (cons "( " 'nat-tok-begin-comment)
        (cons ") " 'nat-tok-end-comment)
        ;; strings can be dormant or printing
        ;; they are closed by the first quote
        ;; after the open, not space quote, so
        ;; that space would be in the string.
        ;; 'nat-tok-string-end can't be in
        ;; this table because some forths
        ;; allow a single quote to start a
        ;; string.
        (cons ".\" " 'nat-tok-print-string)
        (cons "s\" " 'nat-tok-begin-string)
        (cons "\" " 'nat-tok-begin-string)
        ;;
        ;;
        ;;
        ))


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
       (list (cons 'nat-tok-definition ":")))

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
       (list (cons 'nat-tok-definition ":")
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
       (list (cons 'nat-tok-definition ":")
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
