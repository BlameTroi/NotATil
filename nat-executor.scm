;;; begin nat-executor.scm -- The Not A Threaded interpreted Language --
;;;
;;; By Troy Brumley, 2023. I've placed this software under
;;; the unlicense license. See the LICENSE.org or UNLICENSE
;;; files for more details. TL;DR -- released to the public
;;; domain.


;;;
;;; The tokenize-parse-compile-execute phases are not as
;;; distinct as in a true compiled language. Here are the
;;; functions related to actual execution of NotATil code.
;;;


;;
;; Evaluate (interpret, add to dictionary if needed) the
;; tokenized buffer.
;;
(define (nat-evaluate)
  "Sequentially process the tokens in nat-tokenized. Both
immediate and definitional tokens can be in the buffer. The
definitional tokens actually 'execute' in a compile mode to
update the nat-dictionary.

The tokens are presented in a vector which makes it easier
to backtrack when dealing with looping constructs."
  ;; vector-ref vector-set! vector-length
  ;; remember that you can't reference var1 in
  ;; var2 and expect it's value to be updated,
  ;; the vars are updated from base to exp in
  ;; parallel
  ;; (do ((var1 base1 exp1) (var2 base2 exp2) ...)
  ;;   ((test? ...) final-exp)
  ;;  side-effecting-statements ...)
  (let* ((err #f)
         (len (vector-length nat-tokenized))
         ;; current token, last token
         (tp (cons 'nat-tok-none "")) (tt 'nat-tok-none) (tw "") ; this ...
         (lp (cons 'nat-tok-none "")) (lt 'nat-tok-none) (lw "") ; last ...
         (in-def #f)             ; TODO: maybe switch to global nat-*?
         (in-comment #f) (in-string #f)
         (new-word "" )
         (i 0)
         (j 0))
    ;; (define (dbg w)
    ;;   ;; deug prints, i'm old fashioned
    ;;   (display "<==") (display w) (display "==>") (newline)
    ;;   (display "  i = ") (display i) (newline)
    ;;   (display " lp = ") (display lp) (newline)
    ;;   (display " tp = ") (display tp) (newline)
    ;;   (display " def:") (display in-def)
    ;;   (display " cmp:") (display nat-compiling)
    ;;   (display " cmt:") (display in-comment)
    ;;   (display " str:") (display in-string)
    ;;   (newline)
    ;;   )
    ;;
    ;; Advance through tokens. Some processing below will consume multiple
    ;; tokens.
    ;;
    (while (and (not err) (not nat-terminating) (< i len))
      ;; remember prior
      (set! lp tp) (set! lt tt) (set! lw tw)
      ;; get next token
      (set! tp (vector-ref nat-tokenized i))
      (set! tt (car tp)) (set! tw (cdr tp))
      ;; cond works nicely as a case structure here
      (cond
       ;; comments are just whitespace so skip them directly
       ((equal? tt 'nat-tok-begin-comment)
        (if (and  (< (+ 2 i) len)
                  (equal? (car (vector-ref nat-tokenized (+ 2 i))) 'nat-tok-end-comment))

            (begin ; must be followed by comment and end comment, advance
              ;; i + 2 is a end coment so just advance
              (set! i (+ 2 i))
              (set! tp (vector-ref nat-tokenized i))
              (set! tt (car tp))
              (set! tw (cdr tp)) )
            ;; i + 2 was not an end comment and may not even exist, throw error
            (error 'nat-evaluate "illegal comment, missing close comment" tp i (dump-five-around i))))
       ((equal? tt 'nat-tok-end-definition)
        ;; (dbg "new word end")
        (set! nat-pending-def (reverse nat-pending-def))
        (if (token-is-integer-literal (cdr (car nat-pending-def)) radix)
            (error 'nat-evaluate "can not redefine numeric literal via :;" tp i (dump-five-around i)))
        (nat-add-new-word)
        (set! nat-compiling #f)
        )
       ;; start of a new word definition
       ((equal? tt 'nat-tok-begin-definition)
        ;; (dbg "new word start")
        (if nat-compiling
            (error 'nat-evaluate "illegal nesting of colon definition" tp i (dump-five-around i)))
        (set! nat-compiling #t)
        (set! nat-pending-def '())
        )
       ;;
       (nat-compiling
        ;; (dbg "add to new word def")
        (if (equal? lt 'nat-tok-begin-definition)
            (begin                      ; start of new word definition
              (set! new-word tw)))
        (set! nat-pending-def (cons tp nat-pending-def))
        )
       ;; check for entering a defintion, possible new
       ;; word compilation
       ;; ((member tt nat-definition-tokens)
       ;;  (set! in-def #t)
       ;;  (if (eq? tt 'nat-tok-begin-definition)
       ;;      (set! nat-compiling #t))
       ;;  ;; variable, constant, marker, cells allot, and?
       ;;  )
       ;; if this is an immediate word just execute it.
       ;; the only thing that *should* make it to here are
       ;; immediate words.
       ((member tt nat-immediate-tokens)
        (nat-exec tw))
       ;; otherwise report that something is out of whack
       (else (display "nat-evaluate: token not handled. We have a hole in the bucket.") (newline)
             (display "       token: ") (display tp) (newline)
             (display "     context: ") (display  (dump-five-around i)) (newline)
       (display tt) (newline)
       (display nat-immediate-tokens) (newline)
       (display tp) (newline)
       (display tw) (newline)
       (display "testing") (newline))
       ;; end cond
       )
      ;; next token
      (set! i (1+ i)))
    ;;
    )
  ;;
  )

;;
;; Evaluate a token (or word) from the command line. For
;; simple operators (stack manipulation, numeric literals,
;; numeric or relational operators) the execution is
;; immediate. Either push the literal onto a stack or
;; execute the word's definition.
;;
;; Every incoming token is checked against the dictionary.
;; If it is found, the definition is kept for execution
;; or inclusion in a future definition.
;;
;; If a new word is being defined via the : newword
;; definition ; syntax, execution is suspended and
;; instead the literals or word functions are buffered
;; until the definition is complete.
;;
;; Then the new word and its definition are added to the
;; front of the dictionary.
;;
(define (nat-exec word)
  (let ((definition (nat-lookup word)))
    (cond
     ;; empty string happens when multiple delimiters hit on split
     ((string= "" word) )

     ;; ": blah ;" defines a new word, compiling is a
     ;; generous description. accumulate everything
     ;; between : and ; and build something we can
     ;; expand and execute. most words can be redefined
     ;; but there are some critical exceptions in the
     ;; in the perm-words list.
     ((string= ":" word)
      (set! nat-compiling #t)
      (set! nat-pending-def '()))

     ((string= ";" word)
      (set! nat-pending-def (reverse nat-pending-def))
      (if (token-is-integer-literal (car nat-pending-def) radix)
          (error 'feval "can not redefine a numeric literal via :;"
                 (car nat-pending-def) radix (cdr nat-pending-def)))
      (nat-add-new-word)
      (set! nat-compiling #f))

     (nat-compiling
      (set! nat-pending-def (cons word nat-pending-def)))

     ;; once the user says bye, skip until end
     ((string-ci= "bye" word)
      (set! nat-terminating #t))
     (nat-terminating )

     ;; word found in dictionaries?
     ;; idea around definitions is that any user word
     ;; execution can't
     ;; use a definition of depth less than the depth
     ;; of the current word. older words use older
     ;; definitions.
     ((not (equal? 'nat-word-not-found definition))
      (nat-execute word (entry-proc definition)))

     ;; number in supported radix?
     ((not (equal? #f (token-is-integer-literal word radix)))
      (push (token-is-integer-literal word radix)))

     ;; I'm sorry Dave, I can't do that

     (else
      (display "nat-exec word not found ") (display word) (newline)
      ;; (error 'nat-eval
      ;;        "unknown or undefined word"
      ;;        word radix stack-data)
      ))))


;;
;; The evaluation process above looks up a word in the
;; dictionary to find its definition. For built in words
;; such as SWAP or DUP, the definition is a single
;; procedure reference to the Scheme function that
;; implements the word. User defined words are stored as
;; lists of procedure references. A user defined word can
;; include numeric literals and they are pushed directly
;; onto the stack.
;;
(define (nat-execute word proc)
  "Execute WORD by running it's DEFINITION."
  (cond
   ((null? proc) )
   ((procedure? proc)
    (apply proc '()))
   ((integer? proc)
    (push proc))
   ((list? proc)
    (nat-execute word (car proc))
    (nat-execute word (cdr proc)))
   (else
    (error 'nat-execute
           "error in word definition"
           word proc))))



;;;
;;; end nat-executor.scm -- The Not A Threaded interpreted Language --
