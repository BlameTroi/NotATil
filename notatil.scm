;;; notatil.scm -- a forthish language in scheme
;;;
;;; Troy Brumley, March 2023. Licensed under the terms
;;; of The Unlicense. See LICENSE.org.
;;;

(use-modules (ice-9 readline)
             (ice-9 pretty-print)
             (srfi srfi-14))
;; (activate-readline)

;; Starting from a problem on Exercism, I finally find
;; the time, motivation and tools to write that Forth
;; intepreter that I've planned on for so long.
;;
;; Or something very much like one.
;;
;; Guile Scheme provides a better platform for this than
;; Python, C, or Go. I'd prefer assembler or Pascal, but
;; I don't particularly enjoy Intel x86 and Pascal tempts
;; me to use more of the language than I should. Scheme
;; is minimal and that keeps me focused.


;; The goal is a single file implementation that
;; supports terminal based development with the ability
;; to edit and "compile" source. Performance is not a
;; major concern. Simplicity of implementation and
;; correctness are.

;; The starting point for the language, that I'll call
;; notatil, is to provide the basic Forth integer stack
;; and arithmetic operators along with the ability to
;; add new operators via the usual ": <definition> ;"
;; syntax.
;;
;; Built in and user defined operators are referred to as
;; words, and their implementations are stored in a
;; dictionary that is accessed in the traditional Forth
;; manner: sequentially from newest to oldest. Most,
;; but not all, words can be redefined.


;; This is the top level for an interative session. Loop
;; until the user enters "bye".
(define (notatil-termsess)
  "The top level for a terminal interaction."
  (notatil-full-reset)
  (let ((cmdline "")
        (status "enter help for basic help."))
    (notatil-completion-status status)
    (notatil-prompt cmdline)
    (while (not (string-ci= cmdline "bye"))
      ;; to be provided
      (set! status " ok ")
      (notatil-completion-status status)
      (notatil-prompt cmdline))
    ))


(define (notatil-prompt cmdline)
  (set! cmdline (readline "> ")))


(define (notatil-completion-status status)
  (display status)
  (if (> length stack-int 0)
      (display (length stack-int)))
  (newline))

(define (notatil-test prog)
  "Evaluate a command line for testing. Does not
initialize the dictionary. Returns the stack-int."
  (notatil-test-reset)
  (notatil-tokenize prog)
  stack-int)

(define (notatil-test-reset)
  "Reset everything but the dictionary for testing."
  (clear-stacks)
  (set! compiling #f)
  (set! byebye #f)
  (set! pending-def '())
  (base-dec))

(define (notatil-tokenize prog)
  (map notatil-eval (string-split prog (char-set #\space #\tab #\nl))))

(define (notatil-eval word)
   (let ((word-func (proc-for word)))
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
      (set! compiling #t)
      (set! pending-def '()))

     ((string= ";" word)
      (set! pending-def (reverse pending-def))
      (if (token-is-numeric-literal (car pending-def) radix)
          (error 'feval "can not redefine a numeric literal via :;"
                 (car pending-def) radix (cdr pending-def)))
      (enter-into-vocabulary pending-def)
      (set! compiling #f))

     (compiling
      (set! pending-def (cons word pending-def)))

     ;; once the user says bye, skip until end
     ((string-ci= "bye" word)
      (set! byebye #t))
     (byebye )

     ;; word found in dictionaries?
     ;; idea around definitions is that any user word
     ;; execution can't
     ;; use a definition of depth less than the depth
     ;; of the current word. older words use older
     ;; definitions.
     ((not (equal? 'word-not-found word-func))
      ;;(display "executing ")(display word)(newline)
      (notatil-execute word word-func))

     ;; number in supported radix? allowing for others would be nice
     ((not (equal? #f (token-is-numeric-literal word radix)))
      ;;(display "number ")(display word)(newline)
      (push (token-is-numeric-literal word radix)))

     ;; I'm sorry Dave, I can't do that

     (else
      ;;(display "what the actual fuck ")(display word)(newline)
      (error 'notatil-eval
             "unknown or undefined word"
             word radix stack-int)))))


(define (enter-into-vocabulary pending-def)
  ;;(display "enter-into-dictionary ")(display pending-def)(newline)
  (let ((new-word (car pending-def))
        (def (cdr pending-def))
        (tokenized '()) (curr "") (proc '()))
    (while (not (null? def))
      (set! curr (car def))
      (set! proc (proc-for curr))
      (if (equal? proc 'word-not-found)
          (set! proc (token-is-numeric-literal curr radix)))
      (if (or (procedure? proc) (number? proc) (list? proc))
          (set! tokenized (cons proc tokenized))
          (error 'enter-into-dictionary "unknown word in definition :;" curr pending-def))
      (set! def (cdr def)))
    ;; (display "definition ")(display (reverse tokenized))(newline)(newline)
    (set! dictionary (cons (cons new-word (reverse tokenized)) dictionary))))


(define (notatil-execute word word-func)
  (cond
   ((null? word-func) )
   ((procedure? word-func)
    (apply word-func '()))
   ((number? word-func)
    (push word-func))
   ((list? word-func)
    (notatil-execute word (car word-func))
    (notatil-execute word (cdr word-func)))
   (else
    (error 'notatil-execute
           "error in word definition"
           word word-func))))


;; Reset the environment to a known initial state. Clear
;; stacks, restore starting dictionary, set any globals,
;; and return to base 10.
(define (notatil-full-reset)
  "Reset the environment to a clean state."
  (clear-stacks)
  (set! dictionary (list-copy core-words))
  (set! compiling #f)
  (set! byebye #f)
  (set! pending-def '())
  (base-dec))


;; Globals that aren't stacks or dictionary.
(define compiling #f)
(define byebye #f)


;;;
;;; Parse helpers.
;;;

(define (token-is-numeric-literal w b)
  "If string W is a numeric literal in base B, return the
numeric value or #f. Only hex, decimal, octal, and binary
are supported."
  (cond
     ((and (= b 16) (string-every chars-hex w))
      (string->number w b))
     ((and (= b 10) (string-every chars-dec w))
      (string->number w b))
     ((and (= b 8)  (string-every chars-oct w))
      (string->number w b))
     ((and (= b 2)  (string-every chars-bin w))
      (string->number w b))
     (else
      #f)))

;; character sets for testing strings to see if the are
;; valid digit sequences the current base.

(define chars-hex
  (char-set #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9
            #\a #\b #\c #\d #\e #\f
            #\A #\B #\C #\D #\E #\F))

(define chars-dec
  (char-set #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))

(define chars-oct
  (char-set #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7))

(define chars-bin
  (char-set #\0 #\1))


;;;
;;; Radix suppport.
;;;

;; A real Forth supports arbitrary bases. Notatil could
;; easily support bases 2 through 36 with the ten digits
;; and twenty six letters, but I'm sticking with the big
;; four bases: Hexadecimal, decimal, octal, and binary.

;; Persistent record of the current notatil base.
(define radix 10)

;; BASE? ( -- n ) Places the current base on the stack.
(define (base?) (push radix))

;; BASE  ( n -- ) Sets the base to n. Base is limited to
;;                16, 10, 8, and 2 by notatil.
(define (base)
  (check-stack 1 'base)
  (let ((b (pop)))
    (if (not (or (= 16 b) (= 10 b) (= 8 b) (= 2 b)))
        (error 'base
               "illegal base requested must be 16, 10, 8, or 2"
               b
               stack-int)
        (set! radix b))))

;; Mnemonic shortcuts to set the base to one of the
;; four standards.
(define (base-hex) (push 16) (base))
(define (base-dec) (push 10) (base))
(define (base-oct) (push 8)  (base))
(define (base-bin) (push 2)  (base))

;;;
;;; The Stacks.
;;;

;; At a minimum a Forthish language requires a couple of
;; stacks. These are not to be confused with the processor
;; or runtime stack and heap in C or assembler. Notatil
;; provides stacks for integers, reals, and call-return.

(define (clear-stacks)
  "Empty all stacks."
  (set! stack-int  '())
  (set! stack-real '())
  (set! stack-call '()))


(define stack-int  '())
(define stack-real '())
(define stack-call '())


;; Suppport beyond existence won't be provided until
;; needed, but as the integer stack and operations are
;; part of the core, here they are.

;; Add an item to the top of the stack.
(define (push n)
  (set! stack-int (cons n stack-int)))

;; Remove an item from the top of the stack.
(define (pop)
  ;; checking depth should be done elsewhere, we'll
  ;; allow a crash here
  (let ((n (car stack-int)))
    (set! stack-int (cdr stack-int)) n))

;; Called by built in words, not primitives, to catch
;; a stack underflow and report the abort.
;;
;; An optimization would be to keep a running record
;; of the stack depth instead of checking the length
;; at each call. Maybe later.
(define (check-stack n sym)
  "Throw an error if there's a stack underflow."
  (if (> n (length stack-int))
      (error
       'check-stack
       "stack underflow on op"
       sym n (length stack-int) stack-int)))

;; Stack manipulation words.

;; .S ( ? - ? ) Prints the entire stack leaving the
;;              contents unchanged.
;;
(define (dot-s)
  "The .s operator."
  ;; TODO: Radix support on number->string
  ;; TODO: Just where does the output go?
  (string-join (map number->string stack-int) " "))

;; DUP	( n — n n )	Duplicates the top stack item
(define (dup)
  (check-stack 1 'dup)
  (let ((n (pop))) (push n) (push n)))

;; DROP	( n — )	Discards the top stack item
(define (drop)
  (check-stack 1 'drop)
  (pop))

;; SWAP	( n1 n2 — n2 n1 )	Reverses the top two stack
;;                        items
(define (swap)
  (check-stack 2 'swap)
  (let ((n (pop)) (m (pop))) (push n) (push m)))

;; OVER	( n1 n2 — n1 n2 n1 )	Copies second item to top
(define (over)
  (check-stack 2 'over)
  (push (cadr stack-int))) ;; NOTE: violates api via direct access

;; TODO:
;; ROT	( n1 n2 n3 — n2 n3 n1 )	Rotates third item to top
;; 2SWAP	( d1 d2 — d2 d1 )	Reverses the top two pairs of numbers
;; 2DUP	( d — d d )	Duplicates the top pair of numbers
;; 2OVER	( d1 d2 — d1 d2 d1 )	Duplicates the second pair of numbers
;; 2DROP	( d1 d2 — d1 )	Discards the top pair of numbers

;; primitive arithmetic. these can be redefined by the user.


;; –	( n1 n2 — diff )	Subtracts (n1-n2)
(define (op-)
  (check-stack 2 '-)
  (let* ((n (pop)) (m (pop)) (r (- m n))) (push r)))

;; +	( n1 n2 — sum )	Adds
(define (op+)
  (check-stack 2 '+)
  (let* ((n (pop)) (m (pop)) (r (+ m n))) (push r)))

;; *	( n1 n2 — prod )	Multiplies
(define (op*)
  (check-stack 2 '*)
  (let* ((n (pop)) (m (pop)) (r (* m n))) (push r)))

;; /	( n1 n2 — quot )	Divides (n1/n2)
(define (op/)
  (check-stack 2 '/)
  (let* ((n (pop)) (m (pop)) (r (quotient m n))) (push r)))

;; MOD	( n1 n2 — rem )	Divides; returns remainder only
(define (mod)
  (check-stack 2 'mod)
  (let* ((n (pop)) (m (pop)) (r (remainder m n))) (push r)))

;; /MOD	( n1 n2 — rem quot )	Divides; returns remainder
;;                            and quotient
(define (/mod)
  (check-stack 2 '/mod)
  (let* ((n (pop)) (m (pop)) (r (remainder m n)) (q (quotient m n))) (push q) (push r)))


;; Logical and relational operators. As in C or Forth,
;; 0 is false and 1 is true.
;; These can be redefined

(define (forth-bool b)
  "Convert a real boolean to 1 for true, 0 for false."
  (if b 1 0))

;; <    ( n1 n2 -- n2 < n1 )
(define (op<)
  (check-stack 2 '<)
  (let* ((n (pop)) (m (pop))
         (r (< m n)))
    (push (forth-bool r))))

;; =    ( n1 n2 -- n1 = n2 )
(define (op=)
  (check-stack 2 '=)
  (let* ((n (pop)) (m (pop))
         (r (= m n)))
    (push (forth-bool r))))

;; >    ( n1 n2 -- n2 > n1 )
(define (op>)
  (check-stack 2 '>)
  (let* ((n (pop)) (m (pop))
         (r (> m n)))
    (push (forth-bool r))))

;; >    ( n1 -- !n1)
(define (op-not)
  (check-stack 1 'not)
  (let* ((n (pop)))
    (push (forth-bool (zero? n)))))


;; constants -- these can not be redefined

(define (c0)
  (push 0))

(define (c1)
  (push 1))

(define (c-1)
  (push -1))

;; find procedure to execute word

(define (proc-for word)
  (proc-for-r word dictionary))


(define (proc-for-r word words)
  (cond ((null? words) 'word-not-found)
        ((string-ci= word (car (car words))) (cdr (car words)))
        (else (proc-for-r word (cdr words)))))


;; these are words that can not be redefined
(define perm-words
  '("base" "base?"
    "hexadecimal" "hex"
    "decimal" "dec"
    "octal" "oct"
    "binary" "bin"
    "-1" "0" "1"
    "bye" "help" "load" "save"
    ".""" "." """" "variable" "constant"
    ":" ";"))

;; These are the starting words of the notatil system.
;; Think of these as primities. Some can be redefined,
;; but you probably shouldn't do that. See perm-words
;; for those that can't be redefined.
(define core-words
  (list
   ;; constants
   (cons "-1" c-1) (cons "0" c0) (cons "1" c1)

   ;; radix related, allowing some synonyms
   (cons "base" base) (cons "base?" base?) (cons "radix" base?)
   (cons "hexadecimal" base-hex) (cons "hex" base-hex)
   (cons "decimal" base-dec) (cons "dec" base-dec)
   (cons "octal" base-oct) (cons "oct" base-oct)
   (cons "binary" base-bin) (cons "bin" base-bin)

   ;; stack manipulation
   (cons "dup" dup) (cons "drop" drop) (cons "swap" swap)
   (cons "over" over)

   ;; primitive arithmetic
   (cons "+" op+) (cons "-" op-) (cons "/" op/)
   (cons "*" op*) (cons "mod" mod) (cons "/mod" /mod)

   ;; logical operations
   (cons "<" op<) (cons "=" op=) (cons ">" op>) (cons "not" op-not)

   ;; some special words
   (cons ".s" dot-s)
   ;; definition directives are hard coded in main loop
   ))

;; A pending definition is assembled here.
(define pending-def '())

;; This is the current dictionary. When the system starts
;; the core-words are copied. New definitions are added
;; to the head of the list. All word searches start from
;; the head of the list. This prevents a redefinition of
;; a word from changing the behavior of older words that
;; were compiled with an older definition.
(define dictionary '())

;;

;; vocab definition from brodie's forth book
;; +	( n1 n2 — sum )	Adds
;; –	( n1 n2 — diff )	Subtracts (n1-n2)
;; *	( n1 n2 — prod )	Multiplies
;; /	( n1 n2 — quot )	Divides (n1/n2)
;; /MOD	( n1 n2 — rem quot )	Divides; returns remainder and quotient
;; MOD	( n1 n2 — rem )	Divides; returns remainder only
