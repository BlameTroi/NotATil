;;; notatil.scm -- a forthish language in scheme
;;;
;;; Troy Brumley, March 2023. Licensed under the terms
;;; of The Unlicense. See LICENSE.org.
;;;

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
  (let ((cmdline ""))
    (notatil-prompt cmdline)
    (while (not (string-ci= cmdline "bye"))
      ;; to be provided
      (notatil-prompt cmdline))
    ))


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
  (let (b (pop))
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
