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


;;
;; This is the top level for an interative session. Loop
;; until the user enters "bye".
;;
;; Note that readline and emacs with geiser don't play well
;; together so readline is not activated by default. Use
;; the notatil-test function for testing under geiser.
;;
(define (notatil-termsess)
  "The top level for a terminal interaction."
  (notatil-full-reset)
  (let ((cmdline "")
        (status "enter help for basic help."))
    (notatil-completion-status status)
    (set! cmdline (notatil-prompt))
    (while (not byebye)
      (notatil-tokenize cmdline) ;; needs more
      ;; to be provided
      (set! status " ok ")
      (notatil-completion-status status)
      (if (not byebye)
          (set! cmdline (notatil-prompt)))))
  (newline))


;;
;; This is the top level for testing. Call with a string
;; argument holding notatil code. This will allow scriptable
;; testing and simple debugging.
;;
(define (notatil-test prog)
  "Evaluate a command line for testing. Only initializes
the dictionary if it is empty. Returns the stack."
  (notatil-test-reset)
  (notatil-tokenize prog)
  stack-data)

(define (notatil-test-clear-dictionary prog)
  "Test entry for unit test scripting, does a full reset."
  (notatil-full-reset)
  (notatil-tokenize prog)
  stack-data)

;;;
;;; User interaction helpers
;;;

(define (notatil-prompt)
  "Display prompt to the user and accept commands."
  (readline "> "))


(define (notatil-completion-status status)
  "Display the status of the last commands and report
the length of the stack to the user."
  (display status)
  (if (> (length stack-data) 0)
      (display (length stack-data)))
  (newline))

;;;
;;; Parse, evaluate, compile to dictionary, and
;;; execution.
;;;


;;
;; The rules for tokenizing a Forth style language are dead
;; simple. Whitespace separates tokens. There are no special
;; characters.
;;

;;
;; Break the input into tokens. The string-split functin can
;; return empty string tokens if multiple delimiters are read
;; in succession but the evaluator ignores them.
;;

(define (notatil-tokenize prog)
  (map notatil-eval
       (string-split prog (char-set #\space #\tab #\nl))))

;;
;; Evaluate a token (or word) from the command line. For simple
;; operators (stack manipulation, numeric literals, numeric or
;; relational operators) the execution is immediate. Either push
;; the literal onto the stack or execute the word's definition.
;;
;; Every incoming token is checked against the dictionary. If it
;; is found, the definition is kept for execution or inclusion
;; in a future definition.
;;
;; If a new word is being defined via the : newword definition ;
;; syntax, execution is suspended and instead the literals or
;; word functions are buffered until the definition is complete.
;; Then the new word and its definition are added to the front
;; of the dictionary.
;;
(define (notatil-eval word)
   (let ((definition (lookup word)))
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
      (add-new-word pending-def)
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
     ((not (equal? 'word-not-found definition))
      (notatil-execute word (entry-proc definition)))

     ;; number in supported radix?
     ((not (equal? #f (token-is-numeric-literal word radix)))
      (push (token-is-numeric-literal word radix)))

     ;; I'm sorry Dave, I can't do that

     (else
      (error 'notatil-eval
             "unknown or undefined word"
             word radix stack-data)))))


;;
;; The evaluation process above looks up a word in the dictionary to
;; find its definition. For built in words such as SWAP or DUP, the
;; definition is a single procedure reference to the Scheme function
;; that implements the word. User defined words are stored as lists
;; of procedure references. A user defined word can include numeric
;; literals and they are pushed directly onto the stack.
;;
(define (notatil-execute word proc)
  "Execute WORD by running it's DEFINITION."
  (cond
   ((null? proc) )
   ((procedure? proc)
    (apply proc '()))
   ((integer? proc)
    (push proc))
   ((list? proc)
    (notatil-execute word (car proc))
    (notatil-execute word (cdr proc)))
   (else
    (error 'notatil-execute
           "error in word definition"
           word proc))))


;; Reset the environment to a known initial state. Clear
;; stacks, restore starting dictionary, set any globals,
;; and return to base 10.
(define (notatil-full-reset)
  "Reset the environment to a clean state."
  (clear-stacks)
  (dictionary-build)
  (set! compiling #f)
  (set! byebye #f)
  (set! pending-def '())
  (base-dec))


;; Reset the environment for testing. Protects any
;; updates to the dictionary made on prior calls, if
;; any.
(define (notatil-test-reset)
  "Reset everything but the dictionary for testing."
  (clear-stacks)
  (if (zero? (length dictionary))
      (dictionary-build))
  (set! compiling #f)
  (set! byebye #f)
  (set! pending-def '())
  (base-dec))


;;;
;;; Globals that aren't stacks or dictionary.
;;;

(define compiling #f)
(define byebye #f)


;;;
;;; Parse helpers.
;;;

(define (token-is-numeric-literal w b)
  "If string W is a numeric literal in base B, return the
numeric value or #f. Only hex, decimal, octal, and binary
are supported."
  (let ((d (substring w 1))
        (f (lambda (s)
             (cond
              ((and (= b 16) (string-every chars-hex s))
               (string->number w b))
              ((and (= b 10) (string-every chars-dec s))
               (string->number w b))
              ((and (= b 8)  (string-every chars-oct s))
               (string->number w b))
              ((and (= b 2)  (string-every chars-bin s))
               (string->number w b))
              (else
               #f)))))
    (cond
     ((string= "-" (substring w 0 1)) (f d))
     (else (f w)))))


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
               stack-data)
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
  (set! stack-data   '())
  (set! stack-float  '())
  (set! stack-return '()))


(define stack-data   '())  ;; predicates number? integer? exact?
(define stack-float  '()) ;; predicates number? real? inexact?
(define stack-return '())


;; Suppport beyond existence won't be provided until
;; needed, but as the integer stack and operations are
;; part of the core, here they are.


;; Add an item to the top of the stack.
(define (push n)
  (set! stack-data (cons n stack-data)))


(define (push-r n)
  (set! stack-return (cons n stack-return)))


(define (push-f n)
  (set! stack-float (cons n stack-float)))


;; Remove an item from the top of the stack.
;; checking depth should be done elsewhere, we'll
;; allow a crash here
(define (pop)
  (let ((n (car stack-data)))
    (set! stack-data (cdr stack-data)) n))


(define (pop-r)
  (let ((n (car stack-return)))
    (set! stack-return (cdr stack-return)) n))


(define (pop-f)
  (let ((n (car stack-float)))
    (set! stack-float (cdr stack-float)) n))


;; Called by built in words, not primitives, to catch
;; a stack underflow and report the abort.
;;
;; An optimization would be to keep a running record
;; of the stack depth instead of checking the length
;; at each call. Maybe later.
(define (check-stack n sym)
  "Throw an error if there's a stack underflow."
  (if (> n (length stack-data))
      (error
       'check-stack
       "stack underflow on op"
       sym n (length stack-data) stack-data)))


(define (check-return n sym)
  "Throw an error if there's a return stack underflow."
  (if (> n (length stack-return))
      (error
       'check-return
       "return stack underflow on op"
       sym n (length stack-return) stack-return)))


(define (check-float n sym)
  "Throw an error if there's a float stack underflow."
  (if (> n (length stack-float))
      (error
       'check-float
       "float stack underflow on op"
       sym n (length stack-float) stack-float)))


;;
;; Simple output.
;;
;; TODO: Radix support on number->string
;; TODO: Just where does the output go?
;;


;; .S ( ? -- ? ) Prints the entire stack leaving the
;;              contents unchanged.
(define (dot-s)
  "The .s operator."
  (display (string-join (map number->string stack-data) " ")))


;; .R ( n w -- ) Print n right justified in w spaces.
;;               If there are more digits in n than
;;               allowed for by w, print them anyway.
(define (dot-r)
  (check-stack 2 '.R)
  (let ((w (pop)) (n (pop)) (s ""))
    (set! s (number->string n radix))
    (if (< (string-length s) w)
      (set! s (string-pad s w)))
    (display s)))


;; . ( n -- ) Prints the top of the stack in the current radix.
(define (dot)
  (check-stack 1 '.)
  (display (number->string (pop) radix))
  (display #\space))


;; space ( -- ) Print a single space.
(define (space)
  (display #\space))


;; spaces ( n -- ) Print n spaces.
(define (spaces)
  (check-stack 1 'spaces)
  (display (string-pad "" (pop))))


;; cr ( -- ) Prints a carriage return.
(define (cr)
  (newline))


;; emit ( c -- ) Prints the top of the stack as a character.
(define (emit)
  (check-stack 1 'emit)
  (display (integer->char (pop))))


;; ." ( -- ) Prints everything up to but not included a trailing
;;           double quote.
;;           TODO: not implemented yet.
(define (dot-quote)
  (display "dot-quote not yet implemented."))


;; key ( -- c) Accept single character input
(define (key)
  (display "key not yet implemented."))


;;
;; Stack manipulation words.
;;


;; DUP	( n — n n )	Duplicates the top stack item
(define (dup)
  (check-stack 1 'dup)
  (let ((n (pop)))
    (push n) (push n)))


;; DROP	( n — )	Discards the top stack item
(define (drop)
  (check-stack 1 'drop)
  (pop))


;; SWAP	( n1 n2 — n2 n1 )	Reverses the top two stack
;;                        items
(define (swap)
  (check-stack 2 'swap)
  (let ((n2 (pop)) (n1 (pop)))
    (push n2) (push n1)))


;; OVER	( n1 n2 — n1 n2 n1 )	Copies second item to top
(define (over)
  (check-stack 2 'over)
  (let ((n2 (pop)) (n1 (pop)))
    (push n1) (push n2) (push n1)))


;; >r   ( n --) [ -- n ]  Move an item from data to return
(define (to-r)
  (check-stack 1 '>r)
  (push-r (pop)))


;; r>   ( -- n1) [ n1 -- ]  Move an item from return to data
(define (from-r)
  (check-return 1 'r>)
  (push (pop-r)))


;; r@   ( -- n1) [ n1 -- n1] Copy an item from return to data
;; r    ( -- n1) [ n1 -- n1] Same as r@, an older name
(define (fetch-r)
  (check-return 1 'r@)
  (let ((n1 (pop-r)))
    (push n1)
    (push-r n1)))


;; ROT	( n1 n2 n3 — n2 n3 n1 )	Rotates third item to top
(define (rot)
  (check-stack 3 'rot)
  (let ((n3 (pop)) (n2 (pop)) (n1 (pop)))
    (push n2)
    (push n3)
    (push n1)))


;; 2SWAP	( d1 d2 — d2 d1 )	Reverses the top two pairs of numbers
(define (2swap)
  (check-stack 4 '2swap)
  (let ((d2b (pop)) (d2a (pop)) (d1b (pop)) (d1a (pop)))
    (push d2a)(push d2b)
    (push d1a)(push d1b)))


;; 2DUP	( d — d d )	Duplicates the top pair of numbers
(define (2dup)
  (check-stack 2 '2dup)
  (let ((da (pop)) (db (pop)))
    (push da)(push db)
    (push da)(push db)))


;; 2OVER	( d1 d2 — d1 d2 d1 )	Duplicates the second pair of numbers
(define (2over)
  (check-stack 4 '2over)
  (let ((d2b (pop)) (d2a (pop)) (d1b (pop)) (d1a (pop)))
    (push d1a)(push d1b)
    (push d2a)(push d2b)
    (push d1a)(push d1b)))


;; 2DROP	( d1 d2 — d1 )	Discards the top pair of numbers
(define (2drop)
  (check-stack 2 '2drop)
  (pop)(pop))


;;
;; see also http://forth.org/svfig/Len/softstak.htm
;;
;; in stack comments right most is top most!
;;


;; primitive arithmetic. these can be redefined by the user.


;; –	( n1 n2 — diff )	Subtracts (n1-n2)
(define (op-)
  (check-stack 2 '-)
  (let* ((n2 (pop)) (n1 (pop))
         (r (- n1 n2)))
    (push r)))


;; +	( n1 n2 — sum )	Adds
(define (op+)
  (check-stack 2 '+)
  (let* ((n2 (pop)) (n1 (pop))
         (r (+ n1 n2)))
    (push r)))


;; *	( n1 n2 — prod )	Multiplies
(define (op*)
  (check-stack 2 '*)
  (let* ((n2 (pop)) (n1 (pop))
         (r (* n1 n2)))
    (push r)))


;; /	( n1 n2 — quot )	Divides (n1/n2)
(define (op/)
  (check-stack 2 '/)
  (let* ((n2 (pop)) (n1 (pop))
         (r (quotient n1 n2)))
    (push r)))


;; MOD	( n1 n2 — rem )	Divides; returns remainder only
(define (mod)
  (check-stack 2 'mod)
  (let* ((n2 (pop)) (n1 (pop))
         (r (remainder n1 n2)))
    (push r)))


;; /MOD	( n1 n2 — rem quot )	Divides; returns remainder
;;                            and quotient
(define (/mod)
  (check-stack 2 '/mod)
  (let* ((n2 (pop)) (n1 (pop))
         (r (remainder n1 n2)) (q (quotient n1 n2)))
    (push q) (push r)))



;; Casting booleans between Forth (-1, 0) and Scheme (#t, #f).
;; Also make sure a Forth true value is -1.

(define (canonical-bool n)
  "Anything not zero in Forth is true and should be
converted to the proper true value of -1."
  (if (= n 0) 0 -1))

(define (forth-bool b)
  "Convert a Scheme #t or #f to -1 or 0."
  (if b -1 0))

(define (scheme-bool n)
  "Convert a Forth -1 or 0 to #t or #f."
  (if (zero? n) #f #t))


;; Logical and relational operators. As in C or Forth,
;; 0 is false and anything else is true. Forth returns
;; -1 from its relational checks.

;; <    ( n1 n2 -- n2 < n1 )
(define (op<)
  (check-stack 2 '<)
  (let* ((n2 (pop)) (n1 (pop))
         (r (< n1 n2)))
    (push (forth-bool r))))


;; =    ( n1 n2 -- n1 = n2 )
(define (op=)
  (check-stack 2 '=)
  (let* ((n2 (pop)) (n1 (pop))
         (r (= n2 n1)))
    (push (forth-bool r))))


;; <> ( n1 n2 -- n1 != n2 )
(define (op<>)
  (check-stack 2 '<>)
  (let* ((n2 (pop)) (n1 (pop))
         (r (not (= n1 n2))))
    (push (forth-bool r))))


;; >    ( n1 n2 -- n2 > n1 )
(define (op>)
  (check-stack 2 '>)
  (let* ((n2 (pop)) (n1 (pop))
         (r (> n1 n2)))
    (push (forth-bool r))))


;; 0= ( n1 -- n1 = 0)
(define (op0=)
  (check-stack 1 '0=)
  (let* ((n1 (pop))
        (r (= 0 n1)))
    (push (forth-bool r))))


;; 0< ( n1 -- n1 < 0)
(define (op0<)
  (check-stack 1 '0<)
  (let* ((n1 (pop))
        (r (< n1 0)))
    (push (forth-bool r))))


;; 0> ( n1 -- n1 > 0)
(define (op0>)
  (check-stack 1 '0>)
  (let* ((n1 (pop))
        (r (> n1 0)))
    (push (forth-bool r))))


;; ?DUP ( n1 -- n1 n1 ) but only if n1 is not zero
(define (?dup)
  (check-stack 1 '?dup)
  (let* ((n1 (pop))
         (r (not (zero? n1))))
    (push n1)
    (if r (push n1))))


;; not   ( n1 -- !n1)
(define (op-not)
  (check-stack 1 'not)
  (let* ((n1 (pop)))
    (push (forth-bool (zero? n1)))))


;; and   ( n1 n2 -- n1&n2)
(define (op-and)
  (check-stack 2 'and)
  (let* ((n2 (canonical-bool (pop))) (n1 (canonical-bool (pop))))
    (push (forth-bool (and (scheme-bool n1) (scheme-bool n2))))))


;; or    ( n1 n2 -- n1|n2)
(define (op-or)
  (check-stack 2 'or)
  (let* ((n2 (canonical-bool (pop))) (n1 (canonical-bool (pop))))
    (push (forth-bool (or (scheme-bool n1) (scheme-bool n2))))))


;;;
;;; Dictionary lookup and addition.
;;;


;;
;; Returns the dictionary entry for word or 'word-not-found.
;;
(define (lookup word)
  "Return the definition to execute WORD from the dictionary, or
'word-not-found."
  (letrec* ((f (lambda (w d)
                 (cond ((null? d) 'word-not-found)
                       ((string-ci= w (entry-name (car d))) (car d))
                       (else (f w (cdr d)))))))
    (f word dictionary)))


;;
;; A pending definition is a list of word tokens and possibly some
;; numeric literals. The first element of the list is the new word,
;; and subsequent elements comprise the definition.
;;
;; Iterate through the definition and for every element that exists
;; in the current dictionary, copy its definition and add it to the
;; new word being defined.
;;
(define (add-new-word pending-def)
  "Add the new word and it's definition from PENDING-DEF to the
dictionary."
  (if (member (string-downcase (car pending-def)) perm-words)
      (error 'add-new-word "some core words can not be redefined" pending-def))
  (let ((new-word (car pending-def))
        (def (cdr pending-def))
        (tokenized '()) (curr "") (curr-def '()) (proc '()))
    (while (not (null? def))
      (set! curr (car def))
      (set! curr-def (lookup curr))
      (if (equal? curr-def 'word-not-found)
          (set! proc (token-is-numeric-literal curr radix))
          (set! proc (entry-proc curr-def)))
      (if (or (procedure? proc) (integer? proc) (list? proc))
          (set! tokenized (cons proc tokenized))
          (error 'add-new-word "unknown word in definition :;" curr pending-def))
      (set! def (cdr def)))
    (set! dictionary (cons (entry-build new-word 'user-word (reverse tokenized)) dictionary))))


;; These are words that can not be redefined. Their operation is
;; too fundamental to the assumptions in notatil.
(define perm-words
  '(
    ;; all your base are belong to us
    "base" "base?"
    "hexadecimal" "hex"
    "decimal" "dec"
    "octal" "oct"
    "binary" "bin"

    ;; these aren't really words, but i can't let you redefine them
    "-1" "0" "1"

    ;; running the repl
    "bye" "help" "load" "save" "see" "block" "list" "edit"

    ;; return stack manipulation
    "r" "r@" ">r" "r>"
    "do" "loop" "+loop" "i" "j"

    ;; defining words
    "variable" "constant"
    "cells" "alloc"
    ":" ";"
    "forget" "marker"

    "(" ")"

    ))
;;;
;;; Table of handy approximations from Forth book, fixed point ways to use
;;; floating point constants.
;;;

;; so a forth word for these would look like ...
;; : *pi ( n1 -- n1*pi ) 355 * 113 / ;
;;   Number          Approximation            Error
;; π = 3.141 …       355 / 113                8.5 x 10-8
;; π = 3.141 …       1068966896 / 340262731   3.0 x 10-18
;; √2 = 1.414 …      19601 / 13860            1.5 x 10-9
;; √3 = 1.732 …      18817 / 10864            1.1 x 10-9
;; e = 2.718 …       28667 / 10546            5.5 x 10-9
;; √10 = 3.162 …     22936 / 7253             5.7 x 10-9
;; 12√2 = 1.059 …    26797 / 25293            1.0 x 10-9
;; log(2) / 1.6384 = 0.183 …      2040 / 11103  1.1 x 10-8
;; ln(2) / 16.384 = 0.042 …       485 / 11464   1.0 x 10-7

;; the 10-blah are "10 raised to -blah" so 8.5e-8 is the error on first pi


;; These are the starting words of the notatil system.
;; Think of these as primities. Some can be redefined,
;; but you probably shouldn't do that. See perm-words
;; for those that can't be redefined.
;;
;; These are not complete dictionary entries, just the
;; minimum information needed to built real entries via
;; dictionary-build.
(define core-words
  (list

   ;; radix related, allowing some synonyms
   (cons "base" base) (cons "base?" base?) (cons "radix" base?)
   (cons "hexadecimal" base-hex) (cons "hex" base-hex)
   (cons "decimal" base-dec) (cons "dec" base-dec)
   (cons "octal" base-oct) (cons "oct" base-oct)
   (cons "binary" base-bin) (cons "bin" base-bin)

   ;; stack manipulation
   (cons "dup" dup) (cons "drop" drop) (cons "swap" swap)
   (cons "over" over)

   (cons "2dup" 2dup) (cons "2drop" 2drop) (cons "2swap" 2swap)
   (cons "2over" 2over)

   (cons "rot" rot)
   (cons "?dup" ?dup)

   (cons ">r" to-r) (cons "r>" from-r)
   (cons "r@" fetch-r) (cons "r" fetch-r)

   ;; primitive arithmetic
   (cons "+" op+) (cons "-" op-) (cons "/" op/)
   (cons "*" op*) (cons "mod" mod) (cons "/mod" /mod)

   ;; logical operations
   (cons "<" op<) (cons "=" op=) (cons ">" op>) (cons "<>" op<>)
   (cons "0>" op0>) (cons "0<" op0<) (cons "0=" op0=)
   (cons "and" op-and) (cons "or" op-or) (cons "not" op-not)

   ;; simple output
   (cons ".s" dot-s) (cons "." dot) (cons "cr" cr)
   (cons "emit" emit) (cons ".#\"" dot-quote)
   (cons ".r" dot-r) (cons "space" space) (cons "spaces" spaces)

   ;; simple input
   (cons "key" key)
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

;; Dictionary format is a lifo stack implemented as an
;; consed list of pairs. All lokups are sequential. Words
;; and variables are both stored here, but variable array
;; symantics are still not defined.
;;
;; original:
;;
;; ( entry name . procedure ref, variable ref, or list of procedure refs)
;;
;; new:
;;
;; ( entry name . ( entry type . procedure ref, variable ref, or list of procedure refs)
;;
;; TODO: source of definition would be nice. this format
;;       does not lend itself to meaningful "disassembly"

;; Variables will be stored in the dictionary as well.
;; This is going to require that we put type codes on
;; dictionary entries.
(define entry-types '(core-word user-word user-var))
(define (entry-name w) (car w))
(define (entry-type w) (car (cdr w)))
(define (entry-proc w) (cdr (cdr w)))
(define (entry-build n t p)
  (cons n (cons t p)))

(define (dictionary-build)
  (set! dictionary '())
  (letrec* ((n '()) (t 'core-word) (p '())
            (f (lambda (xs)
                 (cond ((null? xs) )
                       (else (set! n (car (car xs)))
                             (set! p (cdr (car xs)))
                             (set! dictionary (cons (entry-build n t p) dictionary))
                             (f (cdr xs)))))))
               (f core-words)))


;;;
;;; words still to do
;;;

;; if words then
;; conditional execution of statements if the top of the
;; stack is true. unlike pascal, then here means endif or
;; fi. it closes the conditional body.

;; if words else other words then
;; adds an else branch to if then.

;; do words loop
;; and i (as in i j k etc)
;; a for or counting loop. as in
;; 10 0 do i . loop
;; will print 0 to 9
;; i provides the current loop index to the stack.
;; j if nested
;; http://forth.org/svfig/Len/softstak.htm

;; begin words until loop
;; do the words and when until is reached, check stack for
;; true. if not true, run the loop again
;;
;;
;; variable names a cell in storage
;; ! stores the top of the stack into the named variable
;; @ retrieves the value of the named variable and puts
;; it on the stack
;; ? is defined as "@ ."
;; +! adds and stores, and could be defined as "@ + !"

;; add source of definition to the dictionary entry

;; disk usage and dictionary cleanout from forth brodie site
;; USE xxx
;; USING xxx	( –)	Use file xxx as Forth “disk”
;; LIST	( n — )	Lists a disk block
;; LOAD	( n — )	Loads a disk block
;; ( xxx)	( — )	Ignores text up to “)” delimeter
;; UPDATE	( — )	Mark most recent block as updated
;; EMPTY-BUFFERS	( — )	Marks all block buffers as empty
;; BLOCK	( n — addr )	Return address of buffer for block n
;; INCLUDE xxx	( — )	Load the text file xxx
;; FORGET xxx	( — )	Forget definitions back through xxx
;; MARKER xxx	( — )	Defines marker xxx to roll back dictionary

;; initial logic and decisions from forth brodie site
;; IF( flag — )
;; If flag is true (non-zero) executes xxx; otherwise executes yyy; continues execution with zzz. The phrase ELSE yyy is optional.
;; IF xxx THEN zzz
;; IF xxx ELSE yyy THEN zzz
;; =( n1 n2 — flag )
;; Returns true if n1 and n2 are equal.
;; <>( n1 n2 — flag )
;; Returns true if n1 and n2 are not equal.
;; <( n1 n2 — flag )
;; Returns true if n1 is less than n2.
;; >( n1 n2 — flag )
;; Returns true if n1 is greater than n2.
;; U<( u1 u2 — flag )
;; Returns true if u1 is less than u2.
;; U>( n1 n2 — flag )
;; Returns true if u1 is greater than u2.
;; 0=( n — flag )
;; Returns true if n is zero.
;; 0<( n — flag )
;; Returns true if n is negative.
;; 0>( n — flag )
;; Returns true if n is positive.
;; AND( n1 n2 — n3 )
;; Returns the logical AND.
;; OR( n1 n2 — n3 )
;; Returns the logical OR.
;; ?DUP( n — n n ) or ( 0 — 0 )
;; Duplicates only if n is non-zero.
;; ABORT” xx”( flag — )
;; If the flag is true, types out an error message, followed by the text. Also clears the stacks and returns control to the terminal. If false, takes no action.

;; fixed point in greater detail from forth brodie
;; 1+( n1 — n2 )
;; Adds one.
;; 1-( n1 — n2 )
;; Subtracts one.
;; 2+( n1 — n2 )
;; Adds two.
;; 2-( n1 — n2 )
;; Subtracts two.
;; 2*( n1 — n2 )
;; Multiplies by two (arithmetic left shift).
;; 2/( n1 — n2 )
;; Divides by two (arithmetic right shift).
;; ABS( n1 — n2 )
;; Returns the absolute value.
;; NEGATE( n1 — n2 )
;; Changes the sign.
;; MIN( n1 n2 — n3 )
;; Returns the minimum.
;; MAX( n1 n2 — n3 )
;; Returns the maximum.
;; >R( n — )
;; Takes a value off the parameter stack and pushes it onto the return stack.
;; R>( — n )
;; Takes a value off the return stack and pushes it onto the parameter stack.
;; R@( — n )
;; Copies the top item from return stack and pushes it onto the parameter stack.
;; */( n1 n2 n3 — n4 )
;; Multiplies, then divides (n1*n2/n3). Uses a double-length intermediate result.
;; */MOD( n1 n2 n3 — n4 n5 )
;; Multiplies, then divides (n1*n2/n3). Returns the remainder (n4) and the quotient (n5). Uses a double-length intermediate result.


;; VARIABLE xxx( — ) Creates a variable named xxx; the word xxx returns its address when executed.
;; xxx: ( — addr )   *not suppporting* another form of VARIABLE xxxx
;; !( n addr — ) Stores a single-length number into the address.
;; @( addr — n ) Fetches that value at addr.
;; ?( addr — ) Prints the contents of the address, followed by one space.
;; +!( n addr — ) Adds a single-length number to the contents of the address.
;; CONSTANT xxx( n — ) Creates a variable named xxx with the value n; the word xxx returns n when executed.
;; xxx: ( — n ) *not supporting* another form of CONSTANT xxxx
;; 2VARIABLE 2CONSTANT 2! 2@ *not supporting*
;; FILL( addr n char — ) Fills n bytes of memory, beginning the address addr, with value char.
;; ERASE( addr n — ) Stores zeroes into n bytes of memory, beginning at address addr.
;; C!( char addr — ) Stores byte char at address addr.
;; C@( char addr — ) Fetches byte value from the address.


;;;
;;; Th-th-th-that's all, folks!
;;;
