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
    (notatil-prompt cmdline)
    (while (not (string-ci= cmdline "bye"))
      ;; to be provided
      (set! status " ok ")
      (notatil-completion-status status)
      (notatil-prompt cmdline))
    ))


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
  stack-int)


;;;
;;; User interaction helpers
;;;

(define (notatil-prompt cmdline)
  "Display prompt to the user and accept commands."
  (set! cmdline (readline "> ")))


(define (notatil-completion-status status)
  "Display the status of the last commands and report
the length of the stack to the user."
  (display status)
  (if (> length stack-int 0)
      (display (length stack-int)))
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
   (let ((word-func (definition-of word)))
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
     ((not (equal? 'word-not-found word-func))
      (notatil-execute word word-func))

     ;; number in supported radix?
     ((not (equal? #f (token-is-numeric-literal word radix)))
      (push (token-is-numeric-literal word radix)))

     ;; I'm sorry Dave, I can't do that

     (else
      (error 'notatil-eval
             "unknown or undefined word"
             word radix stack-int)))))


;;
;; The evaluation process above looks up a word in the dictionary to
;; find its definition. For built in words such as SWAP or DUP, the
;; definition is a single procedure reference to the Scheme function
;; that implements the word. User defined words are stored as lists
;; of procedure references. A user defined word can include numeric
;; literals and they are pushed directly onto the stack.
;;
(define (notatil-execute word word-func)
  "Execute WORD by running it's WORD-FUNC."
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


;; Reset the environment for testing. Protects any
;; updates to the dictionary made on prior calls, if
;; any.
(define (notatil-test-reset)
  "Reset everything but the dictionary for testing."
  (clear-stacks)
  (if (zero? (length dictionary))
      (set! dictionary (list-copy core-words)))
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
  (display (string-join (map number->string stack-int) " ")))

;; . ( n -- ) Prints the top of the stack in the current radix.
(define (dot)
  (check-stack 1 '.)
  (display (number->string (pop) radix))
  (display #\space))

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
;; 0 is false and anything else is true. Forth returns
;; -1 from its relational checks.

(define (forth-bool b)
  "Convert a real boolean to -1 for true, 0 for false."
  (if b -1 0))

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


;;;
;;; Dictionary lookup and addition.
;;;

;;
;; Returns the procedure (a Scheme procedure reference or a list of
;; them) to be executed for a word.
;;
(define (definition-of word)
  "Return the definition to execute WORD from the dictionary, or
'word-not-found."
  (letrec* ((f (lambda (w d)
                 (cond ((null? d) 'word-not-found)
                       ((string-ci= w (car (car d))) (cdr (car d)))
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
        (tokenized '()) (curr "") (proc '()))
    (while (not (null? def))
      (set! curr (car def))
      (set! proc (definition-of curr))
      (if (equal? proc 'word-not-found)
          (set! proc (token-is-numeric-literal curr radix)))
      (if (or (procedure? proc) (number? proc) (list? proc))
          (set! tokenized (cons proc tokenized))
          (error 'add-new-word "unknown word in definition :;" curr pending-def))
      (set! def (cdr def)))
    (set! dictionary (cons (cons new-word (reverse tokenized)) dictionary))))


;; These are words that can not be redefined. Their operation is
;; too fundamental to the assumptions in notatil.
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

   ;; simple output
   (cons ".s" dot-s) (cons "." dot) (cons "cr" cr)
   (cons "emit" emit) (cons ".#\"" dot-quote)

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


;; Variables will be stored in the dictionary as well.
;; This is going to require that we put type codes on
;; dictionary entries.
(define entry-types '(core-word user-word user-var))


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

;; begin words until loop
;; do the words and when until is reached, check stack for
;; true. if not true, run the loop again
;;
;; variable names a cell in storage
;; ! stores the top of the stack into the named variable
;; @ retrieves the value of the named variable and puts
;; it on the stack
;; ? is defined as "@ ."
;; +! adds and stores, and could be defined as "@ + !"



;;;
;;; Th-th-th-that's all, folks!
;;;
