;;; begin nat-dictionary.scm -- The Not A Threaded interpreted Language --
;;;
;;; By Troy Brumley, 2023. I've placed this software under
;;; the unlicense license. See the LICENSE.org or UNLICENSE
;;; files for more details. TL;DR -- released to the public
;;; domain.


;;;
;;; The interpreter looks up program statements in a
;;; LIFO dictionary. Each dictionary entry can contain
;;; function pointers to primitives (notatil functions
;;; implemented in Scheme), literals to place on the
;;; stack, and references to other word definitions.
;;;
;;; In early versions of NotATil, the dictionary is
;;; implemented as a normal Scheme list with new words
;;; cons to the front of the list. Once looping and
;;; control flow words are added, this may need to
;;; become a vector.
;;;
;;; The dictoinary entries are implemented as a nested
;;; list.
;;;
;;; While I don't believe Moore ever framed it this in
;;; these terms, the Forth language dictionary is an
;;; example of lexical scoping. Not quite closures in
;;; the modern sense, but you can shadow an existing
;;; definition of a word and have new words you define
;;; use the new definition without changing earlier
;;; definitions.
;;;


;;
;; This is the current dictionary. When the system starts
;; the it is cleared and then the nat-core-words are used
;; to build the starting dictionary.
;;
(define nat-dictionary
  '())

;;
;; The original dictionary format was a lifo stack implemented
;; as an consed list of pairs: the car was the word name as a
;; string, while the cdr was a procedure reference.
;;
;; Later, distinctions were made between primitives, newly
;; defined words, and the possibility for variables or
;; constants.
;;
;; I'm still new to Scheme and am trying not to dive too deeply
;; into things like properties. Structures are pairs or lists
;; or a combination that can be accessed via cons and cdr.
;;
;; Presently an entry is composed of a case insensitive name,
;; a symbol indicating the general type of the entry, and
;; the procedure reference or sequence of references needed
;; to perform whatever action the word invokes.
;;
;; These variables and functions abstract the accessors.
;;
(define entry-types
  '(core-word                           ; a primitive
    user-word                           ; a user defined word
    user-var                            ; variable or constant
    mark-word                           ; a dictionary marker
    ))
;; Accessors
(define (entry-name w)
  (car w))
(define (entry-type w)
  (car (cdr w)))
(define (entry-proc w)
  (cdr (cdr w)))
;; Build a new entry.
(define (nat-entry-build n t p)
  (cons n (cons t p)))
;; TODO: source of definition would be nice. this format
;;       does not lend itself to meaningful "disassembly"

;;
;; Build up a dictionary from the list of primitives in
;; nat-core-words. The order of the core words does not
;; really matter. The lookup process is done only when
;; a user is typing or a file is loaded for compiling.
;;
(define (nat-dictionary-build)
  (set! nat-dictionary '())
  (letrec*
      ((n '()) (t 'core-word) (p '())
       (f (lambda (xs)
            (cond ((null? xs) )
                  (else (set! n (car (car xs)))
                        (set! p (cdr (car xs)))
                        (set! nat-dictionary
                              (cons (nat-entry-build n t p) nat-dictionary))
                        (f (cdr xs)))))))
    (f nat-core-words)))

;;
;; Look up a word in the nat-dictionary and return either
;; is definition or the symbol 'nat-word-not-found.
;;
;; Words are case insensitive. Forth dates back to a time
;; when mixed case text in programs was mostly unheard of.
;;
(define (nat-lookup word)
  (letrec*
      ((f (lambda (w d)
            (cond ((null? d) 'nat-word-not-found)
                  ((string-ci= w (entry-name (car d))) (car d))
                  (else (f w (cdr d)))))))
    (f word nat-dictionary)))

;;
;; A pending definition is a list of word tokens and
;; possibly some numeric literals. The first element of
;; the list is the new word, and subsequent elements
;; comprise the definition.
;;
;; Iterate through the definition and for every element
;; that exists in the current dictionary, copy its
;; definition and add it to the new word being defined.
;;
(define (nat-add-new-word)
  (let*
      ((nwp (car nat-pending-def)) (new-word (cdr nwp))
       (def (cdr nat-pending-def))
       (currp '())
       (currt 'nat-tok-none)
       (currw "")
       (currw-def '())
       (proc '())
       (tokenized '())
       )
    ;; (define (dbg w)
    ;;   (display "==>") (display w) (display "<==") (newline)
    ;;   (display "      curr: ") (display curr) (newline)
    ;;   (display " tokenized: ") (display tokenized) (newline)
    ;;   (display "      proc: ") (display proc) (newline))
    ;; some words can not be redefined
    (if (member (string-downcase new-word) nat-perm-words)
        (error 'nat-add-new-word "may not redefine core word" new-word nat-pending-def))
    ;; the first pair in nat-pending-def is the name of the
    ;; new word and has been consumed, process each remaining
    ;; pair to build a dictionary entry for the new word.
    (while (not (null? def))
      ;; get next and advance input pointer
      (set! currp (car def)) (set! currt (car currp)) (set! currw (cdr currp))
      (set! def (cdr def))
      ;; is this a word we already know? or a literal?
      (set! currw-def (nat-lookup currw))
      (if (equal? currw-def 'nat-word-not-found)
          (set! proc (token-is-integer-literal currw radix))
          (set! proc (entry-proc currw-def)))
      ;; if it resolved to something we can execute, add
      ;; it to the accumulating definition
      (if (or (procedure? proc) (integer? proc) (list? proc))
          (set! tokenized (cons proc tokenized))
          (error 'add-new-word "unknown word in definition :;" currp nat-pending-def)))
    (set! nat-dictionary (cons (nat-entry-build new-word 'user-word (reverse tokenized)) nat-dictionary))))

;; I believe most Forth like systems will let you shadow any
;; existing word, but NotATil has some words that can not
;; be redefined. This is mostly due to the way new words
;; are compiled, but philosophically I think we need a firm
;; foundation.
;;
(define nat-perm-words
  '(
    ;; all your base are belong to us
    "base" "base?"
    "hexadecimal" "hex"
    "decimal" "dec"
    "octal" "oct"
    "binary" "bin"

    ;; running the repl
    "bye" "help" "load" "save" "see" "block" "list" "edit"

    ;; return stack manipulation
    "r" "r@" ">r" "r>"
    "do" "loop" "+loop" "i" "j"

    ;; defining words
    "variable" "constant"
    "cells" "allot"
    ":" ";"
    "forget" "marker"

    ;; coments
    "(" ")"

    ))

;;
;; These are the core or starting words of the NotATil
;; system. You can think of htese as primitives.  Some
;; but not all can be redefined, but you probably don't
;; need to do so. See nat-perm-words for a list of the
;; words that can not be redefined.
;;
;; These are not complete dictionary entries, just the
;; minimum information needed to build real entries via
;; nat-dictionary-build.
;;
;; ( "string name" . procedure reference )
;;
(define nat-core-words
  (list

   ;; radix related, allowing some synonyms
   (cons "base" base)
   (cons "base?" base?)
   (cons "radix" base?)
   (cons "bin" base-bin)
   (cons "binary" base-bin)
   (cons "dec" base-dec)
   (cons "decimal" base-dec)
   (cons "hex" base-hex)
   (cons "hexadecimal" base-hex)
   (cons "oct" base-oct)
   (cons "octal" base-oct)

   ;; stack manipulation
   (cons "drop" drop)
   (cons "dup" dup)
   (cons "?dup" ?dup)
   (cons "over" over)
   (cons "rot" rot)
   (cons "swap" swap)
   (cons "2drop" 2drop)
   (cons "2dup" 2dup)
   (cons "2over" 2over)
   (cons "2swap" 2swap)
   (cons ">r" to-r)
   (cons "r>" from-r)
   (cons "r@" fetch-r)
   (cons "r" fetch-r)

   ;; primitive arithmetic
   (cons "+" op+)
   (cons "-" op-)
   (cons "/" op/)
   (cons "*" op*)
   (cons "mod" mod)
   (cons "/mod" /mod)

   ;; comparison operations
   (cons "<" op<)
   (cons "=" op=)
   (cons ">" op>)
   (cons "<>" op<>)
   (cons "0>" op0>)
   (cons "0<" op0<)
   (cons "0=" op0=)

   ;; boolean operations
   (cons "and" op-and)
   (cons "or" op-or)
   (cons "not" op-not)

   ;; simple output
   (cons "." dot)
   (cons ".#\"" dot-quote)
   (cons ".r" dot-r)
   (cons ".s" dot-s)
   (cons "cr" cr)
   (cons "emit" emit)
   (cons "space" space)
   (cons "spaces" spaces)

   ;; simple input
   (cons "key" key)

   ;; definition directives are hard coded in main loop
   ;; : ; variable constant marker allot cells

   ;; flow control is also hard coded
   ;; do loop +loop if else then

   ))

;;;
;;; end nat-dictionary.scm -- The Not A Threaded interpreted Language --
