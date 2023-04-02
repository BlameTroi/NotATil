;;; begin nat-entry.scm -- The Not A Threaded interpreted Language --
;;;
;;; By Troy Brumley, 2023. I've placed this software under
;;; the unlicense license. See the LICENSE.org or UNLICENSE
;;; files for more details. TL;DR -- released to the public
;;; domain.


;;;
;;; Outward Facing Functions.
;;;

;;
;; NotATil is an interpreter. Even when directives are read
;; in from a file, the program views the input and output
;; as an old style TTY. The enter key, or new line \n, tells
;; NotATil to process the instructions entered so far and
;; then wait for more input.
;;
;; An end of file or command BYE terminates the program.
;;
;; Program text is completely free form. Words are case
;; insensitive. One or more white space characters delimit
;; words and literals with only a couple of exceptions. See
;; nat-tokenizer.scm for a more detailed explanation.
;;
;; An entry point is provided for an interactive temrinal
;; session (nat-repl). Given the nature of *nix I/O, this
;; can probably be used for batch or scripted programs
;; as well.
;;
;; Additional entry points are provided for both unit test
;; scripting and interaction under Emacs' Geiser. Each takes
;; a single string as an argument and may display output
;; in addition to return the stack contents in a list for
;; validation.
;;

;;;
;;; The Read Eval Print Loop.
;;;

;;
;; nat-repl does what its name implies. Input is read via
;; readline until an end-of-file is encountered or the
;; directive 'bye' is encountered.
;;
;; Error handling is limited at this time and the repl can
;; fail on an exception. Proper handling and reporting are
;; planned later.
;;
;; Note that readline and Emacs with Geiser don't play
;; well together so readline is not activated by default.
;; Use the nat-test functions for testing under Geiser.
;;
(define (nat-repl)
  (activate-readline)
  (nat-full-reset)
  (nat-display-status)
  (nat-prompt-read)
  (while (not nat-terminating)          ; an almost infinite loop
    (nat-tokenize)                      ; tokenize the input
    (nat-evaluate)                      ; evaluate/execute
    (set! nat-status " ok ")            ; status of last command sequence
    (nat-display-status)
    (if (not nat-terminating)           ; if we're not terminating, read
        (nat-prompt-read)))
  (newline))                            ; advance to next line

;; using srfi-78's simple and clean
;; check function. It accepts a string and treats it as
;; a complete invocation of NotATil.
;; This is the top level for an interative session. Loop
;; until the user enters "bye".

;;;
;;; Testing Entry Points:
;;;

;; There are three: nat-test-repl, nat-test-dict, and
;; nat-test. The difference between them is that nat-test
;; always resets the complete NotATil state, nat-test-dict
;; resets all state but the dictionary, and nat-test-repl
;; is a minimal repl with no bells and whistles. It should
;; run OK under emacs but you will only be able to enter
;; clean looking sexps there.
;;
;; All three functions will accept a single string argument
;; and return the stack. The argument is optional for
;; nat-test-repl.
;;

(define (nat-test prog)
  (nat-full-reset)
  (set! nat-buffer prog)
  (nat-tokenize)
  (nat-evaluate)
  stack-data)

(define (nat-test-dict prog)
  (nat-test-reset)
  (set! nat-buffer prog)
  (nat-tokenize)
  (nat-evaluate)
  stack-data)

(define* (nat-test-repl #:optional prog)
  (nat-full-reset)
  (newline)
  (display "nat-test-repl -- enter bye on a new line to end cleanly")
  (newline)
  (if (not prog)
      (set! prog ""))
  (set! nat-buffer (nat-scrub prog))
  (while (not nat-terminating)
    (nat-tokenize)
    (nat-evaluate)
    (if (not nat-terminating)
        (begin
          (newline) (nat-stack-depth)
          (set! nat-buffer (nat-scrub  (readline " > "))))))
  (newline) (display "nat-test-repl -- done") (newline))

;;;
;;; User interaction helpers
;;;

;;
;; Display a prompt and read into the nat-buffer.
;;
(define (nat-prompt-read)
  (nat-stack-depth) (set! nat-buffer (readline " > ")))

;;
;; This is not used as yet, but if the tokenizer detects
;; that the last user entry was incomplete, prompt for
;; more input and append that to the current buffer
;; contents.
;;
;; TODO: Should this force a re-tokenize? Perhaps only from the
;; start of the incomplete entry?
;;
(define (nat-prompt-more)
  (set! nat-buffer (string-append nat-buffer (readline " + "))))

;;
;; Display the status of the last commands and print the
;; depth of the stack and float stack if they are non-
;; zero. The return stack should always be empty at this
;; point.
;;
(define (nat-display-status)
  (display nat-status)
  (newline))

;;
;; Display depth of main stack even if it is empty, and the
;; float stack's depth if it isn't empty. For a prefix to
;; prompts.
;;
(define (nat-stack-depth)
  (display "[s:") (display (number->string (length stack-data)))
  (if (> (length stack-float) 0)
      (begin (display " f:") (display (number->string (length stack-float)))))
  (display "]"))
;;
;; Reset the environment to a known initial state. Clear
;; stacks, restore starting dictionary, set any globals,
;; and return to base 10.
;;
(define (nat-full-reset)
  (clear-stacks)
  (nat-dictionary-build)
  (set! nat-compiling #f)
  (set! nat-terminating #f)
  (set! nat-buffer "")
  (set! nat-status "enter help for basic help.")
  (set! nat-error "?")
  (set! nat-pending-def '())
  (base-dec))


;;
;; Reset the environment for testing. Protects any
;; updates to the dictionary made on prior calls, if
;; any.
;;
(define (nat-test-reset)
  (clear-stacks)
  (if (zero? (length nat-dictionary))
      (nat-dictionary-build))
  (set! nat-compiling #f)
  (set! nat-terminating #f)
  (set! nat-buffer "")
  (set! nat-status "enter help for basic help.")
  (set! nat-error "?")
  (set! nat-pending-def '())
  (base-dec))



;;;
;;; end nat-entry.scm -- The Not A Threaded interpreted Language --
