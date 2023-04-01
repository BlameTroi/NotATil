;;; begin nat.scm -- The Not A Threaded interpreted Language --
;;;
;;; By Troy Brumley, 2023. I've placed this software under
;;; the unlicense license. See the LICENSE.org or UNLICENSE
;;; files for more details. TL;DR -- released to the public
;;; domain.



;;
;; NotATil is a Forth like language but its internals are
;; not implemented as a traditional threaded interpreted
;; language would be. There are similarities, but there
;; are also enough differences that I decided it would not
;; be proper to call this "Forth."
;;
;; The name NotATil is an homage to the great book
;; _Threaded Interpretive Languages_ by R. G. Loeliger.
;;
;; Other sources for this project are my own recollections
;; of using Color Forth (Radio Shack's, not the new one
;; from Moore) on my trusty old TRS-80 Color Computer, the
;; book _Start Forth_ by Leo Brodie (available on the web
;; at https://www.forth.com/starting-forth/0-starting-forth/ ,
;; and the Forth Lessons on the One Laptop Per Child wiki
;; at https://wiki.laptop.org/go/Forth_Lessons .
;;


;;
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
;;
;; Originally I wanted NotATil to be a single file Forth like
;; language interpretter with a literate software lean. Sadly
;; I don't "think" in terms of single files. Too many years
;; working on larger systems. There are pros and cons, but I
;; am going with the multi file approach. This is the main
;; or root file and it includes the files that make up the
;; NotATil language.
;;

;; Goals:
;;
;; - Support terminal based development with the ability
;;   to edit and "compile" source.
;; - Simplicity of implementation is more important than
;;   performanc.
;; - Expand from the minimal stack arithmetic and simple
;;   definition required for the Exercism "Forth" exercise
;;   to include a repl, control flow, and possibly files.

;; Terminology:
;;
;; - Built in and user defined operators are referred to as
;;   words and their definitions are stored in a dictionary.
;; - Stacks are not actual hardware stacks. They are managed
;;   by NotATil.
;; - Definitions are either references to primitive (Scheme
;;   hosted) functions or "compiled" from source.
;; - Compiling in a Forth like language is much more simple
;;   that for a traditional procedural language. We use the
;;   same term for the same general process of converting
;;   programmer text into executable instructions, but there
;;   ends the similarity.


;;;
;;; The whole of NotATil:
;;;

;; Load order minimizes noise when stepping through the load.
;; Skip to the end to get to the real beginning.

;;
;; Common Libraries and Modules
;;
;; I do most of my development and testing in emacs with
;; geiser. I find that geiser and readline don't get along
;; well so I don't activate it here. Instead I do so in
;; the nat-repl entry point.
;;
(use-modules (ice-9 readline)
             (ice-9 pretty-print)
             (srfi srfi-14))
;; (activate-readline)

;;
;; As Peter Suk once said, "State is baaaaaaad." This
;; implies that global state is even worse. And yet, as I
;; worked on the implementation I found that passing
;; parameters added no clarity to the code. Quite the
;; opposite.
;;
;; This is a monolithic application, the processing is
;; sequential, and there are no processes to separate
;; during code entry, compilation, and top level
;; execution.
;;
;; Global storage as defined variables are kept here.
;; There are control tables for both the tokenizer
;; and dictionary, but they are kept in those files.
(load "nat-globals.scm")

;;
;; When running a NotATil program, stacks for integer
;; data, floating point data, and the NotATil call/return
;; stack, are all managed by the nat-executor. Actually,
;; they are mostly managed by the NotATil programmer,
;; but s/he does this via the executor. The stacks and
;; their accessors are in:
(load "nat-stacks.scm")

;;
;; Finally, there are primitives or low level NotATil
;; "words" that are written as Guile Scheme functions
;; and they are kept in:
(load "nat-primitives.scm")

;;
;; Forth like languages have a dictionary at their core.
;; Functions to manage the dictionary are in:
(load "nat-dictionary.scm")

;;
;; Forth like languages accept an input stream and act
;; on it once the stream is complete (if reading from a
;; file) or when the end user signals that the stream
;; so far should be processed by pressing the enter
;; key.
;;
;; The input stream is read by functions in nat-entry.scm
;; and then processed in the following files. Tokenization
;; and initial evaluation are closely related, but are
;; kept in separate source files:
(load "nat-tokenizer.scm")
(load "nat-executor.scm")

;;
;; nat-entry holds the external entry points to NotATil.
;; nat-repl starts an interactive read eval print loop
;; while the various nat-test functions are for scriptable
;; testing:
(load "nat-entry.scm")


;;;
;;; end nat.scm -- The Not A Threaded interpreted Language --
