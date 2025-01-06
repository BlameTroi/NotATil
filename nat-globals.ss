;;; begin nat-globals.scm -- The Not A Threaded interpreted Language --
;;;
;;; By Troy Brumley, 2023. I've placed this software under
;;; the unlicense license. See the LICENSE.org or UNLICENSE
;;; files for more details. TL;DR -- released to the public
;;; domain.


;;;
;;; General global state for NotATil. See nat.scm for the
;;; rational.  There isn't much in the way of general global
;;; state, but what there is gets parked here unless it
;;; finds a home in another file.
;;;
;;; Global state may be bad, but I don't want to be good
;;;
;;; Dependency injection is just gilded global state :)
;;;

(define nat-buffer "")
(define nat-buffer-empty #t)

(define nat-tokenized '())

(define nat-pending-def '())

(define nat-status "enter help for basic help.")
(define nat-error "?")

(define nat-compiling #f)
(define nat-terminating #f)
;; (define nat-in-def #f)
;; (define nat-in-comment #f)
;; (define nat-in-string #f)


;;;
;;; end nat-globals.scm -- The Not A Threaded interpreted Language --
