;;; begin nat-stacks.scm -- The Not A Threaded interpreted Language --
;;;
;;; By Troy Brumley, 2023. I've placed this software under
;;; the unlicense license. See the LICENSE.org or UNLICENSE
;;; files for more details. TL;DR -- released to the public
;;; domain.


;;;
;;; The Stacks.
;;;

;; At a minimum a Forthish language requires a couple of
;; stacks. These are not to be confused with the processor
;; or runtime stack and heap in C or assembler. Notatil
;; provides stacks for integers, reals, and call-return.
;;
;; The integer stack is referred to as the data stack or
;; more often just "the stack".
;;
;; Uses:
;; - The arithmetic is done on the stack using unary or
;;   binary postfix operators. Think Reverse Polish
;;   Notation.
;; - Numeric and boolean comparisons are done on the
;;   stack using postfix operators.
;; - Parameters or arguments for functions and control
;;   flow statements are placed on the stack.
;; - Variables, constants, and strings are accessed via
;;   references placed on the stack.
;;
;; The call-return stack is used primarily for call and
;; return type functions, but it can also be used as a
;; temporary storage within the function or bracketed
;; code (such as the "true path" of an if statement).
;;
;; Real or floating point numbers are stored on the
;; float stack.
;;

;; The current stack implementation is my usual cons
;; at the front Scheme list. I'm trusting that the
;; cons-car-cdr for such lists are not too expensive.
;;
;; There are no arbitrary limits on the stack so you
;; have to work hard to get an overflow. Underflow is
;; checked for and throws an error.
;;
;; All the stacks store boxed Scheme values.


;;;
;;; Stack Management:
;;;


;;
;; The stacks.
;;
(define stack-data   '())
(define stack-float  '())
(define stack-return '())

;;
;; Reset the stacks to empty.
;;
(define (clear-stacks)
  (set! stack-data   '())
  (set! stack-float  '())
  (set! stack-return '()))

;;
;; Stack operations. Parallel implementations for each
;; stack.
;;


;; Add an item to the top of the stack.
(define (push n)
  (set! stack-data (cons n stack-data)))

(define (push-r n)
  (set! stack-return (cons n stack-return)))

(define (push-f n)
  (set! stack-float (cons n stack-float)))


;; Remove an item from the top of the stack. The check
;; for underflow is to be called by the NotATil primitives
;; and they should error without causing an exception
;; here. If those checks are missing, the pops will
;; raise exceptions from the Scheme runtime.
(define (pop)
  (let ((n (car stack-data)))
    (set! stack-data (cdr stack-data)) n))

(define (pop-r)
  (let ((n (car stack-return)))
    (set! stack-return (cdr stack-return)) n))

(define (pop-f)
  (let ((n (car stack-float)))
    (set! stack-float (cdr stack-float)) n))


;; Stack checking:
;;
;; This should be called by builtins/primitives to catch
;; and report a stack underflow back to the user. The
;; exact mechanism for reporting is not worked out yet.
;;
;; An optimization would be to keep a running record of
;; the stack depth instead of checking the length at each
;; call but we'll only do that if there seems to be a real
;; need.
;;
;; Arguments:
;; - N is the number of items that must be available on
;;   the stack.
;; - SYM is a Scheme symbol used to report what function
;;   encountered a stack underflow.
(define (check-stack n sym)
  (if (> n (length stack-data))
      (error
       'check-stack
       "stack underflow on op"
       sym n (length stack-data) stack-data)))

(define (check-return n sym)
  (if (> n (length stack-return))
      (error
       'check-return
       "return stack underflow on op"
       sym n (length stack-return) stack-return)))

(define (check-float n sym)
  (if (> n (length stack-float))
      (error
       'check-float
       "float stack underflow on op"
       sym n (length stack-float) stack-float)))


;;; end nat-stacks.scm -- The Not A Threaded interpreted Language --
;;;
