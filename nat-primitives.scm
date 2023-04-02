;;; begin nat-primitives.scm -- The Not A Threaded interpreted Language --
;;;
;;; By Troy Brumley, 2023. I've placed this software under
;;; the unlicense license. See the LICENSE.org or UNLICENSE
;;; files for more details. TL;DR -- released to the public
;;; domain.

;;;
;;; These are the primitive or built in NotATil words and
;;; supporting functions for format conversions, run time
;;; checks and the like.
;;;

;; At the lowest level, the dictionary entry for a built
;; in word is just a procedure reference to one of these
;; functions. More complex words are created from lists
;; of these references.
;;
;; If I had to draw a distinction between a true primitive
;; and a built in, it would be that the primitives do only
;; one thing (say add two numbers, or copy a value to the
;; return stack).
;;

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
;;; Simple output.
;;;

;; .S ( ? -- ? ) Prints the entire stack leaving the
;;               contents unchanged. Note that the top
;;               of the stack is to the right.
(define (dot-s)
  (display
   (string-join
    (map (lambda (s) (number->string s radix))
         (reverse stack-data))
    " ")))


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


;; . ( n -- ) Prints the top of the stack in the current
;;            radix.
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


;; emit ( c -- ) Prints the top of the stack as a
;;               character. Traditionally that's an
;;               octet, but we'll trust unicode to
;;               handle things.
(define (emit)
  (check-stack 1 'emit)
  (display (integer->char (pop))))


;; ." ( -- ) Prints everything up to but not included a
;;           trailing double quote.
;;           TODO: not implemented yet.
(define (dot-quote)
  (display "dot-quote not yet implemented."))


;; key ( -- c) Accept single character input and place
;;             it on the stack. Might be outside the
;;             traditional ANSI 1-255.
;;             TODO: not implemented yet.
(define (key)
  (display "key not yet implemented."))


;;;
;;; Stack manipulation words.
;;;

;; These are distinct from the stack management code in
;; nat-stacks.scm.

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


;; r>   ( -- n) [ n -- ]  Move an item from return to data
(define (from-r)
  (check-return 1 'r>)
  (push (pop-r)))


;; r@   ( -- n) [ n -- n] Copy an item from return to data
;; r    ( -- n) [ n -- n] Same as r@, an older synonym
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


;; ?DUP ( n1 -- n1 n1 ) but only if n1 is not zero
(define (?dup)
  (check-stack 1 '?dup)
  (let* ((n1 (pop))
         (r (not (zero? n1))))
    (push n1)
    (if r (push n1))))


;;;
;;; NOTE: The 2<blah> and double word math from older
;;;       pre-64 bit days probably aren't needed, but
;;;       they stack manipulations help with address
;;;       length pairs, as with strings.
;;;

;; 2SWAP	( d1 d2 — d2 d1 )	Reverses the top two pairs
;;                          of numbers
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


;; 2OVER	( d1 d2 — d1 d2 d1 ) Duplicates the second pair
;;                             of numbers
(define (2over)
  (check-stack 4 '2over)
  (let ((d2b (pop)) (d2a (pop)) (d1b (pop)) (d1a (pop)))
    (push d1a)(push d1b)
    (push d2a)(push d2b)
    (push d1a)(push d1b)))


;; 2DROP	( d1 d2 — d1 ) Discards the top pair of numbers
(define (2drop)
  (check-stack 2 '2drop)
  (pop)(pop))


;;
;; see also http://forth.org/svfig/Len/softstak.htm
;;
;; in stack comments right most is top most!
;;


;;;
;;; Basic arithmetic.
;;;

;; – ( n1 n2 — diff ) Subtracts n1-n2
(define (op-)
  (check-stack 2 '-)
  (let* ((n2 (pop)) (n1 (pop))
         (r (- n1 n2)))
    (push r)))


;; + ( n1 n2 — sum ) Adds
(define (op+)
  (check-stack 2 '+)
  (let* ((n2 (pop)) (n1 (pop))
         (r (+ n1 n2)))
    (push r)))


;;  * ( n1 n2 — prod ) Multiplies
(define (op*)
  (check-stack 2 '*)
  (let* ((n2 (pop)) (n1 (pop))
         (r (* n1 n2)))
    (push r)))


;; / ( n1 n2 — quot ) Divides n1/n2
(define (op/)
  (check-stack 2 '/)
  (let* ((n2 (pop)) (n1 (pop))
         (r (quotient n1 n2)))
    (push r)))


;; MOD ( n1 n2 — rem ) Divides n1/n2; returns only
;;                     the remainder
(define (mod)
  (check-stack 2 'mod)
  (let* ((n2 (pop)) (n1 (pop))
         (r (remainder n1 n2)))
    (push r)))


;; /MOD ( n1 n2 — rem quot ) Divides; returns both
;;                           remainder and quotient
(define (/mod)
  (check-stack 2 '/mod)
  (let* ((n2 (pop)) (n1 (pop))
         (r (remainder n1 n2)) (q (quotient n1 n2)))
    (push q) (push r)))


;;;
;;; Booleans
;;;

;; Casting booleans between Forth (-1, 0) and Scheme
;; (#t, #f). Also ensure that a Forth true value is -1.

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

;;;
;;; Logical and relational operators.
;;;

;; As in C or Forth, 0 is false and anything else is true.
;; Forth returns -1 from its relational checks for truth.

;; < ( n1 n2 -- n2 < n1 )
(define (op<)
  (check-stack 2 '<)
  (let* ((n2 (pop)) (n1 (pop))
         (r (< n1 n2)))
    (push (forth-bool r))))


;; = ( n1 n2 -- n1 = n2 )
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


;; > ( n1 n2 -- n2 > n1 )
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


;; not ( n1 -- !n1)
(define (op-not)
  (check-stack 1 'not)
  (let* ((n1 (pop)))
    (push (forth-bool (zero? n1)))))


;; and ( n1 n2 -- n1&n2)
(define (op-and)
  (check-stack 2 'and)
  (let*
      ((n2 (canonical-bool (pop)))
       (n1 (canonical-bool (pop))))
    (push (forth-bool
           (and (scheme-bool n1) (scheme-bool n2))))))


;; or ( n1 n2 -- n1|n2)
(define (op-or)
  (check-stack 2 'or)
  (let*
      ((n2 (canonical-bool (pop)))
       (n1 (canonical-bool (pop))))
    (push (forth-bool
           (or (scheme-bool n1) (scheme-bool n2))))))

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
;;  */( n1 n2 n3 — n4 )
;; Multiplies, then divides (n1*n2/n3). Uses a double-length intermediate result.
;;  */MOD( n1 n2 n3 — n4 n5 )
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

;; some of the forms we'll have to deal with
;;
;; USE xxx
;; USING xxx Use file xxx as Forth “disk”
;; ( xxx)	Ignores text up to “)” delimeter
;; INCLUDE xxx Load the text file xxx
;; FORGET xxx	Forget definitions back through xxx
;; MARKER xxx	Defines marker xxx to roll back dictionary
;;            where xxx is a word that forgets itself
;; : ;
;; variable xxx
;; constant xxx
;; cells
;; allot


;;;
;;; end nat-primitives.scm -- The Not A Threaded interpreted Language --
