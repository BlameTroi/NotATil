;;; begin nat-tokenizer.scm -- The Not A Threaded interpreted Language --
;;;
;;; By Troy Brumley, 2023. I've placed this software under
;;; the unlicense license. See the LICENSE.org or UNLICENSE
;;; files for more details. TL;DR -- released to the public
;;; domain.


;;;
;;; Break entered text into tokens and do some syntax and
;;; semantic evaluation. The lines in an interpreter aren't
;;; as cleanly drawn as in a compiler.
;;;

;;
;; The rules for tokenizing a Forth style language are
;; dead simple: whitespace separates tokens with only two
;; exceptions:
;;
;; - Comments are enclosed in parenthesis. An open comment
;;   is ' ( ' while the comment is closed by a close paren.
;;   Not ' ) ' but ') '. Between the parens anything but a
;;   premature close paren can be entered. NotATil treats
;;   comments as white space when evaluating a words.
;; - Strings and enclosed in double quotes. As with the
;;   comment, the opening sequence is quote-space. Characters
;;   after the first space up to the ending quote-space are
;;   significant. White space is preserved.
;;
;; Some Forths support whole line comments via a backslash
;; (\) character but NotATil does not.
;;
;; There are no special characters in the traditional sense
;; but some token sequences will set expectations for later
;; tokens, as in the : name def ; form, if else then, ." string"
;; and so on.

;; Convert the text in nat-buffer into a tokenized notatil
;; program. Both immediate and definitional statements can
;; be in the buffer.
;;
;; The tokens are (type . word) pairs and are returned in a
;; vector to allow for easier backtracking when evaluating
;; flow control statements.
;;
(define (nat-tokenize)

  ;; reset tokenizer state
  (set! nat-buffer (nat-scrub nat-buffer))
  (set! nat-tokenized '())
  (set! nat-compiling #f)
  (set! nat-buffer-empty #f)
  ;; Keep grabbing tokens until the buffer is empty. The
  ;; possibility of the input not being complete is an
  ;; issue for callers. Don't try to tokenize a logically
  ;; incomplete chunk of code.
  (let* ((tok-pair '()) (tok-extra-pair '()) (tok-closing-pair '())
         (tok-type 'nat-tok-none) (tok-string "")
         (dict-entry 'nat-word-not-found))
    (set! tok-pair (nat-next-token))
    (while (not nat-buffer-empty)
      (set! tok-type (car tok-pair))
      (set! tok-string (cdr tok-pair))
      (cond
       ((equal? tok-type 'nat-tok-word)
        (set! dict-entry (nat-lookup tok-string))
        (if (equal? dict-entry 'nat-word-not-found)
            (let ((n (token-is-integer-literal tok-string radix))
                  (f (token-is-real-literal tok-string radix)))
              (cond (n (set! tok-type 'nat-tok-integer))
                    (f (set! tok-type 'nat-tok-real))
                    (else (set! tok-type 'nat-tok-word-unknown)))
              (set! tok-pair (cons tok-type tok-string)))))
       ((equal? tok-type 'nat-tok-begin-comment)
        (set! tok-extra-pair (cons 'nat-tok-comment (nat-buffer-to-char #\))))
        (set! tok-closing-pair (nat-next-token)))
       ((or (equal? tok-type 'nat-tok-print-string) (equal? tok-type 'nat-tok-begin-string))
        (set! tok-extra-pair (cons 'nat-tok-string (nat-buffer-to-char #\")))
        (set! tok-closing-pair (nat-next-token))
        (if (equal? (car tok-closing-pair) 'nat-tok-begin-string)
            (set! tok-closing-pair (cons 'nat-tok-end-string (cdr tok-closing-pair)))))
       ) ;; add processed token to result
      (set! nat-tokenized (cons tok-pair nat-tokenized))
      (if (not (null? tok-extra-pair))
          (set! nat-tokenized (cons tok-extra-pair nat-tokenized)))
      (if (and (not (null? tok-closing-pair))
               (not (equal? (car tok-closing-pair) 'nat-tok-none)))
          (set! nat-tokenized (cons tok-closing-pair nat-tokenized)))
      (set! tok-pair (nat-next-token))
      (set! tok-extra-pair '())
      (set! tok-closing-pair '())))
  ;; This will work better elsewhere as a vector for
  ;; easier backtracking.
  (set! nat-tokenized (list->vector (reverse nat-tokenized))))

;;
;; A diagnostic helper to display the context around the
;; token that caused an error. Argument I is the ref of
;; that token in nat-tokenized.
;;
(define (dump-five-around i)
  (let ((s '()) (b (max (- i 5) 0)) (e (min (+ i 5) (vector-length nat-tokenized))))
    (while (< b e)
      (set! s (cons (cdr (vector-ref nat-tokenized b)) s))
      (set! b (1+ b)))
    (reverse s)))

;;;
;;; Tokenize/Parse helpers.
;;;

;;
;; Various delimiters for strings and comments.
;;
;; These gymnastics are needed because I haven't figured
;; out how to enter (char-set #\" #\( #\) ) with lispy.
;; I'd rather not switch it off and on.
;;
(define dlm-lsc (string->list "\"()"))
(define dlm-quote (car dlm-lsc))
(define dlm-lparen (cadr dlm-lsc))
(define dlm-rparen (caddr dlm-lsc))
(define dlm-openers (char-set dlm-quote dlm-lparen))
(define dlm-closers (char-set dlm-quote dlm-rparen))

;;
;; Normalize string S for tokenization. Change all white-
;; space to blanks, reduce runs of blanks to a single blank
;; unless they are in a Forth string or comment, and add
;; blanks to both ends of the incoming string to simplify
;; boundary handling.
;;
(define (nat-scrub s)
  (let ((accum '(#\space))
        (lastc #\space) (currc #\space)
        (inside #f) (seeking #\nul)
        (t (string->list s)))
    (while (not (null? t))
      ;; consume next char
      (set! lastc currc) (set! currc (car t)) (set! t (cdr t))
      ;; if inside pass straight thru but watch for end
      (if inside
          (begin                        ; just pass inside through
            (set! accum (cons currc accum))
            (if (char=? seeking currc)
                (begin                  ; transition to outside
                  (set! seeking #\nul)
                  (set! inside #f)))
            (continue)))
      ;;
      (if (or (char=? currc dlm-lparen) (char=? currc dlm-quote))
          (begin                        ; entered a string or comment
            (set! accum (cons currc accum))
            (set! inside #t)
            (set! seeking (if (char=? currc dlm-lparen) dlm-rparen dlm-quote))
            (continue)))
      ;;
      (if (or (char=? currc #\nl) (char=? currc #\tab) (char=? currc #\page))
          (set! currc #\space))
      ;;
      (if (or (not (char=? currc #\space)) (not (char=? lastc currc)))
          (set! accum (cons currc accum))))
    ;; end of while, watch for needing one more space
    (if (not (char=? (car accum) #\space))
        (set! accum (cons #\space accum)))
    (list->string (reverse accum))))

;;
;; If string W is a numeric literal in base B, return the
;; numeric value or #f. Only hex, decimal, octal, and binary
;; are supported.
;;
(define (token-is-integer-literal w b)
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

;;
;; If string W is a real (floating point) numeric literal in
;; base 10, return the numeric value or #f. Reals are only
;; valid in decimal (at least that's what I found when I
;; check in gforth) so base B <> 10 is automatically false.
;;
;; I could probably blindly take the result of string->number
;; but will do some checks first."
;;
(define (token-is-real-literal w b)
  (if (and (= b 10)(string-every chars-real w))
      (string->number w)
      #f))

;;
;; character sets for testing strings to see if the are
;; valid digit sequences the current base.
;;
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

(define chars-real
  (char-set #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9
            #\e #\E #\. #\+ #\-))

;;
;; Get the next token out of nat-buffer and return it as a
;; (type . text) pair. Updates tokenizer state and buffer
;; but does not consume past the token.
;;
;; Anything not recognized is assumed to be a word and will
;; be addressed later in notatil."
;;
(define (nat-next-token)
  (nat-trimleft-buffer)
  (cond (nat-buffer-empty
         (set! nat-buffer "")
         (cons 'nat-tok-none ""))
        ((string= "" nat-buffer)
         (set! nat-buffer-empty #t)
         (cons 'nat-tok-none ""))
        (else
         (cons (nat-buffer-token-code) (nat-buffer-to-blank)))))

;;
;; Does the first token in the remaining buffer exist in
;; the code mapping table? If so, return the code or
;; unknown.
;;
(define (nat-buffer-token-code)
  (letrec*
      ((f (lambda (xs)
            (cond ((null? xs) 'nat-tok-word)
                  ((nat-buffer-prefix? (car (car xs))) (cdr (car xs)))
                  (else (f (cdr xs))))
            )))
    (f nat-token-code-map)))

;;;
;;; Buffer parse and carve
;;;

;;
;; Remove leading blanks from nat-buffer. Recursive.
;;
(define (nat-trimleft-buffer)
  (cond ((or nat-buffer-empty (= 0 (string-length nat-buffer)))
         (set! nat-buffer-empty #t)     ; emptied
         (set! nat-buffer ""))
        ((not (char=? #\space (string-ref nat-buffer 0) )))
        (else
         (set! nat-buffer (substring nat-buffer 1)) ; trim and check again
         (nat-trimleft-buffer))))


;;
;; Take the next token from nat-buffer up to a blank or the
;; end of the buffer. The scrubbing process when starting
;; to tokenize is expected to wrap the buffer with a blank
;; on either end.
;;
;; The terminating blank is consumed but not returned."
;;
(define (nat-buffer-to-blank)
  (let* ((i (string-index nat-buffer #\space))
         (t (substring nat-buffer 0 i)))
    (set! nat-buffer (substring nat-buffer (1+ i)))
    t))

;;
;; Return the contents of the buffer up to but not including
;; character C. This would be useful when finding the end of
;; a string or comment. The terminating character is neither
;; returned or consumed.
;;
(define (nat-buffer-to-char c)
  (let ((i (string-index nat-buffer c)) (t ""))
    (cond (i (set! t (substring nat-buffer 0 i))
             (set! nat-buffer (substring nat-buffer i)))
          (else (set! t (string-join (list  "ERROR COULD NOT FIND EXPECTED '" (list->string (list c)) "'") ""))))
    t))

;;
;; YAGNI shmagmi. Stubs as placeholders. I no longer think
;; I'll use these forms but there's no harm in having them
;; here.
;;

;; Return the contents of the buffer up to and including
;; character C."
(define (nat-buffer-through-char c)
  (error "not implemented"))

;; Return the contents of the buffer up to but not
;; including the string token TK.
(define (nat-buffer-to-token tk)
  (error "not implemented"))

;; Return the contents of the buffer up to and
;; including the string token TK.
(define (nat-buffer-through-token tk)
  (error "not implemented"))

;;
;; Check if the prefix of the buffer matches a string.
;;
(define (nat-buffer-prefix? s)
  (= (string-length s) (string-prefix-length-ci nat-buffer s)))

;;;
;;; Tables. We got tables.
;;;

;;
;; Token and type mapping table
;;
;; Not every possible word is included. Control structure
;; and definition words are, plus some repl operations.
;;
(define nat-token-code-map
  (list (cons "do " 'nat-tok-do)
        (cons "loop " 'nat-tok-loop)
        (cons "+loop " 'nat-tok-+loop)
        (cons "begin " 'nat-tok-begin)
        (cons "until " 'nat-tok-until)
        (cons "again " 'nat-tok-again)
        (cons "while " 'nat-tok-while)
        (cons "repeat " 'nat-tok-repeat)
        (cons "leave " 'nat-tok-leave)
        (cons "if " 'nat-tok-if)
        (cons "then " 'nat-tok-then)
        (cons "else " 'nat-tok-else)
        (cons ": " 'nat-tok-begin-definition)
        (cons "; " 'nat-tok-end-definition)
        (cons "variable " 'nat-tok-variable)
        (cons "constant " 'nat-tok-constant)
        (cons "marker " 'nat-tok-marker)
        (cons "see " 'nat-tok-see)
        (cons "list " 'nat-tok-list)
        (cons "bye " 'nat-tok-bye)
        (cons "help " 'nat-tok-help)
        (cons "abort " 'nat-tok-abort)
        (cons "page " 'nat-tok-page)
        (cons "quit " 'nat-tok-quit)
        (cons "( " 'nat-tok-begin-comment)
        (cons ") " 'nat-tok-end-comment)
        ;; strings can be dormant or printing
        ;; they are closed by the first quote
        ;; after the open, not space quote, so
        ;; that space would be in the string.
        ;; 'nat-tok-string-end can't be in
        ;; this table because some forths
        ;; allow a single quote to start a
        ;; string.
        (cons ".\" " 'nat-tok-print-string)
        (cons "s\" " 'nat-tok-begin-string)
        (cons "\" " 'nat-tok-begin-string)
        ;;
        ;;
        ;;
        ))

(define nat-definition-tokens
  '(nat-tok-begin-definition
    nat-tok-end-definition
    nat-tok-marker
    nat-tok-constant
    nat-tok-variable
    ))

(define nat-loop-tokens
  '(nat-tok-do
    nat-tok-loop
    nat-tok-+loop
    nat-tok-begin
    nat-tok-until
    nat-tok-again
    nat-tok-while
    nat-tok-repeat
    nat-tok-leave
    ))

(define nat-conditional-tokens
  '(nat-tok-if
    nat-tok-else
    nat-tok-then
    ))

(define nat-repl-tokens
  '(nat-tok-see
    nat-tok-list
    nat-tok-bye
    nat-tok-help
    ))

(define nat-comment-tokens
  '(nat-tok-begin-comment
    nat-tok-end-comment
    ))

(define nat-string-tokens
  '(nat-tok-print-string
    nat-tok-begin-string
    nat-tok-end-string
    ))

(define nat-immediate-tokens
  '(nat-tok-word
    nat-tok-bye
    nat-tok-integer
    nat-tok-real
    nat-tok-word-unknown                ; will error at times
    ;; later nat-tok-string maybe?
    ))


;;; end nat-tokenizer.scm -- The Not A Threaded interpreted Language --
