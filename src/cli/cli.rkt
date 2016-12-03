#lang racket

(require "../editor.rkt" "printer.rkt")

(define stty-sane "stty sane")
(define stty-echo-on "stty echo")
(define stty-echo-off "stty raw -echo")

(define (displayrn . args)
  (unless (null? args)
    (apply display args))
  (display "\n\r"))

(define (displayf . args)
  (unless (null? args)
    (apply display args))
  (flush-output))

(define-syntax-rule (with-echo body ...)
  (begin (system stty-echo-on)
         (define result (begin body ...))
         (system stty-echo-off)
         result))

(define (read-characters finished?)
  (list->string
   (let loop ()
     (define c (read-char))
     (cond
      ((finished? c)
       (displayrn)
       '())
      (else
       (displayf c)
       (cons c (loop)))))))

(define (read-symbol)
  (displayf "symbol> ")
  (string->symbol (read-characters char-whitespace?)))

(define (read-number)
  (displayf "number> ")
  (string->number (read-characters char-whitespace?)))

(define (read-string)
  (displayf "string> ")
  (read-characters (curry char=? #\")))

(define (cli-print sexp path)
  (display-with-focus sexp path)
  (displayrn))

(define (cli-print-sexps sexps current)
  (displayrn (format "~A/~A" (+ current 1) (length sexps)))
  (for ((sexp sexps) (i (in-naturals)))
    (emphasize (= i current) (display "   ") (displayrn sexp))))

(define cli
  (ui read-char
      cli-print cli-print-sexps
      read-symbol read-number read-string
      (thunk (system stty-sane) (exit))))

(define (main)
  (system stty-echo-off)
  (editor cli)
  (system stty-sane))

(main)
