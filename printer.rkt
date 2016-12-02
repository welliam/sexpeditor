#lang racket

(provide display-with-focus emphasize)

(define term-red "\033[91m")
(define term-reset "\033[0m")

(define (display-pairs sexp focus path)
  (display "(")
  (display-focused-sexp (car sexp) focus (cons 'car path))
  (let loop ((sexp (cdr sexp)) (i 1) (path-matched #f))
    (define traversal-path (append (make-list i 'cdr) path))
    (define this-path-matched (equal? traversal-path focus))
    (cond
     ((and (not this-path-matched) (pair? sexp))
      (display " ")
      (display-focused-sexp (car sexp)
                            focus
                            (cons 'car traversal-path))
      (loop (cdr sexp)
            (+ i 1)
            (or path-matched this-path-matched)))
     (else
      (when (or this-path-matched (not (null? sexp)))
        (display " . ")
        (display-focused-sexp sexp focus traversal-path))
      (display ")")))))

(define-syntax-rule (emphasize p body ...)
  (let ()
    (when p (display term-red))
    (define res (begin body ...))
    (when p (display term-reset))
    res))

(define (display-focused-sexp sexp focus path)
  (emphasize (equal? path focus)
    (cond
     ((pair? sexp) (display-pairs sexp focus path))
     (else (write sexp)))))

(define (display-with-focus sexp focus)
  (display-focused-sexp sexp focus '()))
