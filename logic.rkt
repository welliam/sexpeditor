#lang racket

(provide (all-defined-out))

(define (path-up path)
  (if (null? path)
      path
      (dropf (if (eq? (car path) 'car)
                 (cdr path)
                 path)
             (curry eq? 'cdr))))

(define (path-next path)
  (list* 'car 'cdr (cdr path)))

(define (path-previous path)
  (cons 'car
        (if (or (null? path) (null? (cdr path)))
            '()
            (cddr path))))

(define (traverse f sexp path)
  (let loop ((sexp sexp) (path (reverse path)))
    (cond
     ((null? path) (f sexp))
     ((eq? (car path) 'car)
      (cons (loop (car sexp) (cdr path))
            (cdr sexp)))
     ((eq? (car path) 'cdr)
      (cons (car sexp)
            (loop (cdr sexp) (cdr path))))
     (else (error "unknown direction in path" (car path))))))

(define (change sexp path to)
  (traverse (lambda (sexp) to) sexp path))

(define (lift-car sexp path)
  (traverse (lambda (s) (if (pair? s) (car s) s)) sexp path))

(define (lift-cdr sexp path)
  (traverse (lambda (s) (if (pair? s) (cdr s) s)) sexp path))

(define (listify sexp path)
  (traverse list sexp path))

(define (consify sexp path)
  (traverse (lambda (s) (cons s s)) sexp path))

(define (fix-sexp sexp path)
  (let loop ((sexp sexp) (path (reverse path)))
    (cond
     ((null? path) sexp)
     ((eq? (car path) 'car)
      (if (pair? sexp)
          (cons (loop (car sexp)
                      (cdr path))
                (cdr sexp))
          (cons (loop '() (cdr path)) sexp)))
     ((eq? (car path) 'cdr)
      (if (pair? sexp)
          (cons (car sexp)
                (loop (cdr sexp)
                      (cdr path)))
          (cons sexp (loop '() (cdr path)))))
     (else (error "unknown direction in path" (car path))))))
