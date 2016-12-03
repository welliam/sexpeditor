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

(define (follow-path/update f sexp path)
  ;; returns multiple values: the focused sexp, and the whole sexp
  ;; with f applied to the focused area
  (let loop ((sexp sexp) (path (reverse path)) (todo '()))
    (cond
     ((null? path)
      (values sexp (foldl (lambda (f x) (f x)) (f sexp) todo)))
     ((eq? (car path) 'car)
      (loop (car sexp)
            (cdr path)
            (cons (lambda (x) (cons x (cdr sexp))) todo)))
     ((eq? (car path) 'cdr)
      (loop (cdr sexp)
            (cdr path)
            (cons (lambda (x) (cons (car sexp) x)) todo)))
     (else (error "unknown direction in path" (car path))))))

(define (follow-path sexp path)
  (define-values (result _) (follow-path/update identity sexp path))
  result)

(define (update f sexp path)
  (define-values (_ result) (follow-path/update f sexp path))
  result)

(define (change sexp path to)
  (update (lambda (sexp) to) sexp path))

(define (lift-car sexp path)
  (update (lambda (s) (if (pair? s) (car s) s)) sexp path))

(define (lift-cdr sexp path)
  (update (lambda (s) (if (pair? s) (cdr s) s)) sexp path))

(define (listify sexp path)
  (update list sexp path))

(define (consify sexp path)
  (update (lambda (s) (cons s s)) sexp path))

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
