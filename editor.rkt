#lang racket

(require "logic.rkt")

(provide ui editor default-keybinds)

(struct ui (read-char print read-symbol read-number read-string exit) #:transparent)

(define default-keybinds
  (hash #\s (lambda (sexp path ui)
              (values (change sexp path ((ui-read-symbol ui)))
                      path))
        #\n (lambda (sexp path ui)
              (values (change sexp path ((ui-read-number ui)))
                      path))
        #\" (lambda (sexp path ui)
              (values (change sexp path ((ui-read-string ui)))
                      path))
        #\) (lambda (sexp path ui) (values (change sexp path '()) path))
        #\u (lambda (sexp path ui) (values sexp (path-up path)))
        #\> (lambda (sexp path ui) (values sexp (path-next path)))
        #\< (lambda (sexp path ui) (values sexp (path-previous path)))
        #\n (lambda (sexp path ui) (values sexp (path-next path)))
        #\l (lambda (sexp path ui) (values (listify sexp path) path))
        #\c (lambda (sexp path ui) (values (consify sexp path) path))
        #\a (lambda (sexp path ui) (values sexp (cons 'car path)))
        #\d (lambda (sexp path ui) (values sexp (cons 'cdr path)))
        #\A (lambda (sexp path ui) (values (lift-car sexp path) path))
        #\D (lambda (sexp path ui) (values (lift-cdr sexp path) path))
        #\q (lambda (sexp path ui) ((ui-exit ui)))
        #\^ (lambda (sexp path ui)
              (values sexp (if (null? path) '() (cdr path))))))

(define (do-input sexp path input ui keybinds)
  ;; (display input)
  ;; (display "\r\n")
  (define search (hash-ref keybinds input #f))
  (if search
      (search sexp path ui)
      (values sexp path)))

(define (compute-input sexp path input ui keybinds)
  (define-values (next-sexp next-path)
    (do-input sexp path input ui keybinds))
  (values (fix-sexp next-sexp next-path) next-path))

(define (editor ui (keybinds (default-keybinds)))
  (let loop ((sexp '()) (path '()))
    ((ui-print ui) sexp path)
    (define-values (next-sexp next-path)
      (compute-input sexp path ((ui-read-char ui)) ui keybinds))
    (loop next-sexp next-path)))
