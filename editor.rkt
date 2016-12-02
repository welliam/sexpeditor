#lang racket

(require "logic.rkt")

(provide ui editor default-keybinds current-file)

(define current-file (make-parameter "output.scm"))

(struct ui
  (read-char print print-sexps read-symbol read-number read-string exit)
  #:transparent)

(struct state (buffers buffer-index) #:transparent)

(define (update-list-index f t i)
  (if (zero? i)
      (cons (apply f (car t)) (cdr t))
      (cons (car t) (update-list-index f (cdr t) (- i 1)))))

(define (update-current-buffer st f)
  (struct-copy state st
    (buffers (update-list-index
              f
              (state-buffers st)
              (state-buffer-index st)))))

(define (update-sexp st f)
  (define (update sexp path) (list (f sexp path) path))
  (update-current-buffer st update))

(define (update-path st f)
  (define (update sexp path) (list sexp (f path)))
  (update-current-buffer st update))

(define (update-buffer-index st f)
  (struct-copy state st
    (buffer-index (f (state-buffer-index st)
                     (state-buffers st)))))

(define ((update-sexp-keybind f) st ui)
  (update-sexp st f))

(define ((read-constant-keybind field) st ui)
  ((update-sexp-keybind (curryr change ((field ui))))
   st ui))

(define ((update-path-keybind f) st ui)
  (update-path st f))

(define ((update-buffer-index-keybind f) st ui)
  (update-buffer-index st f))

(define (display-buffers-keybind st ui)
  ((ui-print-sexps ui)
   (map car (state-buffers st))
   (state-buffer-index st))
  st)

(define next-buffer-keybind
  (update-buffer-index-keybind
   (lambda (i buffers)
     (min (+ i 1) (- (length buffers) 1)))))

(define previous-buffer-keybind
  (update-buffer-index-keybind
   (lambda (i buffers)
     (max (- i 1) 0))))

(define (append-new-buffer-keybind st ui)
  (define buffers
    (append (state-buffers st)
            (list (default-buffer))))
  (struct-copy state st
               (buffers buffers)
               (buffer-index (- (length buffers) 1))))

(define (delete-newest-buffer-keybind st ui)
  (define buffers (drop-right (state-buffers st) 1))
  (if (null? buffers)
      st
      (struct-copy state st
        (buffers buffers)
        (buffer-index (min (state-buffer-index st)
                           (- (length buffers) 1))))))

(define (write-to-file-keybind st ui)
  (with-output-to-file (current-file)
    (lambda ()
      (for-each (lambda (buffer)
                  (displayln (car buffer)) (newline))
                (state-buffers st)))
    #:exists 'replace)
  st)

(define default-keybinds
  (hash #\s (read-constant-keybind ui-read-symbol)
        #\n (read-constant-keybind ui-read-number)
        #\" (read-constant-keybind ui-read-string)
        #\) (update-sexp-keybind (curryr change '()))
        #\l (update-sexp-keybind listify)
        #\c (update-sexp-keybind consify)
        #\A (update-sexp-keybind lift-car)
        #\D (update-sexp-keybind lift-cdr)
        #\a (update-path-keybind (curry cons 'car))
        #\d (update-path-keybind (curry cons 'cdr))
        #\> (update-path-keybind path-next)
        #\< (update-path-keybind path-previous)
        #\u (update-path-keybind path-up)
        #\? display-buffers-keybind
        #\= next-buffer-keybind
        #\- previous-buffer-keybind
        #\+ append-new-buffer-keybind
        #\_ delete-newest-buffer-keybind
        #\^ (update-path-keybind
             (lambda (path) (if (null? path) '() (cdr path))))
        #\q (lambda (st ui) ((ui-exit ui)))
        #\W write-to-file-keybind))

(define (do-input st input ui keybinds)
  (define search (hash-ref keybinds input #f))
  (if search (search st ui) st))

(define (do-input/fix-sexp st input ui keybinds)
  (define next-st (do-input st input ui keybinds))
  (update-sexp next-st fix-sexp))

(define (default-buffer) (list '() '()))

(define (default-state) (state (list (default-buffer)) 0))

(define (editor ui (keybinds default-keybinds))
  (let loop ((st (default-state)))
    (apply (ui-print ui)
           (list-ref (state-buffers st) (state-buffer-index st)))
    (loop
     (do-input/fix-sexp st ((ui-read-char ui)) ui keybinds))))
