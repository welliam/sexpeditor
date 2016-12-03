#lang racket

(require "logic.rkt")

(provide ui editor default-keybinds current-file)

(define current-file (make-parameter "output.scm"))

(struct ui
  (read-char print print-sexps read-symbol read-number read-string exit)
  #:transparent)

(struct state (buffers buffer-index clipboard) #:transparent)

(define (update-list-index f t i)
  (if (zero? i)
      (cons (apply f (car t)) (cdr t))
      (cons (car t) (update-list-index f (cdr t) (- i 1)))))

(define (current-buffer st)
  (list-ref (state-buffers st) (state-buffer-index st)))

(define (update-current-buffer st f)
  (struct-copy state st
    (buffers (update-list-index
              f
              (state-buffers st)
              (state-buffer-index st)))))

(define (update-sexp st f)
  (update-current-buffer st (lambda (s p) (list (f s p) p))))

(define (update-path st f)
  (update-current-buffer st (lambda (s p) (list s (f p)))))

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

(define (delete-buffer-keybind st ui)
  (define buffers
    (call-with-values
        (thunk (split-at (state-buffers st) (state-buffer-index st)))
      (lambda (a b) (append a (cdr b)))))
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
                  (writeln (car buffer)) (newline))
                (state-buffers st)))
    #:exists 'replace)
  st)

(define (yank-keybind st ui)
  (match-define (list sexp path) (current-buffer st))
  (struct-copy state st
               (clipboard (follow-path sexp path))))

(define (put-keybind st ui)
  (update-sexp st (curryr change (state-clipboard st))))

(define (yp-swap-keybind st ui)
  (update-sexp (yank-keybind st ui) (curryr change (state-clipboard st))))

(define (jump-to-nth-keybind n)
  (update-path-keybind (curryr path-jump-to-nth n)))

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
        #\space (update-path-keybind path-next)
        #\> (update-path-keybind path-next)
        #\< (update-path-keybind path-previous)
        #\u (update-path-keybind path-up)
        #\? display-buffers-keybind
        #\= next-buffer-keybind
        #\- previous-buffer-keybind
        #\+ append-new-buffer-keybind
        #\_ delete-buffer-keybind
        #\^ (update-path-keybind
             (lambda (path) (if (null? path) '() (cdr path))))
        #\q (lambda (st ui) ((ui-exit ui)))
        #\W write-to-file-keybind
        #\, yank-keybind
        #\. put-keybind
        #\/ yp-swap-keybind

        #\0 (jump-to-nth-keybind 0)
        #\1 (jump-to-nth-keybind 1)
        #\2 (jump-to-nth-keybind 2)
        #\3 (jump-to-nth-keybind 3)
        #\4 (jump-to-nth-keybind 4)
        #\5 (jump-to-nth-keybind 5)
        #\6 (jump-to-nth-keybind 6)
        #\7 (jump-to-nth-keybind 7)
        #\8 (jump-to-nth-keybind 8)
        #\9 (jump-to-nth-keybind 9)))

(define (do-input st input ui keybinds)
  (define search (hash-ref keybinds input #f))
  (if search (search st ui) st))

(define (do-input/fix-sexp st input ui keybinds)
  (define next-st (do-input st input ui keybinds))
  (update-sexp next-st fix-sexp))

(define (default-buffer) (list '() '()))

(define (default-state) (state (list (default-buffer)) 0 '()))

(define (editor ui (keybinds default-keybinds))
  (let loop ((st (default-state)))
    (apply (ui-print ui) (current-buffer st))
    (loop
     (do-input/fix-sexp st ((ui-read-char ui)) ui keybinds))))
