;;; A circular Lisp interpreter in Common/Emacs/Nukata Lisp
;;;    by SUZUKI Hisao on H28.8/10, H29.3/13
;;;    cf. Zick Standard Lisp (https://github.com/zick/ZickStandardLisp)

(progn
  ;; Expr: (EXPR environment (symbol...) expression...)
  ;; Subr: (SUBR . function)
  ;; Environment: ((symbol . value)...)
  ;; N.B. Expr has its own environment since this Lisp is lexically scoped.

  ;; Language-specific Hacks
  (setq funcall (lambda (f x) (f x)))  ; for Nukata Lisp and this Lisp
  (setq max-lisp-eval-depth 10000)     ; for Emacs Lisp
  (setq max-specpdl-size 7000)         ; for Emacs Lisp

  ;; The global environment of this Lisp
  (setq global-env
        (list '(*version* . (1.2 "Lisp" "circlisp"))
              (cons 'car
                    (cons 'SUBR (lambda (x) (car (car x)))))
              (cons 'cdr
                    (cons 'SUBR (lambda (x) (cdr (car x)))))
              (cons 'cons
                    (cons 'SUBR (lambda (x) (cons (car x) (cadr% x)))))
              (cons 'eq
                    (cons 'SUBR (lambda (x) (eq (car x) (cadr% x)))))
              (cons 'atom
                    (cons 'SUBR (lambda (x) (atom (car x)))))
              (cons 'rplaca
                    (cons 'SUBR (lambda (x) (rplaca (car x) (cadr% x)))))
              (cons 'rplacd
                    (cons 'SUBR (lambda (x) (rplacd (car x) (cadr% x)))))
              (cons 'list
                    (cons 'SUBR (lambda (x) x)))
              (cons '+
                    (cons 'SUBR (lambda (x) (+ (car x) (cadr% x)))))
              (cons '*
                    (cons 'SUBR (lambda (x) (* (car x) (cadr% x)))))
              (cons '-
                    (cons 'SUBR (lambda (x) (- (car x) (cadr% x)))))
              (cons 'truncate
                    (cons 'SUBR (lambda (x) (truncate (car x) (cadr% x)))))
              (cons 'mod
                    (cons 'SUBR (lambda (x) (mod (car x) (cadr% x)))))
              (cons '=
                    (cons 'SUBR (lambda (x) (= (car x) (cadr% x)))))
              (cons '<
                    (cons 'SUBR (lambda (x) (< (car x) (cadr% x)))))
              (cons 'print
                    (cons 'SUBR (lambda (x) (print (car x)))))
              (cons 'apply
                    (cons 'SUBR (lambda (x) (apply% (car x) (cadr% x)))))
              (cons 'eval
                    (cons 'SUBR (lambda (x) (eval% (car x) global-env))))))

  (defun caar% (x) (car (car x)))
  (defun cadr% (x) (car (cdr x)))
  (defun cddr% (x) (cdr (cdr x)))
  (defun caddr% (x) (car (cdr (cdr x))))
  (defun cdddr% (x) (cdr (cdr (cdr x))))
  (defun cadddr% (x) (car (cdr (cdr (cdr x)))))

  (defun assq% (key alist)              ; cf. Emacs/Nukata Lisp
    (if alist
        (if (eq key (caar% alist))
            (car alist)
          (assq% key (cdr alist)))
      nil))

  (defun pairlis% (keys data alist)     ; cf. Common Lisp
    (if keys
        (cons (cons (car keys) (car data))
              (pairlis% (cdr keys) (cdr data) alist))
      alist))

  ;; Define symbol as value in the global environment.
  (defun global-def (sym val)
    (rplacd global-env
            (cons (car global-env)
                  (cdr global-env)))
    (rplaca global-env
            (cons sym val)))

  (defun eval% (e env)
    (if (atom e)
        ((lambda (var)
           (if var
               (cdr var)
             e))
         (assq% e env))
      (if (eq (car e) 'quote)           ; (quote e)
          (cadr% e)
        (if (eq (car e) 'if)            ; (if e e e)
            (if (eval% (cadr% e) env)
                (eval% (caddr% e) env)
              (eval% (cadddr% e) env))
          (if (eq (car e) 'progn)       ; (progn e...)
              (eval-progn (cdr e) env nil)
            (if (eq (car e) 'lambda)    ; (lambda (v...) e...)
                (make-closure env (cdr e))
              (if (eq (car e) 'defun)   ; (defun f (v...) e...)
                  (global-def (cadr% e)
                              (make-closure env (cddr% e)))
                (if (eq (car e) 'setq)  ; (setq v e)
                    ((lambda (var value)
                       (if var
                           (rplacd var value)
                         (global-def (cadr% e) value))
                       value)
                     (assq% (cadr% e) env)
                     (eval% (caddr% e) env))
                  (apply% (eval% (car e) env) ; (f e...)
                          (evlis (cdr e) env))))))))))

  ;; (make-closure env '((v...) e...)) => (EXPR env (v...) e...)
  (defun make-closure (env ve)
    (cons 'EXPR
          (cons env ve)))

  ;; (eval-progn '((+ 1 2) 3 (+ 4 5)) global-env nil) => 9
  (defun eval-progn (x env result)
    (if x
        (if (cdr x)
            (eval-progn (cdr x)
                        env
                        (eval% (car x) env))
          (eval% (car x) env))
      result))

  ;; (evlis '((+ 1 2) 3 (+ 4 5)) global-env) => (3 3 9)
  (defun evlis (x env)
    (if x
        (cons (eval% (car x) env)
              (evlis (cdr x) env))
      nil))

  (defun apply% (fun arg)
    (if (eq (car fun) 'EXPR)            ; (EXPR env (v...) e...)
        (eval-progn (cdddr% fun)
                    (pairlis% (caddr% fun)
                              arg
                              (cadr% fun))
                    nil)
      (if (eq (car fun) 'SUBR)          ; (SUBR . f)
          (funcall (cdr fun) arg)
        fun)))

  (defun global-eval (e)
    (eval% e global-env))

  (global-eval (quote

;; -- WRITE YOUR EXPRESSION HERE --
(progn
  (defun fib (n)
    (if (< n 2)
        1
      (+ (fib (- n 1))
         (fib (- n 2)))))
  (print (fib 15)))
;; --------------------------------
)))
