#lang racket/base
(require (prefix-in rkt: racket))
(provide (all-defined-out))

;——

(define s.car rkt:car)
(define s.cdr rkt:cdr)
(define s.+ rkt:+)
(define s.< rkt:<)
(define (num x) (rkt:if (rkt:number? x) x 0))
(define (if/nil Q A E)
  (rkt:if (rkt:equal? Q 'nil) (E) (A)))

(define (atom x) (rkt:if (rkt:pair? x) 'nil 't))
(define (car x) (rkt:if (rkt:pair? x) (s.car x) '()))
(define (cdr x) (rkt:if (rkt:pair? x) (s.cdr x) '()))
(define (equal x y) (rkt:if (rkt:equal? x y) 't 'nil))
(define (natp x)
  (rkt:if (rkt:integer? x) (rkt:if (s.< x 0) 'nil 't) 'nil))
(define (+ x y) (s.+ (num x) (num y)))
(define (< x y)
  (rkt:if (s.< (num x) (num y)) 't 'nil))

(define-syntax if
  (syntax-rules ()
    ((_ Q A E)
     (if/nil Q (lambda () A) (lambda () E)))))

(define-syntax defun
  (syntax-rules ()
    ((_ name (arg ...) body)
     (define (name arg ...) body))))

(define-syntax dethm
  (syntax-rules ()
    ((_ name (arg ...) body)
     (define (name arg ...) body))))

(defun size (x)
  (if (atom x)
      '0
      (+ '1 (+ (size (car x)) (size (cdr x))))))
