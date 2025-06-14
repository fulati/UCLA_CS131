
#lang racket

(define LAMBDA (string->symbol "λ"))

(define (compare-base x y)
  (if (equal? x y) 
    x 
    (list 'if '% x y)))


(define (compare-booleans x y)
  (if (eq? x y)
    x
    (if x '% '(not %))))


(define (quote? x)
  (and (pair? x)
    (pair? (cdr x))
    (null? (cddr x))
    (eq? (car x) 'quote)))


(define (append x y)
  (string->symbol (string-append (symbol->string x) "!" (symbol->string y))))


(define (create-hashmaps x y)
  (define (loop x y hash-x hash-y)
    (cond
      [(or (null? x) (null? y))
       (list hash-x hash-y)]

      [(equal? (car x) (car y))
       (loop (cdr x) (cdr y)
             (hash-set hash-x (car x) (car x))
             (hash-set hash-y (car y) (car y)))]

      [else
       (let ([merged-name (append (car x) (car y))])
         (loop (cdr x) (cdr y)
               (hash-set hash-x (car x) merged-name)
               (hash-set hash-y (car y) merged-name)))]))

  (loop x y (hash) (hash)))


(define (compare-lambdas x y)
  (let* 
    ([x-params (cadr x)]
    [y-params (cadr y)])

    (if (not (= (length x-params) (length y-params)))
      (list 'if '% x y)

      (let* 
        ([x-body   (cddr x)]
        [y-body   (cddr y)]

        [hashmaps (create-hashmaps x-params y-params)]
        [hash-x (car hashmaps)]
        [hash-y (cadr hashmaps)]

        [lambda-type (if (or (eq? (car x) LAMBDA) (eq? (car y) LAMBDA)) LAMBDA 'lambda)]
        [renamed-params (map (λ (v) (hash-ref hash-x v v)) x-params)]
        [merged-body (map (lambda (x y) (compare-with-hashmaps x y hash-x hash-y)) x-body y-body)])
    (cons lambda-type (cons renamed-params merged-body))))))


(define (compare-with-hashmaps x y hash-x hash-y)
  (cond
    [(and (symbol? x) (symbol? y))
      (compare-base
      (hash-ref hash-x x x)
      (hash-ref hash-y y y))]

    [(and (equal? x y) (not (symbol? x))) x]
    [(and (boolean? x) (boolean? y)) (compare-booleans x y)]
    [(and (number? x) (number? y)) (compare-base x y)]
    [(and (quote? x) (quote? y)) (compare-base x y)]

    [(and (list? x) (list? y)
          (or (eq? (car x) 'lambda)
              (eq? (car x) LAMBDA)
              (eq? (car y) 'lambda)
              (eq? (car y) LAMBDA)))
     (compare-lambdas x y)]

    [(and (list? x) (list? y) (= (length x) (length y)))
     (compare-list-with-hashmaps x y hash-x hash-y)]

    [else
     (compare-base x y)]))


(define (compare-list-with-hashmaps xs ys hash-x hash-y)
  (if (or (null? xs) (null? ys))
      '()
      (cons (compare-with-hashmaps (car xs) (car ys) hash-x hash-y)
            (compare-list-with-hashmaps (cdr xs) (cdr ys) hash-x hash-y))))


(define (expr-compare x y)
  (cond
    [(equal? x y) x]
    [(and (boolean? x) (boolean? y)) (compare-booleans x y)]
    [(and (number? x) (number? y)) (compare-base x y)]
    [(and (symbol? x) (symbol? y)) (compare-base x y)]
    [(or (not (list? x)) (not (list? y))) (compare-base x y)]
    [(or (quote? x) (quote? y)) (compare-base x y)]
    [(or (equal? (car x) 'let) (equal? (car y) 'let)) (compare-base x y)]
    [(not (equal? (equal? (car x) 'if) (equal? (car y) 'if))) (compare-base x y)]

    [(or (eq? (car x) 'lambda)
         (eq? (car x) LAMBDA)
         (eq? (car y) 'lambda)
         (eq? (car y) LAMBDA))
     (compare-lambdas x y)]

    [else
     (if (not (= (length x) (length y)))
         (compare-base x y)
         (compare-list-with-hashmaps x y (hash) (hash)))]))



(define (test-expr-compare x y)
 (and 
    (equal? (eval (list 'let '([% #t]) (expr-compare x y))) (eval x))
    (equal? (eval (list 'let '([% #f]) (expr-compare x y))) (eval y))))


(define test-expr-x
  '((lambda (a)
      (if #t
          '(a)
          ((lambda (x) (+ x 1)) a)))
    1))

(define test-expr-y
  '((λ (b)
      (if #f
          '(b)
          ((λ (y) (+ y 2)) b)))
    2))


;; given examples 

(expr-compare 12 12) 
(expr-compare 12 20) 
(expr-compare #t #t) 
(expr-compare #f #f) 
(expr-compare #t #f) 
(expr-compare #f #t) 

(expr-compare '(/ 1 0) '(/ 1 0.0)) 

(expr-compare 'a '(cons a b)) 
(expr-compare '(cons a b) '(cons a b))  
(expr-compare '(cons a lambda) '(cons a λ))  
(expr-compare '(cons (cons a b) (cons b c))
              '(cons (cons a c) (cons a c)))
(expr-compare '(cons a b) '(list a b)) 
(expr-compare '(list) '(list a)) 
(expr-compare ''(a b) ''(a c)) 
(expr-compare '(quote (a b)) '(quote (a c)))  
(expr-compare '(quoth (a b)) '(quoth (a c)))  
(expr-compare '(if x y z) '(if x z z)) 
(expr-compare '(if x y z) '(g x y z))
(expr-compare '((lambda (a) (f a)) 1) '((lambda (a) (g a)) 2))
(expr-compare '((lambda (a) (f a)) 1) '((λ (a) (g a)) 2))
(expr-compare '((lambda (a) a) c) '((lambda (b) b) d))
(expr-compare ''((λ (a) a) c) ''((lambda (b) b) d))
(expr-compare '(+ #f ((λ (a b) (f a b)) 1 2))
              '(+ #t ((lambda (a c) (f a c)) 1 2)))
(expr-compare '((λ (a b) (f a b)) 1 2)
              '((λ (a b) (f b a)) 1 2))
(expr-compare '((λ (a b) (f a b)) 1 2)
              '((λ (a c) (f c a)) 1 2))
(expr-compare '((lambda (lambda) (+ lambda if (f lambda))) 3)
              '((lambda (if) (+ if if (f λ))) 3))
(expr-compare '((lambda (a) (eq? a ((λ (a b) ((λ (a b) (a b)) b a))
                                    a (lambda (a) a))))
                (lambda (b a) (b a)))
              '((λ (a) (eqv? a ((lambda (b a) ((lambda (a b) (a b)) b a))
                                a (λ (b) a))))
                (lambda (a b) (a b))))


(expr-compare '(lambda (lambda) lambda) '(λ (λ) λ))                  ; ⇒ '(λ (lambda!λ) lambda!λ)
(expr-compare '(lambda (a b) a) '(λ (b) b))                          ; ⇒ '(if % (lambda (a b) a) (λ (b) b))
(expr-compare '((λ (if) (+ if 1)) 3) '((lambda (fi) (+ fi 1)) 3))    ; ⇒ '((λ (if!fi) (+ if!fi 1)) 3)