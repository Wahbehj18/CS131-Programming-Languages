#lang racket
(provide expr-compare)

(define lambda-sym (string->symbol "\u03BB"))

(define ids '((list)))

(define make-id
  (lambda (x y)
    (string->symbol (string-append
                      (symbol->string x)
                      "!"
                      (symbol->string y)))
    ))

(define (display-all . args)
  (display args)
  (newline))

(define compare-x-y
  (lambda (x y)
    (cond
      [(equal? x y) x]
      [else
       (make-id x y)]
      )))

(define make-xid-map
  (lambda (x y)
    (cond
      [(null? x) '()]
      ;[(eq? (car x) (car y))
       ;(make-xid-map (cdr x) (cdr y))]
      [(cons (list (car x) (compare-x-y (car x) (car y)))
             (make-xid-map (cdr x) (cdr y)))])))

(define make-yid-map
  (lambda (x y)
    (cond
      [(null? x) '()]
      ;[(eq? (car x) (car y))
       ;(make-yid-map (cdr x) (cdr y))]
      [(cons (list (car y) (compare-x-y (car x) (car y)))
             (make-yid-map (cdr x) (cdr y)))])))

(define process-params
  (lambda (x y)
    (cond
      [(null? x) '()]
      [else (cons (compare-x-y (car x) (car y))
             (process-params (cdr x) (cdr y)))])
    ))

(define eq-or-if
  (lambda (x y)
  (if (equal? x y)
      x
      (list 'if '% x y))))


(define nested-lambda-eq-or-if
  (lambda (x y xmap ymap params)
    #|
    (display-all "NESTED-LAMBDA-EQ-OR-IF")
    (display-all "lambda-expr x: " x)
    (display-all "lambda-expr y: " y)
    (display-all "xmap: " xmap)
    (display-all "ymap: " ymap)
    (display-all "params: " params)|#
  (if (equal? x y)
      x
      ((let ([xcompound (assoc x xmap)]
             [ycompound (assoc y xmap)])
         (cond
           [(and (list? (member x params)) (list? (member y params)))
            ; x and y are params, don't mess with it!
            (list 'if '% x y)]
           [(and (null? xmap) (null? ymap))
            ; maps are empty, don't mess with it!
            (list 'if '% x y)]
           [(and (list? xcompound) (list? (member y params)))
            ; x is a compound but not a param, use the xmap value
            (list 'if '% (cdr xcompound) y)]
           [(and (list? ycompound) (list? (member x params)))
            ; y is a compound but not a param, use the ymap value
            (list 'if '% x (cdr ycompound))]
           ))))))

 
(define process-lambda-expr
  (lambda (x y xmap ymap)
    #|(display-all "LAMBDA-EXPR")
    (display-all "lambda-expr x: " x)
    (display-all "lambda-expr y: " y)
    (display-all "xmap: " xmap)
    (display-all "ymap: " ymap)|#
    (if (null? x)
        '()
        (if (null? y)
            '()
            (cond
              [(not (list? x))
               (let ([xcompound (assoc x xmap)]
                     [ycompound (assoc y ymap)])

                 (cond
                   [(and (equal? xcompound #f) (equal? ycompound #f))
                    (eq-or-if x y)
                    ]
                   [(equal? xcompound #f)
                    (eq-or-if x (cadr ycompound))]
                   [(equal? ycompound #f)
                    (eq-or-if (cadr xcompound) y)]
                   [(equal? (cdr xcompound) (cdr ycompound))
                    (cadr xcompound)]
                   [else (list 'if '% (cadr xcompound) (cadr ycompound))]
                   
                   ))
               ]
              
              [(or (equal? (car x) 'quote) (equal? (car y) 'quote))
                ; quote: don't mess with this
                 (list 'if '% x y)]
              #|[(or
                (and (equal? (car x) 'if) (equal? (assoc (car x) xmap) #f) (not (equal? (car y) 'if)))
                (and (not(equal? (car x) 'if)) (equal? (car y) 'if) (equal? (assoc (car y) xmap) #f)))
                ; if: don't mess with this if they don't match
                 (list 'if '% x y)]|#
              
              [else
               ; no special case: proceed through list

               ; check if compound parameter exists
               (let ([xcompound (assoc (car x) xmap)]
                     [ycompound (assoc (car y) ymap)])

                 (cond
                   [(and (equal? xcompound #f) (equal? ycompound #f))
                    (cond
                      [(or (equal? (car x) 'lambda) (equal? (car x) lambda-sym))
                       ; nested lambda!!
                       (process-lambda-all x y xmap ymap)]
                      [(and (list? (car x))
                            (or (equal? (caar x) 'lambda) (equal? (caar x) lambda-sym)))
                       ; nested lambda with more expr!!
                       (cons (process-lambda-all (car x) (car y) xmap ymap)
                             (process-lambda-expr (cdr x) (cdr y) xmap ymap))]
                      ; not nested lambda, proceed
                      [else (cons (process-lambda-expr (car x) (car y) xmap ymap)
                                    (process-lambda-expr (cdr x) (cdr y) xmap ymap))]

                        )]
                   [(equal? xcompound #f)
                    (cons (eq-or-if (car x) (cadr ycompound))
                          (process-lambda-expr (cdr x) (cdr y) xmap ymap))]
                   [(equal? ycompound #f)
                    (cons (eq-or-if (cadr xcompound) (car y))
                          (process-lambda-expr (cdr x) (cdr y) xmap ymap))]
                   [(equal? (cdr xcompound) (cdr ycompound))
                    (cons (cadr xcompound)
                         (process-lambda-expr (cdr x) (cdr y) xmap ymap))]
                   [else (cons (eq-or-if(cadr xcompound) (cadr ycompound))
                         (process-lambda-expr (cdr x) (cdr y) xmap ymap))]
                   
                   ))])))))

(define process-lambda-all
  (lambda (x y xmap ymap)
  (if (not (equal? (length (cadr x)) (length (cadr y))))
      ; first check if #parameters is the same. if not, don't mess with this
      (list 'if '% x y)
      (if (not (equal? (car x) (car y))) 
          ; use lambda-sym
          (append (list lambda-sym (process-params (cadr x) (cadr y)))
                  (list(process-lambda-expr (caddr x) (caddr y)
                                            (append (make-xid-map (cadr x) (cadr y)) xmap) (append (make-yid-map (cadr x) (cadr y)) ymap)))) 
          ; use given lambda
          (append (list (car x) (process-params (cadr x) (cadr y)))
                  (list(process-lambda-expr (caddr x) (caddr y)
                                            (append (make-xid-map (cadr x) (cadr y)) xmap) (append (make-yid-map (cadr x) (cadr y)) ymap)
                       )))))))

(define list-expr-compare
  (lambda (x y)
    #|
    (display-all "LIST-EXPR")
    (display-all "list x: " x)
    (display-all "list y: " y)|#
    
    (if (null? x)
        '()
        (if (null? y)
            '()
            ; check special cases: quotes, ifs, lambda
            (cond
              [(or (equal? (car x) 'quote) (equal? (car y) 'quote))
                ; quote: don't mess with this
                 (list 'if '% x y)]
              [(or
                (and (equal? (car x) 'if) (not (equal? (car y) 'if)))
                (and (not(equal? (car x) 'if)) (equal? (car y) 'if)))
                ; if: don't mess with this if they don't match
                 (list 'if '% x y)]
              [(and (or (equal? (car x) 'lambda) (equal? (car x) lambda-sym))
                    (or (equal? (car y) 'lambda) (equal? (car y) lambda-sym))
                    (not (null? (cdr x))) (not (null? (cdr y))))
               ; lambda function: process parameters and proceed
               (process-lambda-all x y (list (list '() '())) (list (list '() '())))
               ]
              [(or (and (or (equal? (car x) 'lambda) (equal? (car x) lambda-sym))
                        (not (or (equal? (car y) 'lambda) (equal? (car y) lambda-sym))))
                   (and (or (equal? (car y) 'lambda) (equal? (car y) lambda-sym))
                        (not (or (equal? (car x) 'lambda) (equal? (car x) lambda-sym)))))
               ; mismatched lambda, don't mess with this
               (list 'if '% x y)
               ]
              
              [else
               ; no special case: proceed through list
                (cons (expr-compare (car x) (car y))
                  (list-expr-compare (cdr x) (cdr y)))]
              )))))

(define (expr-compare x y)
  #|
  (display-all "CALLING EXPR-COMPARE")
  (display-all "x: " x) 
  (display-all "y: " y) |#

  (cond 
        [(equal? x y) x]
        [(and (boolean? x) (boolean? y))
         (if x '% '(not %))]
        
        [(or (not (list? x))
             (not (list? y)))
         (list 'if '% x y)]
        [(and (list? x) (list? y))
         (if (equal? (length x) (length y))
             (list-expr-compare x y)
             (list 'if '% x y)
            )]
        
        [else (display "implement me!")]
        ))



(define (test-expr-compare x y) 
  (and (equal? (eval x)
               (eval `(let ((% #t)) ,(expr-compare x y))))
       (equal? (eval y)
               (eval `(let ((% #f)) ,(expr-compare x y))))))

(define test-expr-x
  `(cons 1 ((lambda (a b x) (+ a b)) 10 11 12)))

(define test-expr-y
  `(cons 9 ((λ (c d y) (+ c d)) 20 21 22)))