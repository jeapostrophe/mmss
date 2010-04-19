(module maybe mzscheme
  (require (lib "contract.ss")
           (lib "plt-match.ss")
           (lib "etc.ss")
           (lib "list.ss"))
  (require "string.ss")
  (provide (all-defined))
  
  ;; option types  
  (define-struct maybe () (make-inspector))
  (define-struct (nothing maybe) () (make-inspector))
  (define-struct (just maybe) (value) (make-inspector))
  
  ; maybe-return : alpha -> (maybe alpha)
  (define (maybe-return a)
    (make-just a))
  ; maybe-bind : (maybe alpha) (alpha -> (maybe beta)) -> (maybe beta)
  (define (maybe-bind m f)
    (if (nothing? m)
        (make-nothing)
        (f (just-value m))))
  
  (define maybe-write/string 
    (match-lambda
      [(struct nothing ())
       (write/string `(none))]
      [(struct just (v))
       (write/string `(some ,v))]))
  (define (maybe-read/string s)
    (match (read/string s)
      [`(none)
        (make-nothing)]
      [`(some ,v)
        (make-just v)]))                      
  
  (define (combine* c ms)
    (foldl
     (lambda (x y)
       (combine c x y))
     (make-nothing)
     ms))
  
  (define (combine c x y)
    (match (list x y)
      [(list (struct nothing ()) (struct nothing ()))
       (make-nothing)]
      [(list (struct nothing ()) (struct just (_y)))
       y]
      [(list (struct just (_x)) (struct nothing ()))
       x]
      [(list (struct just (_x)) (struct just (_y)))
       (make-just (c _x _y))]))

  (define (maybe-equal? equal? x y)
    (match (list x y)
      [(list (struct nothing ()) (struct nothing ()))
       #t]
      [(list (struct nothing ()) (struct just (_y)))
       #f]
      [(list (struct just (_x)) (struct nothing ()))
       #f]
      [(list (struct just (_x)) (struct just (_y)))
       (equal? _x _y)]))
  
  ; maybe/c : fc -> fc
  (define (maybe/c fc)
    (flat-named-contract
     "Maybe(alpha)"
     (lambda (x)
       (or (nothing? x)
           (and (just? x)
                ((flat-contract-predicate fc) (just-value x)))))))
  
  (define f->f/maybe
    (opt-lambda (f [fail #f])
      (lambda args
        (let ([r (apply f args)])
          (if (eq? fail r)
              (make-nothing)
              (make-just r))))))
  (define assq/maybe (f->f/maybe assq))
  (define assv/maybe (f->f/maybe assv))
  
  ; map/maybe -> (alpha -> maybe(beta)) (list[n] alpha) -> (list[m] beta) : m <= n
  (define (map/maybe f lst)
    (reverse
     (foldl (lambda (e a)
              (let ([me (f e)])
                (if (nothing? me)
                    a
                    (cons (just-value me) a))))
            empty
            lst))))