(module list mzscheme
  (require (lib "etc.ss")
           (lib "list.ss")
           (prefix srfi:43: (lib "43.ss" "srfi")))
  (require "maybe.ss")
  (provide (all-defined))  
  
  (define (member? obj lst)
    (not (not (member obj lst))))
  (define (memq? obj lst)
    (not (not (memq obj lst))))
  (define (memv? obj lst)
    (not (not (memv obj lst))))
  
  (define (exists? pred? lst)
    (let/ec esc
      (for-each (lambda (v)
                  (when (pred? v)
                    (esc #t)))
                lst)
      #f))
  
  (define (average lst)
    (if (empty? lst)
        0
        (apply /
               (foldl (lambda (e a)
                        (list (+ e (first a))
                              (+ 1 (second a))))
                      (list 0 0)
                      lst))))
  
  (define (replace alpha beta lst)
    (replace/op
     (lambda (e) (eq? alpha e))
     (lambda (e) beta)
     lst))
  (define (replace/op pred? new lst)
    (reverse
     (foldl (lambda (e a)
              (if (pred? e)
                  (cons (new e) a)
                  (cons e a)))
            empty
            lst)))
  
  ; elem-ref : alpha (listof alpha) -> (maybe number)
  (define (elem-ref e0 lst)
    (let/ec esc
      (foldl (lambda (e1 a)
               (when (eq? e0 e1)
                 (esc (make-just a)))
               (add1 a))
             0
             lst)
      (make-nothing)))
  
  (define (list-prefix lst i)
    (let/ec esc
      (foldl (lambda (e a)
               (if (eq? i (car a))
                   (esc (reverse (cdr a)))
                   (cons (add1 (car a))
                         (cons e (cdr a)))))
             (cons 0 empty)
             lst)))
  (define (list-head l x)
    (if (<= (length l) x)
        l
        (build-list x (lambda (i) (list-ref l i)))))
  
  (define (list-swap oi ni lst)
    (let ([nv (list->vector lst)])
      (srfi:43:vector-swap! nv oi ni)
      (vector->list nv)))
  
  ; first-in-list : (alpha -> boolean) (listof alpha) -> (+ alpha #f)
  (define (first-in-list pred? lst)
    (let loop ([lst lst])
      (if (empty? lst)
          #f
          (let ([c (car lst)])
            (if (pred? c)
                c
                (loop (rest lst)))))))
  
  ; list-splice : (listof a) number number -> (listof a)
  (define (list-splice as start end)
    (let loop ([i 0] [as as] [r (list)])
      (if (or (>= i end) (empty? as))
          (reverse r)
          (if (< i start)
              (loop (add1 i) (rest as) r)              
              (loop (add1 i) (rest as) (list* (first as) r))))))
  
  ; assoc/proj/cmp : (a -> b) (b b -> boolean) (list b) -> b -> (U #f a)
  (define ((assoc/proj/cmp proj cmp lst) obj)
    (if (empty? lst)
        #f
        (if (cmp obj (proj (first lst)))
            (first lst)
            ((assoc/proj/cmp proj cmp (rest lst)) obj))))
  
  ; <=/proj : (b* -> boolean) (a -> b) -> (a* -> boolean)
  (define (<=/proj <= proj)
    (lambda args
      (apply <= (map proj args))))
  
  ; cascade-<= : (a* -> boolean) (a* -> boolean) (a* -> boolean)
  (define (cascade-<= == <= next)
    (lambda args
      (if (apply == args)
          (apply next args)
          (apply <= args))))
  
  (define (list->unique-list l)
    (reverse
     (foldl (lambda (e a)
              (if (member e a)
                  a
                  (list* e a)))
            empty
            l)))
  
  (define (rest/empty l)
    (if (empty? l)
        empty
        (rest l)))
  
  (define (between e l)
    (foldr
     (lambda (x a)
       (cons x 
             (if (empty? a)
                 empty
                 (cons e a))))
     empty
     l))
  
  (define (all-prefixes l)
    (map reverse
         (let loop ([l (reverse l)])
           (cond
             [(empty? l) l]
             [else 
              (cons l (loop (cdr l)))]))))
  (define slices
    (opt-lambda (l k [fill? #f] [padding #f])
      (let loop ([l l] [r `()] [c `()] [i 0])
        (if (null? l)
            (append r 
                    (list (if fill?
                              (let loop ([l c] [i i])
                                (if (eq? i k)
                                    l
                                    (loop (append l (list padding))
                                          (+ i 1))))
                              c)))
            (let ([c-l (car l)])
              (if (eq? i k)
                  (loop (cdr l)
                        (append r (list c))
                        (list c-l)
                        1)
                  (loop (cdr l)
                        r
                        (append c (list c-l))
                        (+ i 1))))))))
  
  (define (telescoping-map f bs gs)
    (cond 
      [(empty? gs)
       (map (lambda (b) (apply f b))
            bs)]
      [(cons? gs)
       (let ([g (first gs)])
         (telescoping-map f 
                          (apply append
                                 (map (lambda (b)
                                        (map (lambda (n)
                                               (append b (list n)))
                                             (apply g b)))
                                      bs))
                          (rest gs)))]))
  
  
  (define (for-each/triple f l)
    (for-each 
     f
     (cons #f (reverse (cdr (reverse l))))
     l
     (append (cdr l) (list #f)))))