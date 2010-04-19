(module permute mzscheme
  (require (lib "list.ss")
           (lib "etc.ss")
           (prefix is: (lib "integer-set.ss")))
  (provide random-list
           random-range)
  
  ; random-range : number? number? -> number?
  ; given a and b, returns a number in [a,b]
  (define (random-range low high)
    (+ low (random (add1 (- high low)))))
  
  ; random-list : (listof a) -> (listof a)
  ; returns a new list with the elements of the old in a random order
  (define (random-list lst)
    (let loop ([new empty]
               [is (is:make-range 0 (max 0 (sub1 (length lst))))])
      (if (equal? (is:card is) 0)
          new
          (let* ([wfs (is:integer-set-contents is)]
                 [a-rr (list-ref wfs (random (length wfs)))]
                 [next-point (random-range (car a-rr) (cdr a-rr))])
            (loop (list* (list-ref lst next-point) new)
                  (is:difference is (is:make-range next-point)))))))
  
  ; Below is code from Jacob Matthews
  (provide for-all for-all*)

  ; for-all : SYNTAX
  ; (for-all EXPR1 (ID EXPR2) ...)
  ; each expr2 must evaluate to a proper list (they need not have the same size as each other). 
  ; For each possible combination of items in the lists obtained by evaluating the EXPR2's in left-to-right
  ; order, evaluates expr1 with each ID bound to that item and returns a list consisting of all the
  ; results of those evaluations.
  (define-syntax (for-all stx)
    (syntax-case stx ()
      [(_ body (id arg) ...)
       (andmap identifier? (syntax-e #'(id ...)))
       #'(for-all* (lambda (id ...) body) arg ...)]))

  (define (for-all* f . xs) (map (lambda (x) (apply f x)) (all-selections xs)))
  
  ; all-selections : (listof (listof X)) -> (listof (listof X))
  ; returns all possible lists created by selecting an individual element from 
  ; each of the lists contained in the input argument
  ; E.G., (all-selections '((a b c) (1 2))) => '((a 1) (a 2) (b 1) (b 2) (c 1) (c 2))
  ;       (all-selections '((a b c) ()))    => '()
  ;       (all-selections '())              => '()
  (define (all-selections xs)
    (cond
      [(null? xs) '()]
      [(null? (cdr xs)) (map list (car xs))]
      [else
       (let ((rest (all-selections (cdr xs))))
         (apply append (map (lambda (item) (inject item rest)) (car xs))))]))
  
  ; inject : X (listof (listof X)) -> (listof (listof X))
  (define (inject item lists)
    (cond
      [(null? lists) '()]
      [else (cons (cons item (car lists)) (inject item (cdr lists)))])))
  