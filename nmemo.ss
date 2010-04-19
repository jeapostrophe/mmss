(module nmemo mzscheme
  (require (lib "list.ss"))
  (provide (all-defined))
  
  ; nmemo : (listof integer?) ((integer? ... -> alpha?) integer? ... -> alpha?) -> (integer? ... -> alpha?)
  (define (nmemo max* prim)
    (letrec ([matrix (make-vector (add1 (first max*)) #f)]
             [internal
              (lambda args
                (let loop ([pos 0]
                           [prev #f]
                           [curv matrix])
                  (if (not curv)
                      (if (< pos (length max*))
                          (begin (vector-set! prev (list-ref args (sub1 pos))
                                              (make-vector (add1 (list-ref max* pos)) #f))
                                 (apply internal args))
                          (let ([value (apply prim internal args)])
                            (vector-set! prev (list-ref args (sub1 pos)) value)
                            value))
                      (if (< pos (length max*))
                          (loop (add1 pos)
                                curv
                                (vector-ref curv (list-ref args pos)))
                          curv))))])
      internal)))
