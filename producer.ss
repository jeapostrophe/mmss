(module producer mzscheme
  (require (lib "plt-match.ss"))
  (require "maybe.ss")
  
  ; generator-producer : ((alpha -> void) -> void) -> (producer alpha)
  (define (generate-producer body) 
    (define resume (box (make-nothing)))
    (lambda (real-send) 
      (define send-to (box real-send))
      (define (send value-to-send)
        (set-box! send-to 
                  (let/cc k 
                    (begin 
                      (set-box! resume (make-just k))
                      ((unbox send-to) value-to-send)))))
      (if (just? (unbox resume))
          ((just-value (unbox resume)) real-send)
          (body send))))
  
  ; yield-producer : (producer alpha) -> alpha
  (define (yield-producer i)
    (let/cc k (i k)))
  
  ; yield-producer* : (producer (maybe alpha)) -> (listof alpha)  
  (define (yield-producer* i)
    (let loop ([result (list)])
      (match (yield-producer i)
        [(struct nothing ())
         result]
        [(struct just (v))
         (loop (list* v result))]))))