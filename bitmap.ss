(module bitmap mzscheme
  (require (lib "list.ss"))
  (provide (all-defined))
  
  (define empty-bitmap 0)
  
  (define (bitmap-add a-bm k)
    (bitwise-ior a-bm (arithmetic-shift 1 k)))
  (define (bitmap-rem a-bm k)
    (bitwise-and a-bm (bitwise-not (arithmetic-shift 1 k))))
  (define (bitmap-on? a-bm k)
    (= 1 (arithmetic-shift (bitwise-and a-bm (arithmetic-shift 1 k)) (* -1 k))))
  
  (define (bitmap->list a-bm a-list)
    (let loop ([i (sub1 (length a-list))]
               [result empty])
      (if (< i 0)
          result          
          (loop (sub1 i) 
                (if (bitmap-on? a-bm (add1 i))
                    (list* (list-ref a-list i) result)
                    result))))))
              